#####################################
## fit model
##
## simon jackman
## october 2013
#####################################
suppressPackageStartupMessages(c(library('coda'),library('rjags')))
options(stringsAsFactors=FALSE)
args <- commandArgs(TRUE)
chart <- args[1]

## url to the pollster csv
url <- paste("http://elections.huffingtonpost.com/pollster/",chart,".csv",sep="")
data <- read.csv(file=url)

#############################
## data preperation for jags
#############################

## who are the candidates?
otherCols <- c("Pollster", "Entry.Date.Time..ET.", "Mode", "Start.Date", "Number.of.Observations", "Pollster.URL", "End.Date", "Population", "Source.URL", "Partisan", "Affiliation")
others <- c("Other","Undecided","Not Voting","Not.Voting","Refused","Wouldn't Vote","Wouldn.t.Vote","None")
theCandidates <- setdiff(setdiff(colnames(data), others), otherCols)

## contrasts we want
theContrasts <- list(theCandidates[1:2])

## what we will loop over, below
theResponses <- as.vector(c(theCandidates,others),mode="list")
theResponses <- c(theResponses,theContrasts)

## dates
today <- as.Date(Sys.time(),tz="America/Washington_DC")
data$startDate <- as.Date(data$Start.Date)
data$endDate <- as.Date(data$End.Date)
dateSeq <- seq.Date(from=min(data$startDate),
                    to=today,
                    by="day")
data$fieldPeriod <- as.numeric(data$endDate)-as.numeric(data$startDate)
#data <- subset(data, fieldPeriod>=0)
if(any(data$fieldPeriod<0)){
    stop("found mangled start and end dates")
}
data$fieldPeriod <- data$fieldPeriod + 1
NDAYS <- length(dateSeq)

## missing sample sizes?
nobs <- data$Number.of.Observations
nobs.bad <- is.na(nobs) | is.nan(nobs) | nobs<=0
if(any(nobs.bad)){
    cat(paste("mean imputing for",
              sum(nobs.bad),
              "bad/missing sample sizes\n"))
    nobs.bar <- tapply(nobs,data$Pollster,mean,na.rm=TRUE)
    nobs.bar[is.na(nobs.bar)] <- mean(nobs,na.rm=TRUE)

    nobs[nobs.bad] <- nobs.bar[match(data$Pollster[nobs.bad],names(nobs.bar))]
}
data$nobs <- nobs
rm(nobs)

## pollsters and pops
data$pp <- paste(data$Pollster,data$Population,sep=":")
thePollsters <- sort(unique(data$pp))

if (file.exists("/var/www/html/pollster")) {
  dataDir <- paste("/var/www/html/pollster/shared/models/",chart,sep="")
  M <- 100E3                ## number of MCMC iterates
  keep <- if (NDAYS > 600) 1E3 else 5E3
} else {
  dataDir <- paste("data/",chart,sep="")
  M <- 1E3                  ## number of MCMC iterates
  keep <- 1E3               ## how many to keep
}
dir.create(dataDir, showWarnings=FALSE, recursive=TRUE)

thin <- M/keep            ## thinning interval

## object for jags
makeJagsObject <- function(who,
                           offset=0){
    tmpData <- data
    theColumn <- match(who,names(tmpData))
    if(any(is.na(theColumn))){
        cat(paste("couldn't find",
                  who,
                  "in data, returning NULL\n"))
        return(NULL)
    }

    y.tmp <- tmpData[,theColumn]                     ## the response
    y.tmp <- matrix(y.tmp,ncol=length(theColumn))    ## be a matrix
    ok <- apply(y.tmp,1,function(x)!(any(is.na(x)))) ## clobber NA
    tmpData <- tmpData[ok,]                          ## subset to obs with good data
    y <- as.matrix(tmpData[,theColumn])
    if(dim(y)[2]==2){
      ## we have a contrast!
      a <- y[,1]/100
      b <- y[,2]/100
      y <- a - b
      va <- a*(1-a)
      vb <- b*(1-b)
      cov <- -a*b
      v <- (va + vb - 2*cov)/tmpData$nobs
    } else {
      y <- y/100
      v <- y*(1-y)/tmpData$nobs          ## variance
    }
    prec <- 1/v
    ## pollster/population combinations
    j <- match(tmpData$pp,thePollsters)

    ## loop over polls
    NPOLLS <- dim(tmpData)[1]
    counter <- 1
    pollList <- list()
    for(i in 1:NPOLLS){
        pollLength <- tmpData$fieldPeriod[i]
        ##cat(paste("pollLength:",pollLength,"\n"))
        dateSeq.limits <- match(c(tmpData$startDate[i],tmpData$endDate[i]),dateSeq)
        dateSeq.local <- dateSeq.limits[1]:dateSeq.limits[2]
        ##cat("dateSeq.local:\n")
        ##print(dateSeq.local)
        pollList[[i]] <- data.frame(y=rep(y[i],pollLength),
                                    j=rep(j[i],pollLength),
                                    prec=rep(prec[i]/pollLength,pollLength),
                                    date=dateSeq.local)
    }
    pollList <- do.call("rbind",pollList)

    forJags <- as.list(pollList)
    forJags$NOBS <- dim(pollList)[1]
    forJags$NHOUSES <- length(thePollsters)
    forJags$NPERIODS <- length(dateSeq)

    ## renormalize dates relative to what we have for this candidate
    firstDay <- match(min(tmpData$startDate),dateSeq)
    forJags$date <- forJags$date - firstDay + 1
    forJags$NPERIODS <- forJags$NPERIODS - firstDay + 1

    ## prior for house effects
    forJags$d0 <- rep(0,forJags$NHOUSES)
    delta.prior.sd <- .025
    delta.prior.prec <- (1/delta.prior.sd)^2
    forJags$D0 <- diag(rep(delta.prior.prec,forJags$NHOUSES))

    ## offset, user-defined?
    forJags$offset <- offset

    return(list(forJags=forJags,firstDay=firstDay))
}

makeInits <- function(){
    sigma <- runif(n=1,0,.01)
    xi <- rep(NA,forJags$NPERIODS)
    xi[1] <- runif(n=1)
    for(i in 2:forJags$NPERIODS){
        xi[i] <- rnorm(n=1,xi[i-1],sd=sigma)
    }
    xi.bad <- xi < .01
    xi[xi.bad] <- .01
    xi.bad <- xi > .99
    xi[xi.bad] <- .99

    ## house effect delta inits
    delta <- rnorm(n=forJags$NHOUSES,mean=0,sd=.02)
    out <- list(xi.raw=xi,sigma=sigma,delta.raw=delta)
    return(out)
}

makeInitsContrasts <- function(){
  sigma <- runif(n=1,0,.01)
  xi <- rep(0,forJags$NPERIODS)

  ## house effect delta inits
  delta <- rnorm(n=forJags$NHOUSES,mean=0,sd=.02)
  out <- list(xi.raw=xi,sigma=sigma,delta.raw=delta)
  return(out)
}

#######################################
## loop over the responses to be modelled
for(who in theResponses){
  who <- unlist(who)
  print(length(who))
    cat("\n")
    cat(paste("running for outcome",
              paste(who,collapse=" minus "),
              "\n"))

    library(rjags)
    tmp <- makeJagsObject(who,offset=0)
    forJags <- tmp$forJags
    firstDay <- tmp$firstDay

    if(is.null(forJags)){
        next
    }

  initFunc <- makeInits
  if(length(who)==2){
    initFunc <- makeInitsContrasts
  }
    ## call JAGS
    foo <- jags.model(file="singleTarget.bug",
                      data=forJags,
                      n.chains=4,
                      inits=initFunc)
    update(foo,M/5)

    out <- coda.samples(foo,
                        variable.names=c("xi","delta","sigma","dbar"),
                        n.iter=M,thin=thin)

    ## save output
    fname <- paste(dataDir,'/',gsub(paste(who,collapse=""),pattern=" ",replacement=""),
                   ".jags.RData",sep="")
    save("data","dateSeq","firstDay",
         "forJags","out",
         file=fname)
}

source("postJags.R")
