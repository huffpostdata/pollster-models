#####################################
## fit model
##
## simon jackman - october 2013
## natalie jackson - july 2014
#####################################
suppressPackageStartupMessages(c(library('coda'),library('rjags')))
options(stringsAsFactors=FALSE)
library(truncnorm)
library(rjson)
library(rjags)

set.seed(.5)
args <- commandArgs(TRUE)
chart <- args[1]

## url to the pollster csv
url <- paste("http://elections.huffingtonpost.com/pollster/",chart,".csv",sep="")
data <- read.csv(file=url)
chartObj <- fromJSON(file=paste("http://elections.huffingtonpost.com/pollster/api/charts/",chart,".json",sep=""), method='C')

#############################
## data preparation for jags
#############################

## who are the candidates?
otherCols <- c("Pollster", "Entry.Date.Time..ET.", "Mode", "Start.Date", "Number.of.Observations", "Pollster.URL", "End.Date", "Population", "Source.URL", "Partisan", "Affiliation")
others <- c("Other","Undecided","Not Voting","Not.Voting","Refused","Wouldn't Vote","Wouldn.t.Vote","None")
theCandidates <- setdiff(setdiff(colnames(data), others), otherCols)
allChoices <- setdiff(colnames(data), otherCols)

## contrasts we want
theContrasts <- list(theCandidates[1:2])

## what we will loop over, below
theResponses <- as.vector(c(theCandidates,others),mode="list")
theResponses <- c(theResponses,theContrasts)

## dates
today <- as.Date(Sys.time(),tz="America/New_York")
eday <- toString(chartObj['election_date'])
electionday <- as.Date(ifelse(eday == "NULL", today, eday))
data$startDate <- as.Date(data$Start.Date)
data$endDate <- as.Date(data$End.Date)
dateSeq <- seq.Date(from=min(data$startDate),
                    to=electionday,
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

# change zeroes to NA
for (choice in allChoices) {
  data[[choice]] <- ifelse(data[[choice]]==0, NA, data[[choice]])
}

## pollsters and pops
data$pp <- paste(data$Pollster,data$Population,sep=":")
thePollsters <- sort(unique(data$pp))
thePollstersA <- as.list(sort(data$pp))

#Classifying single partisans into groups
dup1 <- duplicated(data$pp, fromLast=FALSE)
dup2 <- duplicated(data$pp, fromLast=TRUE)
data$duplicate <- ifelse(dup1=="FALSE" & dup2=="FALSE", "Single poll", "2 or more")
data$pp2 <- ifelse(data$duplicate=="2 or more", data$pp, 0)
data$pp2 <- ifelse(data$Affiliation=="Rep" & data$pp2==0, "Single Republican Polls", data$pp2)
data$pp2 <- ifelse(data$Affiliation=="Dem" & data$pp2==0, "Single Democrat Polls", data$pp2)
data$pp2 <- ifelse(data$Affiliation=="Other" & data$pp2==0, "Single Other Polls", data$pp2)
data$pp2 <- ifelse(data$Affiliation=="None" & data$pp2==0, data$pp, data$pp2)
thePollstersGrp <- sort(unique(data$pp2))
npolls <- length(data$pp2)

#partisanship indicators
data$pprep <- ifelse(data$Affiliation == "Rep", 1, 0)
data$ppdem <- ifelse(data$Affiliation == "Dem", 1, 0)
data$ppoth <- ifelse(data$Affiliation == "None", 1, 0)

#Read in and match pollsters to exclusion list
exclude <- read.csv("exclusionlist.csv")

#Good defined by partisanship and exclusion list
match <- match(data$Pollster,exclude$pollster, nomatch = 0)
data$goodlist <- ifelse(match==0 & data$Affiliation=="None" & data$Population != "Adults", 1, 0)

##subset of polls in each group
theGoodPolls <- ifelse(data$goodlist == 1, data$pp, NA)
theGoodPollslist <- sort(unique(theGoodPolls))

##code to take out RV duplicates
splitPP <- do.call(rbind, strsplit(theGoodPollslist, ':'))
splitPP <- as.data.frame(splitPP)
splitPP$dup3 <- duplicated(splitPP$V1, fromLast=FALSE)
splitPP$V1 <- ifelse(splitPP$dup3=="TRUE", NA, splitPP$V1)
splitPP$V2 <- ifelse(splitPP$dup3=="TRUE", NA, splitPP$V2)
Goodlistnodups <- paste(splitPP$V1,splitPP$V2,sep=":")
match <- match(theGoodPollslist,Goodlistnodups, nomatch = 0)
theGoodPolls <- ifelse(match==0, NA, Goodlistnodups)
theGoodPollslist <- sort(unique(theGoodPolls))

Goodfreqs <- as.numeric(table(theGoodPolls))
theRepPolls <- ifelse(data$pprep ==1, data$pp, NA)
Repfreqs <- as.numeric(table(theRepPolls))
theRepPollslist <- sort(unique(theRepPolls))
theDemPolls <- ifelse(data$ppdem ==1, data$pp, NA)
theDemPollslist <- sort(unique(theDemPolls))
Demfreqs <- as.numeric(table(theDemPolls))
theOthPolls <- ifelse(data$ppoth == 1, data$pp, NA)
theOthPollslist <- sort(unique(theOthPolls))
Othfreqs <- as.numeric(table(theOthPolls))

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
    j <- match(tmpData$pp2,thePollstersGrp)

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
    forJags$NHOUSES <- length(thePollstersGrp)
    forJags$NPERIODS <- length(dateSeq)

    ## renormalize dates relative to what we have for this candidate
    firstDay <- match(min(tmpData$startDate),dateSeq)
    forJags$date <- forJags$date - firstDay + 1
    forJags$NPERIODS <- forJags$NPERIODS - firstDay + 1

    ## prior for house effects
    forJags$d0 <- rep(0,forJags$NHOUSES)
    delta.prior.sd <- .03
    delta.prior.prec <- (1/delta.prior.sd)^2
    forJags$D0 <- diag(rep(delta.prior.prec,forJags$NHOUSES))

    return(list(forJags=forJags,firstDay=firstDay))
}

makeInits <- function(){
    sigma <- runif(n=1,0,.01)
    xi <- rep(NA,forJags$NPERIODS)
    xi[1] <- rtruncnorm(n=1,0,1,.45,.1)
    for(i in 2:forJags$NPERIODS){
        xi[i] <- rtruncnorm(n=1,0,1,xi[i-1],sd=sigma)
    }

    ## house effect delta inits
    delta <- rnorm(n=forJags$NHOUSES,mean=0,sd=.02)
    out <- list(xi.raw=xi,sigma=sigma,delta.raw=delta)
    return(out)
}

makeInitsContrasts <- function(){
  sigma <- runif(n=1,0,.01)
  xi <- rep(0,forJags$NPERIODS)
  xi[1] <- rtruncnorm(n=1,0,1,.45,.1)
    for(i in 2:forJags$NPERIODS){
      xi[i] <- rtruncnorm(n=1,0,1,xi[i-1],sd=sigma)
    }

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
