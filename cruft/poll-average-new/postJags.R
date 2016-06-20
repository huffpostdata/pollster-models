## post-estimation
options(stringsAsFactors=FALSE)
library(rjags)

postProcess <- function(fname){
    load(file=fname)

    out2 <- as.array(out)
    xi <- out2[,grep("^xi",dimnames(out2)[[2]]),]
    xibar <- apply(xi,2,
                   function(x){
                     c(mean(x),
                       quantile(x,c(.025,.975)),
                       sd(x))
                   })
    xibar <- t(xibar)
    theDateSeq <- as.character(dateSeq)[seq(from=firstDay,
                                            length=dim(xibar)[1],by=1)]
    dimnames(xi) <- list(NULL,theDateSeq,1:dim(xi)[3])
    dimnames(xibar) <- list(theDateSeq,
                            c("mean","2.5","97.5","sd"))

    ## rough plot
    plotData <- data.frame(y=forJags$y,
                           date=dateSeq[forJags$date+firstDay])
    par(lend=2)

    M <- 250
    s <- sample(size=M,x=1:nrow(xi))
    chain <- sample(size=M,x=1:dim(xi)[3],replace=TRUE)
    ylims <- range(c(as.vector(xi[s,,]),
                     forJags$y))

    ## delta processing to come here too
    delta <- out2[,grep("^delta",dimnames(out2)[[2]]),]
    dimnames(delta) <- list(NULL,thePollstersGrp,NULL)

    deltabar <- apply(delta,2,
                      function(x){
                        n <- length(x)
                        ok <- (1:n)>(n/2)
                        x <- x[ok]
                        c(mean(x),
                          quantile(x,c(.025,.975)),
                          sd(x),
                          mean(x>0))
                      })
    deltabar <- t(deltabar)
    colnames(deltabar) <- c("Estimate","2.5%","97.5%","StdDev","Pr[delta>0]")
    indx <- order(deltabar[,"Estimate"])
    deltabar <- deltabar[indx,]

    ## dbar processing
    dbar <- out2[,grep("^dbar",dimnames(out2)[[2]]),]
    dbarbar <- apply(dbar,2,
                      function(x){
                        n <- length(x)
                        ok <- (1:n)>(n/2)
                        x <- x[ok]
                        c(mean(x),
                          quantile(x,c(.025,.975)),
                          sd(x),
                          mean(x>0))
                      })
    dbarbar <- t(dbarbar)
    colnames(dbarbar) <- c("Estimate","2.5%","97.5%","StdDev","Pr[dbar>0]")

    return(list(xi=xi,xibar=xibar,delta=deltabar,dbar=dbarbar))
}

#######################################
## combine results for response options
combine <- function(tmp){
    nms <- names(tmp)
    noInclude <- grep("minus",nms)
    tmp[noInclude] <- NULL

    cat("combining/renormaling output for the following response options\n:")
    print(names(tmp))

    n <- length(tmp)
    nms <- names(tmp)
    xi <- lapply(tmp,function(x)x$xi)
    d <- dim(xi[[1]])
    niter <- d[1]
    nchains <- d[3]

    dateSpans <- lapply(xi,
                        function(x){
                            n <- ncol(x)
                            d <- as.Date(colnames(x))
                            return(d[c(1,n)])
                        })

    dateSpans <- do.call("rbind",dateSpans)
    dateRange <- c(dateSpans[which.min(as.Date(dateSpans[,1],origin="1970-01-01")),1],
                   dateSpans[which.max(as.Date(dateSpans[,2],origin="1970-01-01")),2])

    dateSeq <- seq.Date(from=as.Date(dateRange[1],origin="1970-01-01"),
                        to=as.Date(dateRange[2],origin="1970-01-01"),
                        by="day")

    ## create 4-d master array
    tmpArray <- array(NA,c(niter,length(dateSeq),nchains,n))
    dimnames(tmpArray) <- list(NULL,as.character(dateSeq),1:nchains,nms)
    for(j in 1:n){
        ## populate master array with output for each response option
        matchingDays <- match(colnames(xi[[j]]),as.character(dateSeq))
        tmpArray[,matchingDays,,j] <- xi[[j]]
    }

    ## renormalize (and check)
    tmpArray.renorm <- apply(tmpArray,c(1,2,3),
                             function(x)x/sum(x,na.rm=TRUE)*100)
    rm(tmpArray)   ## give back some memory
    tmpArray <- aperm(tmpArray.renorm,perm=c(2,3,4,1))
    save("tmpArray",file=paste(dataDir,"/tmpArray.RData",sep=""))
    ##all.sum <- apply(tmpArray,c(1,2,3),sum,na.rm=TRUE)

    ## average over iterations and chains
    xibar <- apply(tmpArray,c(2,4),mean,na.rm=TRUE)
    xibar[is.nan(xibar)] <- NA

    ## quantiles
    xiq <- apply(tmpArray,c(2,4),
                 quantile,
                 probs=c(.025,.975),
                 na.rm=TRUE)
    xiq <- aperm(xiq,c(2,3,1))
    xiq.out <- xiq[,1,]
    for(j in 2:n){
        xiq.out <- rbind(xiq.out,xiq[,j,])
    }
    nrecs <- dim(xiq.out)[1]

    ## stack by candidate
    xibar.out <- data.frame(who=rep(nms,each=length(dateSeq)),
                            date=rep(as.character(dateSeq),n),
                            xibar=as.vector(xibar),
                            lo=xiq.out[,1],
                            up=xiq.out[,2],
                            prob=rep(NA,nrecs))

    return(xibar.out)
}


#################################################
## difference function
diffSummary <- function(a,b){
  load(file=paste(dataDir,"/tmpArray.RData",sep=""))

  theOnes <- match(c(a,b),dimnames(tmpArray)[[4]])
  d <- list(tmpArray[,,,theOnes[1]],
            tmpArray[,,,theOnes[2]])
  theRows <- sort(unique(unlist(lapply(d,function(x)dimnames(x)[[2]]))))
  nDays <- length(theRows)
  nIter <- dim(d[[1]])[1]
  nChains <- dim(d[[1]])[3]
  z1 <- array(NA,c(nIter,nDays,nChains))
  z2 <- array(NA,c(nIter,nDays,nChains))
  z1[,match(dimnames(d[[1]])[[2]],theRows),] <- d[[1]]
  z2[,match(dimnames(d[[2]])[[2]],theRows),] <- d[[2]]
  z <- z1 - z2
  zbar <- apply(z,2,
                  function(x){
                      c(mean(x,na.rm=TRUE),
                        quantile(x,c(.025,.975),na.rm=TRUE),
                        mean(x>0,na.rm=TRUE))
                  })
  zbar <- t(zbar)
  nrecs <- dim(zbar)[1]
  return(data.frame(who=rep(paste(a,"minus",b),nrecs),
                    date=theRows,
                    xibar=zbar[,1],
                    lo=zbar[,2],
                    up=zbar[,3],
                    prob=zbar[,4]*100))
}

combineHouse <- function(tmp){
    n <- length(tmp)
    nms <- names(tmp)

    out <- data.frame(who=rep(nms,each=length(thePollstersGrp)),
                      pollster=rep(thePollstersGrp,n))

    for(i in 1:nrow(out)) {
      out[i,"est"] <- round(tmp[[out[i,"who"]]][["delta"]][out[i,"pollster"], "Estimate"], 4)
      out[i,"lo"] <- round(tmp[[out[i,"who"]]][["delta"]][out[i,"pollster"], "2.5%"], 4)
      out[i,"hi"] <- round(tmp[[out[i,"who"]]][["delta"]][out[i,"pollster"], "97.5%"], 4)
      out[i,"dev"] <- round(tmp[[out[i,"who"]]][["delta"]][out[i,"pollster"], "StdDev"], 4)
    }

    return(out)
}

#Adjusted house effects
combineHouse2 <- function(house){
	housecontrast$est <- housecontrast$est - goodmean
	out <- data.frame(housecontrast)
	return(out)
}

#########################################
## process jags output
tmp <- list()
for(who in theResponses){
  who <- unlist(who)
    cat("\n")
    cat(paste("post-processing for candidate",
              paste(who,collapse=" minus "),
                    "\n"))
    fname <- paste(dataDir,'/',paste(who,collapse=""),".jags.RData",sep="")
    if(file.exists(fname)){
        cat(paste("reading JAGS output and data from file",fname,"\n"))
        tmp[[paste(who,collapse=" minus ")]] <- postProcess(fname)
    }
    else {
        cat(paste("nothing to do for",who,"\n"))
    }
}

house <- combineHouse(tmp)
housecontrast <- house[min(grep("minus",house$who)):nrow(house),]

##Calculate avg of "good" polls deltas here##
goodmeanA <- match(housecontrast$pollster, theGoodPollslist)
goodmeanB <- ifelse(goodmeanA != "NA", housecontrast$est, NA) ##if no goodpolls in list, all will be NA and mean will be 0
goodmeanB <- na.omit(goodmeanB)
goodmeanC <- goodmeanB * Goodfreqs
goodmean <- (sum(goodmeanC)/sum(Goodfreqs))
goodmean[length(theGoodPollslist) < 2] <- goodmean/2
hlfmean <- goodmean/2

nm1 <- strsplit(housecontrast$who,split=" minus ")[[1]][[1]]
len1 <- length(tmp[[nm1]]$xi)
for (i in 1:len1) {
  tmp[[nm1]]$xi[i] <- tmp[[nm1]]$xi[i] + hlfmean
}

nm2 <- strsplit(housecontrast$who,split=" minus ")[[1]][[2]]
len2 <- length(tmp[[nm2]]$xi)
for (i in 1:len2) {
  tmp[[nm2]]$xi[i] <- tmp[[nm2]]$xi[i] - hlfmean
}

## combine response options
out <- combine(tmp)

## process contrasts
theContrasts <- grep("minus",names(tmp))
n.Contrasts <- length(theContrasts)
if(n.Contrasts>0){
  outContrasts <- list()
  for(j in 1:n.Contrasts){
    thisContrast.name <- names(tmp)[theContrasts[j]]
    nms <- strsplit(thisContrast.name,split=" minus ")[[1]]
    outContrasts[[j]] <- diffSummary(nms[1],nms[2])
  }
  outContrasts <- do.call("rbind",outContrasts)
  out <- rbind(out,outContrasts)
}

##########################################

write.csv(out,
          file=paste(dataDir,"/out.csv",sep=""))

write.csv(combineHouse2(house),
          file=paste(dataDir,"/house.csv",sep=""))

unlink(paste(dataDir,"/*.RData",sep=""))
unlink(paste(dataDir,"/*.pdf",sep=""))