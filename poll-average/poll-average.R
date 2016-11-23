options(warn=2, showWarnCalls=TRUE)

suppressPackageStartupMessages(library('rjags'))
library(httr)
suppressPackageStartupMessages(library(jsonlite))

McmcParams <- list(
  fast=list(
    n_iterations=1000,
    n_samples=1000,
    n_chains=4,
    n_pre_monitor_iterations=1000
  ),
  slow=list(
    n_iterations=100000,
    n_samples=5000,
    n_chains=4,
    n_pre_monitor_iterations=20000
  )
)

MakeJagsObjectForAverage <- function(pollPoints, endDate) {
  # Creates data for rjags.
  #
  # pollPoints is a data.frame with
  # start_date,end_date,pollster_methodology,value,variance
  # (pollster_methodology is a factor/string like "PPP:Adults")
  #
  # Output: list(jagsInput, indexToMethodology) where indexToMethodology is
  #         useful for decoding JAGS _output_. (since JAGS _input_ uses indexes
  #         to represent pollster+methodology combinations.

  methodologies = sort(unique(pollPoints$pollster_methodology))

  pollPoints$n_days <- as.integer(pollPoints$end_date - pollPoints$start_date + 1)
  pollPoints$methodology_index <- match(pollPoints$pollster_methodology, methodologies)

  delta.prior.sd <- .025
  delta.prior.prec <- (1 / delta.prior.sd) ^ 2

  startDate <- min(pollPoints$start_date)

  return(list(
    jagsInput=list(
      y=rep(pollPoints$value, pollPoints$n_days),
      j=rep(pollPoints$methodology_index, pollPoints$n_days),
      prec=rep(1 / pollPoints$variance / pollPoints$n_days, pollPoints$n_days),
      date=rep(pollPoints$start_date - startDate, pollPoints$n_days) + sequence(pollPoints$n_days),
      NOBS=sum(pollPoints$n_days),
      NHOUSES=length(methodologies),
      NPERIODS=as.integer(endDate - startDate + 1),
      d0=rep(0, length(methodologies)),
      D0=diag(delta.prior.prec, length(methodologies))
    ),
    indexToMethodology=methodologies,    # Convert j to pollster_methodology
    firstDate=min(pollPoints$start_date) # Convert period to date
  ))
}

MakeJagsChainInitsBuilder <- function(nDates, nMethodologies, isContrast) {
  # Creates a function to return initial values for a single JAGS "chain".
  #
  # In concept, a "chain" is one random population, i.e. a curve with date as
  # x and value as y. It walks randomly and is always between 0 (0%) and 1
  # (100%).
  return(function() {
    sigma <- runif(n=1, 0, .003)

    if (isContrast) {
      xi <- rep(0, nDates)
    } else {
      # Walk a random curve
      xi <- rep(NA, nDates)
      xi[1] <- runif(n=1)
      for (i in 2:nDates) {
        xi[i] <- rnorm(n=1, xi[i-1], sd=sigma)
      }
      # Clamp between 0 and 1, exclusive
      xi <- pmin(0.99999, pmax(0.00001, xi))
    }

    # Create random house effects
    delta <- rnorm(n=nMethodologies, mean=0, sd=.02)

    return(list(xi.raw=xi, sigma=sigma, delta.raw=delta))
  })
}

FractionToRoundedPercent <- function(fraction) {
  return(round(100 * fraction, digits=4))
}

JagsOutputToDateEstimates <- function(jagsOutput, jagsObject, clampAtZero) {
  # Turns the mcmc.list from a JAGS run into a data.frame
  #
  # Output columns: date,xibar,lo,up,prob. xibar,lo,up are out of 100.
  #
  # "prob" is a number from 1 ("xibar > 0, always") to 0 ("xibar < 0, always").
  # It's only necessary for our "minus" runs; otherwise it's 1.

  # 3D array: [iteration, date integer, chain number]
  # jagsOutput format: each iteration is xi1, xi2, xi3, ..., xiN, delta1, ..., deltaN
  iter_date_chain <- as.array(jagsOutput)[, 1:jagsObject$jagsInput$NPERIODS, ]
  iter_date_chain <- pmin(pmax(iter_date_chain, ifelse(clampAtZero, 0.0, -1.0)), 1.0)

  # Transpose so outer dims are date
  # [iteration, chain number, date integer]
  values <- aperm(iter_date_chain, c(1, 3, 2))

  # Cast as a 2D array: we don't need a difference between iter and chain
  dim(values) <- c(dim(values)[1] * dim(values)[2], dim(values)[3])
  dimnames(values) <- list(iteration=NULL, date=NULL)

  return(data.frame(
    date=seq.Date(from=jagsObject$firstDate, by='day', length.out=jagsObject$jagsInput$NPERIODS),
    xibar=FractionToRoundedPercent(apply(values, 'date', mean)),
    lo=FractionToRoundedPercent(apply(values, 'date', function(row) quantile(row, 0.025))),
    up=FractionToRoundedPercent(apply(values, 'date', function(row) quantile(row, 0.975))),
    prob=FractionToRoundedPercent(apply(values, 'date', function(row) mean(row >= 0)))
  ))
}

JagsOutputToHouseEffects <- function(jagsOutput, jagsObject) {
  # Turns the mcmc.list from a JAGS run into a data.frame
  #
  # Output columns: pollster,est,lo,hi,dev

  # 3D array: [iteration, methodology index, chain number]
  # jagsOutput format: each iteration is xi1, xi2, xi3, ..., xiN, delta1, ..., deltaN
  iter_date_chain <- as.array(jagsOutput)[, (jagsObject$jagsInput$NPERIODS + 1):(jagsObject$jagsInput$NPERIODS + jagsObject$jagsInput$NHOUSES), ]

  # Transpose so outer dims are date
  # [iteration, chain number, date integer]
  values <- aperm(iter_date_chain, c(1, 3, 2))

  # Cast as a 2D array: we don't need a difference between iter and chain
  dim(values) <- c(dim(values)[1] * dim(values)[2], dim(values)[3])
  dimnames(values) <- list(
    iteration=NULL,
    methodology=jagsObject$indexToMethodology
  )

  return(data.frame(
    pollster=jagsObject$indexToMethodology,
    est=FractionToRoundedPercent(apply(values, 'methodology', mean)),
    lo=FractionToRoundedPercent(apply(values, 'methodology', function(row) quantile(row, 0.025))),
    hi=FractionToRoundedPercent(apply(values, 'methodology', function(row) quantile(row, 0.975))),
    dev=FractionToRoundedPercent(apply(values, 'methodology', sd)),
    row.names=NULL
  ))
}

CalculateAverageByDate <- function(pollPoints, isContrast, endDate) {
  # Uses an MCMC model to calculate poll averages per date.
  #
  # Args:
  #  pollPoints: data.frame: start_date,end_date,pollster_methodology,value,variance
  #              (pollster_methodology looks like "PPP:Adults")
  #              (value is between 0 and 1)
  #  isContrast: if true, values are spreads between candidates (-1..1). If
  #              false, values are poll results for a single candidate (0..1).
  #  endDate: last date to model (the start date is the first value date)
  #
  # Return value: a list() with:
  #  dateEstimates: data.frame with date,xibar,lo,up
  #  houseEffects: data.frame with pollster,est,lo,hi,dev
  mcmcParams <- McmcParams[[speed]]

  jagsObject <- MakeJagsObjectForAverage(pollPoints, endDate)

  jagsModel <- jags.model(
    file="./singleTarget.bug",
    data=jagsObject$jagsInput,
    n.chains=mcmcParams$n_chains,
    inits=MakeJagsChainInitsBuilder(
      jagsObject$jagsInput$NPERIODS,
      jagsObject$jagsInput$NHOUSES,
      isContrast
    ),
    quiet=TRUE
  )
  update(jagsModel, mcmcParams$n_pre_monitor_iterations)
  jagsOutput <- coda.samples(
    jagsModel,
    variable.names=c("xi", "delta"),
    n.iter=mcmcParams$n_iterations,
    thin=mcmcParams$n_iterations / mcmcParams$n_samples
  )

  return(list(
    dateEstimates=JagsOutputToDateEstimates(jagsOutput, jagsObject, !isContrast),
    houseEffects=JagsOutputToHouseEffects(jagsOutput, jagsObject)
  ))
}

FindCsvLabels <- function(frame) {
  colNames <- colnames(frame)
  lastIndex <- match('poll_id', colNames) - 1
  return(colNames[1:lastIndex])
}

StopIfCsvHasBadDates <- function(csv) {
  if (any(csv$end_date < csv$start_date)) {
    stop("found mangled start and end dates")
  }
}

FindCsvNumObservationsForVarianceCalculations <- function(csv) {
  nobs <- csv$sample_size

  ok <- !is.na(nobs) & nobs > 0
  if (any(!ok)) {
    cat(paste0("Fabricating " + sum(!ok) + " sample sizes because data is missing them\n"))

    nobsByPollster <- tapply(nobs, csv$pollster, mean, na.rm=TRUE)
    nobsByPollster[is.na(nobsByPollster)] <- mean(nobs, na.rm=TRUE)
    nobs[!ok] <- nobsByPollster[match(csv$pollster[!ok], names(nobsByPollster))]
  }

  # Impose a fake limit. This is only useful for calculating variance. The
  # theory is: a poll with 10,000 observations is probably no more accurate than
  # a poll with 5,000 observations (because other methodology considerations are
  # more of a factor than number of observations).
  nobsTruncated <- pmin(3000, nobs)

  return(nobsTruncated)
}

AnalyzePollsterChart <- function(baseUrl, slug, speed) {
  # Downloads CSV and JSON data from Pollster and builds output data.frames:
  #
  # dateEstimates: who,date,xibar,lo,up,prob
  # houseEffects: who,pollster,est,lo,hi,dev

  basenameUrl <- paste0(baseUrl, "/api/charts/", chart)
  csv <- read.csv(
    file=url(paste0(basenameUrl, ".csv")),
    colClasses=c("start_date"="Date", "end_date"="Date"),
    check.names=FALSE
  )

  StopIfCsvHasBadDates(csv)
  effectiveNobs <- FindCsvNumObservationsForVarianceCalculations(csv)

  json <- fromJSON(url(paste0(basenameUrl, ".json")))

  electionDate <- as.Date(json$election_date)
  todayDate <- Sys.Date()
  endDate <- min(electionDate, todayDate)

  labels <- FindCsvLabels(csv)

  calculateAveragesForLabel <- function(label) {
    cat(paste0("Running for outcome ", label))

    y <- csv[[label]] / 100
    variance <- pmax(0.00001, y * (1 - y) / effectiveNobs)

    pollPoints <- data.frame(
      start_date=csv$start_date,
      end_date=csv$end_date,
      pollster_methodology=paste0(csv$pollster, ':', csv$sample_subpopulation),
      value=y,
      variance=variance
    )
    pollPoints <- pollPoints[!is.na(pollPoints$value),]

    output <- CalculateAverageByDate(pollPoints, FALSE, endDate)
    dateEstimates <- output$dateEstimates
    dateEstimates$prob <- NA # probability only applies to contrasts
    houseEffects <- output$houseEffects

    cat("\n")

    return(list(
      dateEstimates=data.frame(who=rep(label, nrow(dateEstimates)), dateEstimates),
      houseEffects=data.frame(who=rep(label, nrow(houseEffects)), houseEffects)
    ))
  }

  calculateAveragesForContrast <- function(label1, label2) {
    label <- paste0(label1, ' minus ', label2)
    cat(sprintf("Running for outcome %s\n", label))

    a <- csv[[label1]] / 100
    b <- csv[[label2]] / 100
    y <- a - b
    va <- a * (1 - a)
    vb <- b * (1 - b)
    cov <- -1 * a * b
    variance <- pmax(0.00001, (va + vb - 2 * cov) / effectiveNobs)

    pollPoints <- data.frame(
      start_date=csv$start_date,
      end_date=csv$end_date,
      pollster_methodology=paste0(csv$pollster, ':', csv$sample_subpopulation),
      value=y,
      variance=variance
    )
    pollPoints <- pollPoints[!is.na(pollPoints$value),]

    output <- CalculateAverageByDate(pollPoints, TRUE, endDate)
    dateEstimates <- output$dateEstimates
    houseEffects <- output$houseEffects

    cat("\n")

    return(list(
      dateEstimates=data.frame(who=rep(label, nrow(dateEstimates)), dateEstimates),
      houseEffects=data.frame(who=rep(label, nrow(houseEffects)), houseEffects)
    ))
  }

  labelAverages <- lapply(labels, FUN=calculateAveragesForLabel)
  contrastAverages <- calculateAveragesForContrast(labels[1], labels[2])

  dateEstimatesList <- lapply(labelAverages, function(x) x$dateEstimates)
  houseEffectsList <- lapply(labelAverages, function(x) x$houseEffects)

  return(list(
    dateEstimates=rbind(do.call(rbind, dateEstimatesList), contrastAverages$dateEstimates),
    houseEffects=rbind(do.call(rbind, houseEffectsList), contrastAverages$houseEffects)
  ))
}

FrameToCsvBytes <- function(frame) {
  # Global options, yay R...
  options(scipen=999)

  tempfile <- file()
  write.csv(frame, file=tempfile, na="", row.names=FALSE)
  bytes <- paste(readLines(tempfile), collapse='\r\n')
  close(tempfile)
  return(bytes)
}

PostCsv <- function(csvBytes, url) {
  r <- httr::POST(url, body=list(csv=csvBytes))
  httr::stop_for_status(r)
}

args <- commandArgs(TRUE)
base_url <- args[1]
chart <- args[2]
speed <- ifelse(length(args) >= 3 && args[3] == 'fast', 'fast', 'slow')

results <- AnalyzePollsterChart(base_url, chart, speed)

#PostCsv(FrameToCsvBytes(results$dateEstimates), paste0(base_url, '/api/charts/', chart, '/model-output.csv'))
#PostCsv(FrameToCsvBytes(results$houseEffects), paste0(base_url, '/api/charts/', chart, '/house-effects.csv'))
