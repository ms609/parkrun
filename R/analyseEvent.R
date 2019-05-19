#' Plot event history
#' 
#' @param course event name
#' @param events Integer vector specifying events to analyse
#' @param runsToQualify How many events ought an athlete to have 
#' attended in order to contribute to the analysis?  Lower numbers lead to more accurate, 
#' but slower, analyses.
#' @param forceWrite Logical specifying whether to overwrite saved properties of the race.
#' Automatically overwritten if min(events) and max(events) are greater than those last saved.
#' 
#' @author Martin R. Smith
#' @importFrom viridisLite viridis
#' @export
AnalyseEvent <- function (course, events, runsToQualify = length(events) / 4, forceWrite=FALSE) {
  
  eventHistory <- EventHistory(course)
  if (any(eventHistory$date == 1)) {
    # Fix write errors!
    history <- ScrapeEventHistory(course, write=FALSE)
    eventHistory$date <- history$date
    write.table(eventHistory, paste0(EventDirectory(course), '/index.txt'))
  }
  eventDates <- as.Date(eventHistory[as.character(events), 'date'], origin='1970-01-01')
  
  
  results <- suppressWarnings(dplyr::bind_rows(lapply(events, function (event) GetResults(course, event))))
  results <- results[!is.na(results$athleteNumber), ]
  results$date <- eventHistory[results$runSeqNumber, 'date']
  athleteRuns <- table(results$athleteNumber)
  repeatRunners <- names(athleteRuns [athleteRuns > max(2L, runsToQualify)])
  repeatResults <- results[results$athleteNumber %in% repeatRunners, ]
  
  outliers <- as.character(unlist(by(repeatResults, repeatResults$athleteNumber, function(x) {
    athleteSpeeds <- 5000 / x$timeInSeconds
    normalRange <- median(athleteSpeeds) + (sd(athleteSpeeds) * c(-4, +4))
    rownames(x)[athleteSpeeds < normalRange[1] | athleteSpeeds > normalRange[2]]
  })))
  repeatResults <- repeatResults[!rownames(repeatResults) %in% outliers, ]
  Season <- function (date) 2 * pi * as.integer(as.Date.factor(date) - as.Date('1999-01-01')) / 365.25
  
  with(repeatResults, {
    eventSeason <- Season(date)
    timeModel <- lm(I(5000 / timeInSeconds) ~ sin(eventSeason) + cos(eventSeason) + (athleteNumber * eventSeason), data=repeatResults)
    timeResid <- resid(timeModel)
    #plot(timeResid ~ runSeqNumber)
    
    residLm <- lm(timeResid ~ runSeqNumber)
    residCoefs <- summary(residLm)$coefficients
    runSeqs <- as.integer(sub('runSeqNumber', '', rownames(residCoefs)[-1], fixed=TRUE))
    runSeq0 <- events [!events %in% runSeqs]
    if (length(runSeq0) > 1) stop("Some events not found: ", runSeq0[-1])
    runSeqs <- c(runSeq0, runSeqs)
    
    residEsts <- residCoefs[, 'Estimate']
    residErr <- residCoefs[, 'Std. Error']
    runEsts <- c(runSeq0 = 0, residEsts[-1] + residEsts[1])
    runEsts <- runEsts - mean(runEsts[-1])
      
    runNotable <- abs(runEsts) > residErr
    
    coefs <- summary(timeModel)$coefficients
    eventSeasons <- Season(eventDates)
    eventSeasonality <- coefs['(Intercept)', 'Estimate'] +
      coefs['sin(eventSeason)', 'Estimate'] * sin(eventSeasons) +
      coefs['cos(eventSeason)', 'Estimate'] * cos(eventSeasons)
    eventResid <- runEsts[order(runSeqs)]
    eventSpeed <- eventSeasonality + eventResid
    plot(eventSpeed ~ eventDates,
         pch='.', xlab="Event date", ylab="Seasonal speed variation", axes=F, col='white')
    
    xTicks <- seq(min(eventDates), max(eventDates), length.out=7)
    axis(1, at=xTicks, labels=format(xTicks, '%Y-%m-%d'), las=2, cex=0.7)
    axis(2)
    axisSpeeds <- mean(eventSpeed) * seq(0.9, 1.1, length.out=19)
    axis(4, at=axisSpeeds, labels=SecondsToMinutes(5000 / axisSpeeds))
    mtext('Representative athlete time', side=4, line=0)
    mtext(paste0(course, ' parkrun'), side=3, line=0)
    
    eventErrors <- residErr[order(runSeqs)]
    monthCols <- viridis(12)
    monthSemiTrans <- viridis(12, alpha=0.3)
    
    xx <- lapply(seq_along(eventSpeed), function(i) {
      lines(rep(eventDates[i], 2), eventSpeed[i] + c(1, -1) * eventErrors[i],
            col = monthSemiTrans[as.integer(format(as.Date(eventDates[i], origin='1970-01-01'), '%m'))])
    })
    lines(eventSeasonality~eventDates)
    eventNotable <- runNotable[order(runSeqs)]
    notableDates <- eventDates[eventNotable]
    text(notableDates, eventSpeed[eventNotable],
         col=monthCols[as.integer(format(as.Date(notableDates, origin='1970-01-01'), '%m'))],
         label=events[eventNotable],
         cex=0.7)
  })
  
  # Write to file?
  
  lmFile <- paste0(EventDirectory(course), '/regression.txt')
  writeToFile <- TRUE
  if (!forceWrite && file.exists(lmFile)) {
    existingStats <- read.table(lmFile, row.names = 1)
    if (min(events) > existingStats['minEvent', 1] || 
      max(events) < existingStats['maxEvent', 1]) writeToFile <- FALSE
  }
  if (writeToFile) write.table(
    c(minEvent=min(events), maxEvent=max(events), 
      coefs['sin(eventSeason)', c('Estimate', 'Std. Error')],
      coefs['cos(eventSeason)', c('Estimate', 'Std. Error')]),
    row.names = c('minEvent', 'maxEvent', 'sinEst', 'sinErr', 'cosEst', 'cosErr'),
    col.names = FALSE,
    file = lmFile)
}
