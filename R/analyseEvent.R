#' Plot event history
#' 
#' @param course event name
#' @param events Integer vector specifying events to analyse
#' @param runsToQualify How many events ought an athlete to have 
#' attended in order to contribute to the analysis?  Lower numbers lead to more accurate, 
#' but slower, analyses.
#' 
#' @author Martin R. Smith
#' @export
AnalyseEvent <- function (course, events, runsToQualify = length(events) / 4) {
  
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
  #repeatResults <- results[!is.na(results$athleteNumber) & results$athleteNumber %in% repeatRunners, ]
  athleteRuns <- table(results$athleteNumber)
  repeatRunners <- names(athleteRuns [athleteRuns > max(2L, runsToQualify)])
  repeatResults <- results[results$athleteNumber %in% repeatRunners, ]
  
  outliers <- as.character(unlist(by(repeatResults, repeatResults$athleteNumber, function(x) {
    athleteSpeeds <- 5000 / x$timeInSeconds
    normalRange <- median(athleteSpeeds) + (sd(athleteSpeeds) * c(-4, +4))
    rownames(x)[athleteSpeeds < normalRange[1] | athleteSpeeds > normalRange[2]]
  })))
  repeatResults <- repeatResults[!rownames(repeatResults) %in% outliers, ]
  
  
  
  with(repeatResults, {
    # The passage of time makes no significant contribution
    model <- lm(I(5000 / timeInSeconds) ~ athleteNumber + runSeqNumber)
    coefs <- summary(model)$coefficients
    plot(eventDates, coefs[1, 1] + c(0, coefs[paste0('runSeqNumber', events[-1]), 'Estimate']), 
         pch='.', xlab="Event date", ylab="Loyal athlete speed", axes=F, col='white')
    xTicks <- seq(min(eventDates), max(eventDates), length.out=7)
    axis(1, at=xTicks, labels=format(xTicks, '%Y-%m-%d'), las=2, cex=0.7)
    timeAdjustment <- seq(-90, 90, by=15)
    axis(2)
    axis(4, at=(5000 / ((typicalTime * 60) + timeAdjustment)) - (5000 / (typicalTime * 60)), labels=timeAdjustment)
    
    eventLoadings <- c(0, coefs[paste0('runSeqNumber', events[-1]), 'Estimate'])
    eventErrors <- c(0, coefs[paste0('runSeqNumber', events[-1]), 'Std. Error'])
    intercept <- coefs[1, 1]
    monthCols <- viridis(12)
    monthSemiTrans <- viridis(12, alpha=0.3)
    
    xx <- lapply(seq_along(eventLoadings), function(i) {
      lines(rep(eventDates[i], 2), intercept + eventLoadings[i] + c(1, -1) * eventErrors[i],
            col = monthSemiTrans[as.integer(format(as.Date(eventDates[i], origin='1970-01-01'), '%m'))])
    })
    abline(h=intercept + mean(eventLoadings), col='#00000044')
    text(eventDates, intercept + eventLoadings,
         col=monthCols[as.integer(format(as.Date(eventDates, origin='1970-01-01'), '%m'))],
         label=events,
         cex=0.7)
    # athlete1Runs <- results[results$athleteNumber == athlete1, ]
    # athlete2Runs <- results[results$athleteNumber == athlete2, ]
    # points(athlete1Runs$date, 5000/athlete1Runs$timeInSeconds, col=2, pch=2)
    # points(athlete2Runs$date, 5000/athlete2Runs$timeInSeconds, col=4, pch=4)
    # points(eventDates)
  })
}