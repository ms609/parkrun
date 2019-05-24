## TODO: Ignore first N events... check eventTrends to determine N!

#' Gather results from cache
#' @param courses Vector specifying names of courses to query
#' @return List of matrices of results
#' @author Martin R. Smith
CachedResults <- function (courses) {
  lapply(courses, function (course) {
    eventHistory <- EventHistory(course)
    eventResults <- list.files(EventDirectory(course), pattern="\\d+", full.names=TRUE)
    ret <- suppressWarnings(dplyr::bind_rows(lapply(eventResults, read.table)))
    ret$date <- as.Date(eventHistory[as.character(ret$runSeqNumber), 'date'],
                        origin='1970-01-01')
    ret
  })
}

#' Bind Results
#' @param results List of results from multiple courses, perhaps from CachedResults
#' @return Single matric of results, with original courses identified
#' 
#' @author Martin R. Smith
#' @importFrom dplyr bind_rows
#' @export
BindResults <- function (results) {
  results <- suppressWarnings(dplyr::bind_rows(results))
  results <- results[!is.na(results$athleteNumber), ]
  results$runSeqNumber <- paste0(results$eventName, results$runSeqNumber)
  results$eventName <- relevel(as.factor(results$eventName), ref=results[[1]][1])
  results
}

#' Compare two events
#' 
#' @param course1,course2 Names of courses to compare, per their parkrun URLs.
#' 
#' @return Vector of length two, specifying (1) an estimate of how much faster course2 is than course1; 
#' (2) the standard error of the estimate
#' 
#' @author Martin R. Smith
#' @export
CompareTwoEvents <- function (course1, course2) {
  
  courseResults <- CachedResults(c(course1, course2))
  results <- BindResults(courseResults)
  uniqueAthletes <- na.omit(unique(results$athleteNumber))
  athletesAtEvent <- vapply(courseResults, function (x) uniqueAthletes %in% x$athleteNumber, logical(length(uniqueAthletes)))
  tourists <- uniqueAthletes[rowSums(athletesAtEvent) > 1]
  
  usefulResults <- results[results$athleteNumber %in% tourists, ]
  
  outliers <- as.character(unlist(by(usefulResults, usefulResults$athleteNumber, function(x) {
    athleteSpeeds <- 5000 / x$timeInSeconds
    normalRange <- median(athleteSpeeds) + (sd(athleteSpeeds) * c(-4, +4))
    rownames(x)[athleteSpeeds < normalRange[1] | athleteSpeeds > normalRange[2]]
  })))
  usefulResults <- usefulResults[!rownames(usefulResults) %in% outliers, ]
  
  athleteResults <- table(usefulResults$athleteNumber)
  athleteEntries <- rev(table(athleteResults))
  
  tooManyAthletes <- 500
  minRuns <- names(athleteEntries[sum(cumsum(athleteEntries) < tooManyAthletes)])
  usefulAthletes <- rownames(athleteResults)[athleteResults >= as.integer(minRuns)]
  usefulResults <- usefulResults[usefulResults$athleteNumber %in% usefulAthletes, ]
  
  eventSeason <- EventSeason(usefulResults$date)
  #timeModel <- lm(I(5000 / timeInSeconds) ~ (eventName * (sin(eventSeason) + cos(eventSeason))) + (as.factor(athleteNumber) * eventSeason), data=usefulResults)
  timeModel <- lm(I(5000 / timeInSeconds) ~ eventName + sin(eventSeason) + cos(eventSeason) + (as.factor(athleteNumber) * eventSeason), data=usefulResults)
  # BIC prefers single sin/cos for all events
  
  coefs <- summary(timeModel)$coefficients
  
  # Return:
  coefs[2, 1:2]
}

#' Add pairing to course matrix
#' 
#' @export
AddToCourseMatrix <- function (course1, course2) {
  if (identical(course1, course2)) return (NA)
  
  difference <- CompareTwoEvents(course1, course2)
  resultsFile <- 'results/matrix.txt'
  errorsFile <- 'results/matrixErrors.txt'
  if (file.exists(resultsFile)) {
    mat <- read.table(resultsFile)
    err <- read.table(errorsFile)
  } else {
    courses <- sort(c(course1, course2))
    err <- mat <- matrix(nrow=2, ncol=2, dimnames = list(courses, courses))
  }
  
  mat[course1, course2] <- difference[1]
  mat[course2, course1] <- -difference[1]
  err[course1, course2] <- err[course2, course1] <- difference[2]
  
  write.table(mat, resultsFile)
  write.table(err, errorsFile)
}

AddCourseToMatrix <- function (course) {
  existing <- read.table('results/matrix.txt')
  lapply(rownames(existing), AddToCourseMatrix, course)
  invisible()
}

MatrixComparison <- function (course1, course2) {
  results <- read.table('results/matrix.txt')
  aToX <- results[course1, ]
  xToB <- results[course2, ]
  
  aToB <- aToX - xToB
  aToB[course1, course2] <- results[course1, course2]
  
  errors <- read.table('results/matrixErrors.txt')
  variance <- errors * errors
  aXVar <- variance[course1, ]
  xBVar <- variance[course2, ]
  
  aBVar <- aXVar + xBVar
  aBVar[course1, course2] <- errors[course1, course2]
  
  # https://stats.stackexchange.com/questions/265626/combining-the-result-of-two-uncertain-measurements/265698
  inverseVariance <- 1 / aBVar
  
  c(
    Estimate = sum(aToB * inverseVariance, na.rm=TRUE) / sum(inverseVariance, na.rm=TRUE),
    Std.Error = sqrt(1/sum(inverseVariance, na.rm=TRUE))
  )
}

SpeedToTimes <- function (estimate, error, exampleTimes = 
                            c(17, 18.5, 19, 19.5, 20, 21, 22, 25, 28, 30, 35) * 60) {
  
  if (missing(error)) {
    error <- estimate[2]
    estimate <- estimate[1]
  }
  exampleSpeeds <- 5000 / exampleTimes
  
  target <- 5000 / (exampleSpeeds + estimate)
  max <- 5000 / (exampleSpeeds + (estimate - error))
  min <- 5000 / (exampleSpeeds + (estimate + error))
  
  ret <- paste0(SecondsToMinutes(target), " ± ", round(colMeans(rbind(target - min, max - target))), 's')
  names(ret) <- SecondsToMinutes(exampleTimes)
  
  ret
  
}

# Ridiculous: riverside vs lincoln