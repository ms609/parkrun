#' Scrape results from parkrun
#' 
#' Reads the table of results from http://www.parkrun.org.uk/`eventName`/results/weeklyresults/?runSeqNumber=`runSeqNumber`
#' and returns the output as an array.
#' 
#' @param runSeqNumber Integer specifying the event number (see details).
#' 
#' @return An array corresponding to the HTML table on the page requested
#' 
#' @author Martin R. Smith
#' 
#' @importFrom RCurl getURL
#' @export
ScrapeResults <- function(eventName, runSeqNumber) {
  
  ParseCell <- function(expression, string, perl=FALSE) {
    ifelse(string == '/>' | string == ">Unknown</td>", NA, sub(expression, "\\1", string, perl=perl))
  }
  
  url <- paste0("http://www.parkrun.org.uk/", eventName, "/results/weeklyresults/?runSeqNumber=", runSeqNumber)
  html <- getURLContent(url)
  
  # Load results table
  resultsTable <- strsplit(strsplit(html, '<tbody', fixed=TRUE)[[1]][2], '</tbody>', fixed=TRUE)[[1]][1]
  tableRows <- strsplit(strsplit(resultsTable, "<tr>", fixed=TRUE)[[1]][-1], '</tr>', fixed=TRUE)
  # columnNames <- strsplit(tableRows[[1]][1], "(?:</?th/?>)*</?th[^>.]*?>", perl=TRUE)[[1]][-1]
  tableCells <- vapply(tableRows, function(row) unlist(strsplit(row, '<td', fixed=TRUE)), character(12))
  
  times <- ParseCell(">([\\d\\:]+)</td>", tableCells[4, ], perl=TRUE)
  timeBits <- strsplit(times, ':', fixed=TRUE)
  timeInSeconds <- vapply(timeBits, 
                          function (bits) as.integer(sum(as.integer(bits) * (60L^rev(seq_along(bits) - 1L)))), 
                          integer(1))
  gender <- as.factor(ParseCell(">(.)</td>", tableCells[7, ]))
  genderUnknown <- is.na(gender)
  male <- !genderUnknown & gender == 'M'
  female <- !genderUnknown & gender == 'F'
  
  # Document event characteristics
  eventIndex <- paste0(EventDirectory(eventName), '/index.txt')
  eventDate <- as.Date(sub(".*(\\d\\d)/(\\d\\d)/(\\d\\d\\d\\d)</", "\\3-\\2-\\1", strsplit(html, 'h2>', fixed=TRUE)[[1]][2]))
  
  averageSpeed <- 5000L / timeInSeconds
  eventSummary <- data.frame(row.names = runSeqNumber, 
                             date = format(as.Date(eventDate, origin='1970-01-01'), '%Y-%m-%d'),
                             athletes = length(gender),
                             maleAthletes = sum(male), femaleAthletes = sum(female),
                             maleSpeedMean = mean(averageSpeed[male]), maleSpeedSD = sd(averageSpeed[male]),
                    femaleSpeedMean = mean(averageSpeed[female]), femaleSpeedSD = sd(averageSpeed[female]),
                    extraTime = NA)
  
  if (!file.exists(eventIndex)) {
    ScrapeEventHistory(eventName, eventIndex)
  } else {
    index <- read.table(eventIndex, as.is=TRUE)
    index[as.character(runSeqNumber), ] <- eventSummary
    write.table(index, file=eventIndex)
  }
  
  # Return:
  data.frame (
    eventName = as.factor(eventName),
    runSeqNumber = as.factor(runSeqNumber),
    pos = as.integer(ParseCell(".*>(\\d+)</td>", tableCells[2, ])),
    athleteNumber = as.factor(ParseCell(".*athleteNumber\\=(\\d+)\".*", tableCells[3, ])),
    time = times,
    timeInSeconds = timeInSeconds,
    ageCat = as.factor(ParseCell(".*ageCat=(.*)\".*</td>", tableCells[5, ])),
    ageGrade = as.numeric(ParseCell(">(.*) %</td>", tableCells[6, ])),
    gender = gender,
    genderPos = as.numeric(ParseCell(">(\\d*)</td>", tableCells[8, ])),
    note = ParseCell(">(.*)</td>", tableCells[10, ]),
    totalRuns = as.integer(ParseCell(">(\\d*)</td>", tableCells[11, ]))
  )
}

#' Start an index file by scraping the dates of previous events
#' @param eventIndex Path to `index.txt` file containing details of each event
#' @export
ScrapeEventHistory <- function (eventName, eventIndex) {
  dir.create(EventDirectory(eventName))
  
  ParseCell <- function(expression, string, perl=FALSE) {
    ifelse(string == '/>' | string == ">Unknown</td>", NA, sub(expression, "\\1", string, perl=perl))
  }
  
  url <- paste0("http://www.parkrun.org.uk/", eventName, "/results/eventhistory/")
  html <- getURLContent(url)
  
  runDateRegExp <- '\\?runSeqNumber=(\\d+)\\\\*">(\\d\\d)/(\\d\\d)/(\\d\\d\\d\\d)'
  dates <- format(as.Date(sub(pattern=runDateRegExp, replacement="\\4-\\3-\\2", 
      regmatches(html, gregexpr(runDateRegExp, html))[[1]])), "%Y-%m-%d")
  write.table(
    data.frame(row.names = as.character(rev(seq_along(dates))), date = as.integer(dates),
               athletes = NA, maleAthletes = NA, femaleAthletes = NA,
               maleSpeedMean = NA, maleSpeedSD = NA, femaleSpeedMean = NA, femaleSpeedSD = NA,
               extraTime = NA),
    file = eventIndex)
  
}

#' Determine path to local cache of results
#' @export
EventDirectory <- function (eventName) {
  paste0(sub("(.*parkrun).*", "\\1", getwd()), '/results/', eventName)
}

#' Load cache of past event details
#' @template eventNameParam
#' 
#' @export
EventHistory <- function (eventName) {
  read.table(paste0(EventDirectory(eventName), "/index.txt"))
}

#' Obtain parkrun results from cache, scraping if not available
#' @inheritParams ScrapeResults
#' 
#' @author Martin R. Smith
#' @export
GetResults <- function (eventName, runSeqNumber) {
  
  eventFile <- paste0(EventDirectory(eventName), '/', runSeqNumber, '.txt')
  
  if (file.exists(eventFile)) {
    results <- read.table(eventFile)
    #results <- read.table(eventFile, colClasses=c('factor', 'factor', 'integer', 'factor', 'character', 'numeric', 'factor',
    #                                              'numeric', 'factor', 'integer', 'character', 'integer'))
  } else {
    results <- ScrapeResults(eventName, runSeqNumber)
    Sys.sleep(runif(1) * 12) # Be polite: avoid overloading server
    if (!dir.exists(EventDirectory(eventName))) dir.create(EventDirectory(eventName))
    write.table(results, eventFile)
  }
  
  results$eventName <- as.factor(results$eventName)
  results$runSeqNumber <- as.factor(results$runSeqNumber)
  results$athleteNumber <- as.factor(results$athleteNumber)
  results$ageCat <- as.factor(results$ageCat)
  results$gender <- as.factor(results$gender)
  
  # Return: 
  results
}

#' Summarise parkrun results
#' 
#' @param results Data frame of results, perhaps generated by [ScrapeResults]
#' @param athlete Integer vector specifying athlete(s) whose speeds should be indicated on the plot
#' 
#' @author Martin R. Smith
#' @export
SummariseResults <- function (results, athlete=NA, labelSpeed = FALSE) {
  with(results, {
    
    male <- ifelse(is.na(gender), FALSE, gender == 'M')
    female <- ifelse(is.na(gender), FALSE, gender == 'F')
    
    maleTimes <- timeInSeconds[male]
    femaleTimes <- timeInSeconds[female]
    
    maleSpeeds <- 5000 / maleTimes
    femaleSpeeds <- 5000 / femaleTimes
    
    shapiro.test(maleSpeeds)
    shapiro.test(femaleSpeeds)
    
    maleCol <- "#6688dd"
    femaleCol <- "#eeaa66"
    
    breakSize <- 0.25
    breaks <- seq(min(c(maleSpeeds, femaleSpeeds)) - breakSize, max(maleSpeeds) + breakSize, by=breakSize)
  
    maleHist <- hist(maleSpeeds, breaks=breaks, plot=FALSE)
    femaleHist <- hist(femaleSpeeds, breaks=breaks, plot=FALSE)
    
    
    plot.new()
    plot.window(xlim=range(breaks), ylim=range(c(maleHist$counts, femaleHist$counts)))
    timeTicks <- seq(10, 60, by=5)
    
    if (labelSpeed) {
      axis(1)
      mtext("Speed (m/s)", 1, line=2)
    } else {
      axis(1, at=5000 / (60 * timeTicks), labels=timeTicks)
      mtext("Time (minutes)", 1, line=2)
    }
    axis(2)
    mtext("Athletes", 2, line=2)
    
    plot(maleHist, col=paste0(maleCol, '88'), add=TRUE)
    plot(femaleHist, col='#eeaa6688', add=TRUE)
  
    abline(v=mean(maleSpeeds), col=maleCol, lty=2)
    curve(max(maleHist$counts) * dnorm(x, mean(maleSpeeds), sd(maleSpeeds)),
          min(maleSpeeds), max(maleSpeeds), add=T, col=maleCol)
    
    abline(v=mean(femaleSpeeds), col=femaleCol, lty=2)
    curve(max(femaleHist$counts) * dnorm(x, mean(femaleSpeeds), sd(femaleSpeeds)),
          min(femaleSpeeds), max(femaleSpeeds), add=T, col=femaleCol)
    
    
    myAthlete <- ifelse(is.na(athleteNumber), FALSE, athleteNumber %in% athlete)
    if (any(myAthlete)) {
      abline(v = 5000 / timeInSeconds[myAthlete], col='#228835', lwd=2)
    }
  })
}


