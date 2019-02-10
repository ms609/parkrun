#' Scrape results from parkrun
#' 
#' Reads the table of results from http://www.parkrun.org.uk/`eventName`/results/weeklyresults/?runSeqNumber=`runSeqNumber`
#' and returns the output as an array.
#' 
#' @param eventName Name of the event, taken from the URL (see details).
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
  resultsTable <- strsplit(strsplit(html, '<tbody', fixed=TRUE)[[1]][2], '</tbody>', fixed=TRUE)[[1]][1]
  tableRows <- strsplit(strsplit(resultsTable, "<tr>", fixed=TRUE)[[1]][-1], '</tr>', fixed=TRUE)
  # columnNames <- strsplit(tableRows[[1]][1], "(?:</?th/?>)*</?th[^>.]*?>", perl=TRUE)[[1]][-1]
  tableCells <- vapply(tableRows, function(row) unlist(strsplit(row, '<td', fixed=TRUE)), character(12))
  
  times <- ParseCell(">([\\d\\:]+)</td>", tableCells[4, ], perl=TRUE)
  # From https://stackoverflow.com/questions/10835908:
  timeInSeconds <- as.numeric(as.POSIXct(strptime(times, format = "%M:%OS"))) - 
                as.numeric(as.POSIXct(strptime("0", format = "%S")))
  
  data.frame (
    pos = as.integer(ParseCell(".*>(\\d+)</td>", tableCells[2, ])),
    athleteNumber = as.integer(ParseCell(".*athleteNumber\\=(\\d+)\".*", tableCells[3, ])),
    time = times,
    timeInSeconds = timeInSeconds,
    ageCat = ParseCell(".*ageCat=(.*)\".*</td>", tableCells[5, ]),
    ageGrade = as.numeric(ParseCell(">(.*) %</td>", tableCells[6, ])),
    gender = ParseCell(">(.)</td>", tableCells[7, ]),
    genderPos = as.numeric(ParseCell(">(\\d*)</td>", tableCells[8, ])),
    note = ParseCell(">(.*)</td>", tableCells[10, ]),
    totalRuns = as.integer(ParseCell(">(\\d*)</td>", tableCells[11, ]))
  )
}

