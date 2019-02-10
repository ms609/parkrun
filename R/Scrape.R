#' Scrape results from parkrun
#' 
#' Reads the table of results from http://www.parkrun.org.uk/`eventName`/results/weeklyresults/?runSeqNumber=`runSeqNumber`
#' and returns the output as an array.
#' 
#' @param eventName Name of the event, taken from the URL (see details).
#' @param runSeqNumer Integer specifying the event number (see details).
#' 
#' @return An array corresponding to the HTML table on the page requested
#' 
#' @author Martin R. Smith
#' 
#' @importFrom RCurl getURL
ScrapeResults <- function(eventName, runSeqNumber) {
  url <- paste0("http://www.parkrun.org.uk/", eventName, "/results/weeklyresults/?runSeqNumber=", runSeqNumber)
  html <- getURLContent(url)
  tableRows <- strsplit(html, "<tr>", fixed=TRUE)
}