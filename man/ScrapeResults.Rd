% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scrape.R
\name{ScrapeResults}
\alias{ScrapeResults}
\title{Scrape results from parkrun}
\usage{
ScrapeResults(eventName, runSeqNumber)
}
\arguments{
\item{runSeqNumber}{Integer specifying the event number (see details).}
}
\value{
An array corresponding to the HTML table on the page requested
}
\description{
Reads the table of results from http://www.parkrun.org.uk/\code{eventName}/results/weeklyresults/?runSeqNumber=\code{runSeqNumber}
and returns the output as an array.
}
\author{
Martin R. Smith
}
