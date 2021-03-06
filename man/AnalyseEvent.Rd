% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyseEvent.R
\name{AnalyseEvent}
\alias{AnalyseEvent}
\title{Plot event history}
\usage{
AnalyseEvent(course, events, runsToQualify = length(events)/4)
}
\arguments{
\item{course}{event name}

\item{events}{Integer vector specifying events to analyse}

\item{runsToQualify}{How many events ought an athlete to have
attended in order to contribute to the analysis?  Lower numbers lead to more accurate,
but slower, analyses.}
}
\description{
Plot event history
}
\author{
Martin R. Smith
}
