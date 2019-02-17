#' Convert seconds to minutes
#' 
#' @param s Number of seconds
#' 
#' @return A character vector giving a time in minutes, e.g. "61:04".
#' @author Martin R. Smith
#' @export
SecondsToMinutes <- function (s) {
  s <- round(s)
  paste(s %/% 60, formatC(round(s %% 60, 0), width=2, flag='0'), sep = ':')
}
