context("Scrape.R")

test_that("Results scraped correctly", {
  scrapings <- ScrapeResults('riverside', 300L)
  
  expect_equal(c(1L, 534L), range(scrapings$pos))
  expect_equal(2562692L, scrapings$athleteNumber[532])
  expect_equal(1037L, scrapings$timeInSeconds[1])
  expect_equal('JM10', scrapings$ageCat[520])
  expect_equivalent(as.table(c('F' = 244L, 'M' = 259L)), table(scrapings$gender))
  expect_equal(288L, max(scrapings$genderPos, na.rm=TRUE)) # unknowns treated as male
  expect_equal(296L, length(unique(scrapings$note)))
  expect_equal(32L, median(scrapings$totalRuns, na.rm=TRUE))
})