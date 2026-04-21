test_that("calendar documentation avoids package-shipped holiday datasets", {
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  paths <- c(
    file.path(root, "README.md"),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE)
  )
  txt <- unlist(lapply(paths, readLines, warn = FALSE), use.names = FALSE)

  expect_false(any(grepl("data\\([\"']holiday[\"']", txt)))
  expect_false(any(grepl("data\\([\"']chcal[\"']", txt)))
  expect_false(any(grepl("\\bholiday\\$", txt)))
  expect_false(any(grepl("\\bchcal\\$", txt)))
})
