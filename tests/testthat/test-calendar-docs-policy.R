test_that("calendar documentation avoids package-shipped holiday datasets", {
  wd <- normalizePath(getwd(), mustWork = TRUE)
  candidates <- unique(normalizePath(
    c(
      wd,
      file.path(wd, ".."),
      file.path(wd, "..", ".."),
      file.path(wd, "..", "..", ".."),
      Sys.getenv("R_PACKAGE_SOURCE", unset = NA_character_)
    ),
    mustWork = FALSE
  ))
  candidates <- candidates[!is.na(candidates)]
  root <- candidates[file.exists(file.path(candidates, "README.md")) &
                       dir.exists(file.path(candidates, "vignettes"))][1]
  skip_if(is.na(root), "source documentation files are not available")

  paths <- c(
    file.path(root, "README.md"),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE)
  )
  paths <- paths[file.exists(paths)]
  skip_if_not(length(paths) > 0, "source documentation files are not available")

  txt <- unlist(lapply(paths, readLines, warn = FALSE), use.names = FALSE)

  expect_false(any(grepl("data\\([\"']holiday[\"']", txt)))
  expect_false(any(grepl("data\\([\"']chcal[\"']", txt)))
  expect_false(any(grepl("\\bholiday\\$", txt)))
  expect_false(any(grepl("\\bchcal\\$", txt)))
})
