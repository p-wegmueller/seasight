test_that(".has_seats_model_switch_msg never crashes if summary.seas fails", {
  # Temporarily override summary.seas so summary(m) errors
  old <- getS3method("summary", "seas", optional = TRUE)
  summary.seas <- function(...) stop("boom")
  on.exit({
    if (!is.null(old)) {
      assign("summary.seas", old, envir = .GlobalEnv)
    } else {
      rm("summary.seas", envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  m <- structure(list(), class = "seas")
  expect_true(is.na(seasight:::.has_seats_model_switch_msg(m)))
})
