test_that("sa_batch_run writes structured logs and resumes", {
  out_dir <- file.path(tempdir(), paste0("seasight-batch-", Sys.getpid()))
  fake_report <- function(y, outfile, ...) {
    writeLines("ok", outfile)
    invisible(list(report = outfile))
  }

  res <- sa_batch_run(
    series = list(a = 1:3, b = 4:6),
    out_dir = out_dir,
    timeout = 5,
    report_fun = fake_report
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(res$status, c("ok", "ok"))
  expect_true(all(file.exists(res$outfile)))

  log_file <- file.path(out_dir, "_sa_run_log.csv")
  expect_true(file.exists(log_file))
  log <- utils::read.csv(log_file, stringsAsFactors = FALSE)
  expect_equal(log$id, c("a", "b"))
  expect_true(all(c("ok", "status", "reason", "message", "outfile") %in% names(log)))

  resumed <- sa_batch_run(
    series = list(a = 1:3, b = 4:6),
    out_dir = out_dir,
    timeout = 5,
    report_fun = fake_report
  )
  expect_equal(resumed$status, c("skipped", "skipped"))
})

test_that("batch path IDs are Windows-safe and ANSI messages are stripped", {
  expect_equal(seasight:::.safe_path_id("bad:name/with*chars"), "bad_name_with_chars")
  expect_equal(seasight:::.strip_ansi("\033[31mred\033[39m"), "red")
})
