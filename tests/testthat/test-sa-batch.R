test_that("sa_batch_run executes and logs basic status rows", {
  out_dir <- file.path(tempdir(), paste0("seasight-batch-", Sys.getpid(), "-", as.integer(stats::runif(1, 1, 1e6))))

  fake_report <- function(y, outfile, ...) {
    writeLines(as.character(length(y)), outfile)
    invisible(list(report = outfile))
  }

  res <- sa_batch_run(
    series = list(a = 1:3, b = 4:8),
    out_dir = out_dir,
    resume = FALSE,
    report_fun = fake_report
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(res$id, c("a", "b"))
  expect_equal(res$status, c("ok", "ok"))
  expect_true(all(res$ok))
  expect_true(all(file.exists(res$outfile)))

  log_file <- file.path(out_dir, "_sa_run_log.csv")
  expect_true(file.exists(log_file))
  log <- utils::read.csv(log_file, stringsAsFactors = FALSE)
  expect_equal(log$id, c("a", "b"))
  expect_true(all(c("ok", "status", "reason", "message", "outfile", "elapsed_seconds") %in% names(log)))
})

test_that("sa_batch_run supports resume skip behavior", {
  out_dir <- file.path(tempdir(), paste0("seasight-batch-resume-", Sys.getpid(), "-", as.integer(stats::runif(1, 1, 1e6))))

  fake_report <- function(y, outfile, ...) {
    writeLines("ok", outfile)
    invisible(list(report = outfile))
  }

  first <- sa_batch_run(
    series = list(a = 1:3, b = 4:6),
    out_dir = out_dir,
    report_fun = fake_report
  )
  expect_equal(first$status, c("ok", "ok"))

  resumed <- sa_batch_run(
    series = list(a = 1:3, b = 4:6),
    out_dir = out_dir,
    report_fun = fake_report,
    resume = TRUE
  )
  expect_equal(resumed$status, c("skipped", "skipped"))
  expect_true(all(resumed$ok))
})

test_that("path and message sanitizers are stable", {
  expect_equal(seasight:::.safe_path_id("bad:name/with*chars"), "bad_name_with_chars")
  expect_equal(seasight:::.safe_path_id("   "), "series")
  expect_equal(seasight:::.strip_ansi("\033[31mred\033[39m"), "red")
})

test_that("probe_case writes via report_fun and returns normalized path", {
  out <- tempfile("probe-case-", fileext = ".html")
  fake_report <- function(y, outfile, ...) {
    writeLines("ok", outfile)
    invisible(list(report = outfile))
  }

  p <- probe_case(1:12, outfile = out, verbose = FALSE, report_fun = fake_report)
  expect_true(file.exists(out))
  expect_equal(p, normalizePath(out, mustWork = FALSE))
})
