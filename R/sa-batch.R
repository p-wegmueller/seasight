.strip_ansi <- function(x) {
  x <- as.character(x %||% "")
  gsub("\\033\\[[0-9;]*[[:alpha:]]", "", x)
}

.safe_path_id <- function(x) {
  x <- as.character(x %||% "series")
  x <- gsub("[<>:\\"/\\\\|?*]+", "_", x)
  x <- gsub("\\s+", "_", trimws(x))
  x <- gsub("_+", "_", x)
  x <- sub("^\\.+", "", x)
  if (!nzchar(x)) "series" else x
}

.batch_one_report <- function(y, outfile, report_fun, dots, timeout) {
  started <- Sys.time()

  run_local <- function() {
    old_limit <- NULL
    if (is.finite(timeout) && timeout > 0) {
      old_limit <- tryCatch(base::setTimeLimit(elapsed = timeout, transient = TRUE), error = function(e) NULL)
      on.exit(try(base::setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), silent = TRUE), add = TRUE)
    }
    do.call(report_fun, c(list(y = y, outfile = outfile), dots))
  }

  out <- tryCatch(
    {
      if (requireNamespace("callr", quietly = TRUE) && identical(report_fun, sa_report_html)) {
        callr::r(
          func = function(y, outfile, dots) {
            do.call(seasight::sa_report_html, c(list(y = y, outfile = outfile), dots))
          },
          args = list(y = y, outfile = outfile, dots = dots),
          timeout = if (is.finite(timeout) && timeout > 0) timeout else Inf,
          spinner = FALSE,
          show = FALSE
        )
      } else {
        run_local()
      }
      list(ok = TRUE, status = "ok", reason = NA_character_, message = "completed")
    },
    callr_timeout_error = function(e) {
      list(ok = FALSE, status = "timeout", reason = "timeout", message = conditionMessage(e))
    },
    error = function(e) {
      msg <- conditionMessage(e)
      status <- if (grepl("time limit|reached elapsed time limit|timeout", msg, ignore.case = TRUE)) "timeout" else "error"
      list(ok = FALSE, status = status, reason = status, message = msg)
    }
  )

  out$elapsed_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  out
}

#' Run seasonal-adjustment reports over a list of series
#'
#' Sequential batch runner with per-series status rows, resume behavior and a
#' structured CSV log. If `callr` is installed and `report_fun` is the default
#' [sa_report_html()], each report is executed in a subprocess with the requested
#' timeout. Otherwise, the runner falls back to a local fail-soft call with a
#' base R elapsed-time limit.
#'
#' @param series A `ts` object or list of series. List names are used as IDs.
#' @param out_dir Directory where HTML reports and `_sa_run_log.csv` are written.
#' @param ids Optional character vector of IDs, same length as `series`.
#' @param timeout Per-series timeout in seconds. Use `Inf` or `NULL` for no timeout.
#' @param resume If `TRUE`, existing output files are skipped and logged as `skipped`.
#' @param report_fun Report function. Defaults to [sa_report_html()]. Primarily useful
#'   for tests or custom report wrappers.
#' @param log_file Optional CSV log path. Defaults to `_sa_run_log.csv` in `out_dir`.
#' @param ... Additional arguments passed to `report_fun`.
#'
#' @return A tibble with one row per series: `id`, `ok`, `status`, `reason`,
#'   `message`, `outfile` and `elapsed_seconds`.
#' @export
sa_batch_run <- function(series,
                         out_dir = "sa_reports",
                         ids = NULL,
                         timeout = 300,
                         resume = TRUE,
                         report_fun = sa_report_html,
                         log_file = NULL,
                         ...) {
  if (inherits(series, "ts")) series <- list(series = series)
  if (!is.list(series) || !length(series)) {
    stop("`series` must be a non-empty list of series or a single ts object.", call. = FALSE)
  }

  n <- length(series)
  if (is.null(ids)) {
    ids <- names(series)
    if (is.null(ids) || any(!nzchar(ids))) ids <- paste0("series", seq_len(n))
  }
  if (length(ids) != n) stop("`ids` must have the same length as `series`.", call. = FALSE)

  timeout <- if (is.null(timeout)) Inf else suppressWarnings(as.numeric(timeout)[1])
  if (!is.finite(timeout)) timeout <- Inf

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(log_file)) log_file <- file.path(out_dir, "_sa_run_log.csv")

  dots <- list(...)
  rows <- vector("list", n)

  for (i in seq_len(n)) {
    id <- as.character(ids[[i]])
    outfile <- file.path(out_dir, paste0(.safe_path_id(id), ".html"))

    if (isTRUE(resume) && file.exists(outfile)) {
      row <- list(ok = TRUE, status = "skipped", reason = "exists", message = "output exists", elapsed_seconds = 0)
    } else {
      row <- .batch_one_report(series[[i]], outfile, report_fun, dots, timeout)
    }

    rows[[i]] <- tibble::tibble(
      id = id,
      ok = isTRUE(row$ok),
      status = as.character(row$status %||% if (isTRUE(row$ok)) "ok" else "error"),
      reason = as.character(row$reason %||% NA_character_),
      message = .strip_ansi(row$message %||% ""),
      outfile = outfile,
      elapsed_seconds = as.numeric(row$elapsed_seconds %||% NA_real_)
    )

    utils::write.csv(dplyr::bind_rows(rows[seq_len(i)]), log_file, row.names = FALSE, na = "")
  }

  dplyr::bind_rows(rows)
}

#' Probe one seasonal-adjustment case
#'
#' Convenience helper for reproducing a problematic single series. It writes a
#' temporary HTML report by default and returns the report path invisibly.
#'
#' @param y Series to analyse.
#' @param outfile Output HTML path. Defaults to a temporary file.
#' @param verbose If `TRUE`, print the output path.
#' @param ... Additional arguments passed to [sa_report_html()].
#'
#' @return Invisibly, the normalized output path.
#' @export
probe_case <- function(y, outfile = tempfile("seasight-probe-", fileext = ".html"), verbose = TRUE, ...) {
  res <- sa_report_html(y = y, outfile = outfile, ...)
  path <- normalizePath(res$report %||% outfile, mustWork = FALSE)
  if (isTRUE(verbose)) message("Probe report written: ", path)
  invisible(path)
}
