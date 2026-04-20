.strip_ansi <- function(x) {
  x <- as.character(x)
  gsub("\033\\[[0-?]*[ -/]*[@-~]", "", x, perl = TRUE)
}

.safe_path_id <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  if (!nzchar(x)) x <- "series"
  x <- gsub('[<>:"/\\\\|?*]', "_", x)
  x <- gsub("[[:cntrl:]]+", "_", x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("_+", "_", x)
  x <- sub("^_+", "", x)
  x <- sub("_+$", "", x)
  if (tolower(x) %in% c("con", "prn", "aux", "nul", paste0("com", 1:9), paste0("lpt", 1:9))) {
    x <- paste0(x, "_")
  }
  if (!nzchar(x)) "series" else x
}

.batch_one_report <- function(y, outfile, report_fun, dots, timeout = Inf) {
  started <- Sys.time()

  out <- tryCatch(
    {
      if (is.finite(timeout)) {
        if (!requireNamespace("callr", quietly = TRUE)) {
          stop("Package `callr` is required when `timeout` is finite.", call. = FALSE)
        }
        callr::r(
          func = function(y, outfile, report_fun, dots) {
            do.call(report_fun, c(list(y = y, outfile = outfile), dots))
          },
          args = list(y = y, outfile = outfile, report_fun = report_fun, dots = dots),
          timeout = timeout,
          stdout = "|",
          stderr = "|"
        )
      } else {
        do.call(report_fun, c(list(y = y, outfile = outfile), dots))
      }
      list(ok = TRUE, status = "ok", reason = NA_character_, message = "completed")
    },
    callr_timeout_error = function(e) {
      list(ok = FALSE, status = "timeout", reason = "timeout", message = conditionMessage(e))
    },
    error = function(e) {
      list(ok = FALSE, status = "error", reason = "error", message = conditionMessage(e))
    }
  )

  out$elapsed_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  out
}

#' Run seasonal-adjustment reports over a list of series
#'
#' Sequential batch runner with per-series status rows and a structured CSV log.
#' Existing outputs can be skipped via `resume = TRUE`.
#'
#' @param series A `ts` object or a non-empty list of series.
#' @param out_dir Directory where HTML reports and `_sa_run_log.csv` are written.
#' @param ids Optional character vector of IDs, same length as `series`.
#' @param resume If `TRUE`, existing output files are skipped and logged as `skipped`.
#' @param timeout Per-series timeout in seconds. Use `Inf` to run in the current
#'   R process without a timeout. Finite values require the suggested `callr`
#'   package and run each case in a clean subprocess.
#' @param report_fun Report function to call per item. Defaults to [sa_report_html()].
#' @param log_file Optional CSV log path. Defaults to `_sa_run_log.csv` in `out_dir`.
#' @param ... Additional arguments passed to `report_fun`.
#'
#' @return A tibble with one row per series: `id`, `ok`, `status`, `reason`,
#'   `message`, `outfile`, and `elapsed_seconds`.
#' @export
sa_batch_run <- function(series,
                         out_dir = "sa_reports",
                         ids = NULL,
                         resume = TRUE,
                         timeout = Inf,
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
      row <- .batch_one_report(series[[i]], outfile, report_fun, dots, timeout = timeout)
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
#' Convenience helper for reproducing a problematic single series. It writes an
#' HTML report and returns the normalized output path invisibly.
#'
#' @param y Series to analyse.
#' @param outfile Output HTML path. Defaults to a temporary file.
#' @param verbose If `TRUE`, print the output path.
#' @param report_fun Report function. Defaults to [sa_report_html()].
#' @param ... Additional arguments passed to `report_fun`.
#'
#' @return Invisibly, the normalized output path.
#' @export
probe_case <- function(y,
                       outfile = tempfile("seasight-probe-", fileext = ".html"),
                       verbose = TRUE,
                       report_fun = sa_report_html,
                       ...) {
  res <- report_fun(y = y, outfile = outfile, ...)
  path <- normalizePath(res$report %||% outfile, mustWork = FALSE)
  if (isTRUE(verbose)) message("Probe report written: ", path)
  invisible(path)
}
