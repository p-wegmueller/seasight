# =============================================================================
# sa-report.R Build the Seasonal Adjustment HTML report
# =============================================================================

#' Build the Seasonal Adjustment HTML report (engine)
#'
#' Internal workhorse that renders the HTML report. Most users should call
#' [sa_report_html()], which is a thin wrapper around this function.
#'
#' @inheritParams sa_report_html
#' @keywords internal
#' @noRd
sa_issue_report_html <- function(
    y,
    current_model = NULL,
    td_usertype = "td",
    td_candidates = NULL,
    use_fivebest = TRUE,
    title = "Seasonal Adjustment Report",
    outfile = "sa_report.html",
    png_width = 1400, png_height = 900,
    print_to_console = FALSE,
    print_which = c("new","current","both"),
    include_easter = c("auto","always","off"),
    easter_len = 15L,
    engine = c("seats","x11","auto"),
    w_engine = 1,
    outlier_types    = c("AO","LS","TC"),
    outlier_method   = "AddOne",
    outlier_critical = 4,
    outlier_alpha    = NULL
){
  # ---------- small helpers -------------------------------------------------
  
  # Local AICc helper (safe across engines)
  .aicc <- function(m) {
    aic <- suppressWarnings(tryCatch(stats::AIC(m), error = function(e) NA_real_))
    k   <- suppressWarnings(tryCatch(length(stats::coef(m)), error = function(e) NA_integer_))
    n   <- suppressWarnings(tryCatch(length(stats::na.omit(seasonal::original(m))), error = function(e) NA_integer_))
    if (!is.finite(aic) || !is.finite(k) || !is.finite(n) || n <= (k + 1)) return(aic)
    aic + (2 * k * (k + 1)) / (n - k - 1)
  }
  
  .render_top_candidates_table <- function(df) {
    # Render compact HTML table for the "Top candidates" block.
    has <- function(nm) nm %in% names(df)
    tbl <- df %>%
      dplyr::mutate(
        TD   = dplyr::case_when(has("with_td")  ~ ifelse(.data$with_td, "yes", "no"),
                                TRUE            ~ NA_character_),
        QS_X11     = if (has("QS_p_x11"))   .flagP(.data$QS_p_x11,   threshold = 0.10) else NA_character_,
        QS_SEATS   = if (has("QS_p_seats")) .flagP(.data$QS_p_seats, threshold = 0.10) else NA_character_,
        QS_overall = if (has("QS_p"))       .flagP(.data$QS_p,       threshold = 0.10) else NA_character_,
        LB_p     = if (has("LB_p")) .flagP(.data$LB_p, threshold = 0.05) else NA_character_,
        AICc     = if (has("AICc")) .num(.data$AICc, 2) else NA_character_,
        Amp_pct  = if (has("seasonal_amp_pct")) .pct(.data$seasonal_amp_pct, 1) else NA_character_,
        Vola_red = if (has("vola_reduction_pct")) .pct(.data$vola_reduction_pct, 1) else NA_character_,
        Dist_L1  = if (has("dist_sa_L1")) .num(.data$dist_sa_L1, 2) else NA_character_
      ) %>%
      dplyr::mutate(
        Model = dplyr::case_when(
          has("ARIMA_disp") ~ .data$ARIMA_disp,
          has("arima")      ~ as.character(.data$arima),
          has("spec_id")    ~ as.character(.data$spec_id),
          TRUE              ~ "\u2014"
        )
      ) %>%
      dplyr::transmute(
        Best, Incumbent, Model, TD, AICc, LB_p, QS_X11, QS_SEATS, QS_overall, Vola_red, Amp_pct, Dist_L1,
        .row_class = dplyr::case_when(
          is_best ~ "row-best",
          is_incumbent ~ "row-prev",
          TRUE ~ ""
        )
      )
    
    th <- htmltools::tags$tr(
      lapply(c("Best","Incumbent","Model","TD","AICc","LB p","QS X-11","QS SEATS","QS overall","Vola \u2193","Amp %","Dist L1"),
             function(x) htmltools::tags$th(x))
    )
    
    rows <- purrr::pmap(
      list(
        tbl$Best, tbl$Incumbent, tbl$Model, tbl$TD, tbl$AICc, tbl$LB_p, tbl$QS_X11,
        tbl$QS_SEATS, tbl$QS_overall, tbl$Vola_red, tbl$Amp_pct, tbl$Dist_L1, tbl$.row_class
      ),
      function(Best, Incumbent, Model, TD, AICc, LB_p, QS_X11, QS_SEATS, QS_overall, Vola_red, Amp_pct, Dist_L1, cls) {
        htmltools::tags$tr(class = cls,
                           htmltools::tags$td(Best),
                           htmltools::tags$td(Incumbent),
                           htmltools::tags$td(htmltools::HTML(htmltools::htmlEscape(Model))),
                           htmltools::tags$td(TD),
                           htmltools::tags$td(AICc),
                           htmltools::tags$td(LB_p),
                           htmltools::tags$td(QS_X11),
                           htmltools::tags$td(QS_SEATS),
                           htmltools::tags$td(QS_overall),
                           htmltools::tags$td(Vola_red),
                           htmltools::tags$td(Amp_pct),
                           htmltools::tags$td(Dist_L1)
        )
      }
    )
    
    legend <- htmltools::div(class = "legend",
                             htmltools::span(class = "chip chip-best", "\u2705 best"),
                             htmltools::span(class = "chip chip-prev", "\u2B50 current")
    )
    
    htmltools::tagList(
      htmltools::tags$table(class = "tbl tbl-colored", th, rows),
      legend
    )
  }
  
  # ---------- args -----------------------------------------------------------
  print_which    <- match.arg(print_which)
  include_easter <- if (is.logical(include_easter)) { if (include_easter) "auto" else "off" } else { match.arg(include_easter) }
  engine         <- match.arg(engine)
  
  # --- run selector with the user's knobs ---
  res <- auto_seasonal_analysis(
    y = y,
    use_fivebest = use_fivebest,
    auto_outliers = TRUE,
    include_easter = include_easter,
    easter_len = easter_len,
    transform_fun = "auto",
    td_usertype = td_usertype,
    td_candidates = td_candidates,
    current_model = current_model,
    include_history_top_n = 10,
    outlier_types    = outlier_types,
    outlier_method   = outlier_method,
    outlier_critical = outlier_critical,
    outlier_alpha    = outlier_alpha,
    engine           = engine,
    w_engine         = w_engine
  )
  
  # Consolidate ARIMA display string
  candidates <- .coalesce_arima_cols(res$table)
  best_r     <- dplyr::slice(candidates, 1)
  best       <- res$best
  
  best_engine  <- toupper(.engine_used(best))
  best_has_td  <- isTRUE(best_r$with_td)
  best_td_name <- if (best_has_td) (best_r$td_label %||% best_r$td_name %||% NULL) else NULL
  
  # Try to recover the incumbent id robustly from result structure
  incumbent_spec_id <- res$incumbent_spec_id %||% res$current_spec_id %||% {
    if ("is_current" %in% names(candidates) && any(candidates$is_current, na.rm = TRUE)) {
      candidates$spec_id[which.max(candidates$is_current)]
    } else NA_character_
  }
  
  # ----- Outlier rule line (Diagnostics card) ------------------------
  ol_crit <- if (!is.null(outlier_alpha)) stats::qnorm(1 - outlier_alpha/2) else outlier_critical
  ol_rule_line <- glue::glue(
    "Outlier detection: method {outlier_method}; types = {paste(outlier_types, collapse = ', ')}; ",
    "critical z = {.num_safe(ol_crit, 2)}",
    "{if (!is.null(outlier_alpha)) glue::glue(' (\u2248 \u03B1 = {.num_safe(outlier_alpha, 3)})') else ''}."
  )
  
  # Seasonality call & DNA rule
  no_sa <- identical(res$seasonality$overall$call_overall[1], "DO_NOT_ADJUST")
  dna   <- no_sa || .do_not_adjust(best_r)
  
  y_expr <- deparse(substitute(y))
  
  if (no_sa) {
    best_sa <- .as_ts(y)
  } else {
    best_sa <- tryCatch(seasonal::final(best), error = function(e) NULL)
  }
  
  # --- Decision pill text -------------------------------------------------
  has_baseline <- !is.null(current_model) || !is.null(res$baseline$current_sa)
  decision_txt <- if (dna) {
    "DO_NOT_ADJUST"
  } else if (!has_baseline) {
    "no current model provided"
  } else {
    sa_should_switch(res)  # "CHANGE_TO_NEW_MODEL" or "KEEP_CURRENT_MODEL"
  }
  
  decision_reason <- NULL
  if (identical(decision_txt, "KEEP_CURRENT_MODEL") && is.finite(best_r$LB_p) && best_r$LB_p < 0.05) {
    decision_reason <- "Best candidate fails residual autocorrelation test"
  } else if (identical(decision_txt, "KEEP_CURRENT_MODEL") && is.finite(best_r$corr_seas) && best_r$corr_seas > 0.995) {
    decision_reason <- "New model produces almost identical seasonal factors"
  }
  
  # Copy-paste code blocks
  if (no_sa) {
    best_spec_code <- paste0(
      "## No seasonal adjustment recommended\n",
      "# Reason: QSori (X-11=", .fmtP(best_r$QSori_p_x11),
      ", SEATS=",               .fmtP(best_r$QSori_p_seats),
      "), IDS=",                best_r$IDS,
      ", M7=",                  .num(best_r$M7, 2), "\n",
      "sa <- ", y_expr
    )
  } else {
    best_spec_code <- sa_copyable_call(
      best,
      x_expr    = y_expr,
      xreg_expr = if (best_has_td) best_td_name else NULL,
      include_force = FALSE,
      engine = "auto"
    )
  }
  
  # Derive a robust ARIMA string for display
  best_arima_disp <- if (no_sa) {
    NA_character_
  } else {
    from_code <- tryCatch(.extract_arima_str(best_spec_code), error = function(e) NA_character_)
    if (is.character(from_code) && nzchar(from_code)) {
      from_code
    } else {
      # fallback to row \u2192 ARIMA_disp \u2192 arima
      z <- NA_character_
      if ("ARIMA_disp" %in% names(best_r) && is.character(best_r$ARIMA_disp) && nzchar(best_r$ARIMA_disp)) {
        z <- best_r$ARIMA_disp
      } else if ("arima" %in% names(best_r) && is.character(best_r$arima) && nzchar(best_r$arima)) {
        z <- best_r$arima
      }
      z
    }
  }
  
  prev_spec_code <- if (!is.null(current_model)) {
    sa_copyable_call(current_model,
                     x_expr = y_expr,
                     xreg_expr = NA,
                     engine = .engine_used(current_model))
  } else NULL
  
  # Console-only mode
  if (isTRUE(print_to_console)) {
    if (print_which %in% c("new","both")) {
      cat("\n# ---- New model ----\n"); cat(best_spec_code, "\n")
    }
    if (!is.null(prev_spec_code) && print_which %in% c("current","both")) {
      cat("\n# ---- Current model ----\n"); cat(prev_spec_code, "\n")
    }
    return(invisible(list(best_code = best_spec_code, prev_code = prev_spec_code, res = res)))
  }
  
  # 3) current/best details & outliers (hide 'best' outliers if DNA)
  prev_sa <- prev_outliers <- NULL
  if (!is.null(current_model)) {
    prev_sa       <- tryCatch(seasonal::final(current_model), error = function(e) NULL)
    prev_outliers <- tryCatch(extract_outliers(current_model),   error = function(e) NULL)
  }
  best_outliers <- if (dna) tibble::tibble(type=character(), period=character(), coef=numeric())
  else tryCatch(extract_outliers(best), error = function(e) tibble::tibble(type=character(), period=character(), coef=numeric()))
  
  selection_para <- .build_selection_rationale(res, current_model, override_best_arima = best_arima_disp)
  
  results_para   <- .build_results_commentary(
    prev_sa        = prev_sa,
    best_sa        = best_sa,
    best_outliers  = best_outliers,
    prev_outliers  = if (!is.null(prev_outliers)) prev_outliers else NULL
  )
  
  # 4) SA comparison/coefficients \u2014 only if incumbent differs from best
  render_alt <- .should_show_alternative(candidates, incumbent_spec_id)
  
  img_cmp_sa  <- img_cmp_pc <- stats_tbl_html <- NULL
  if (isTRUE(render_alt) && !is.null(current_model)) {
    # SA levels comparison
    al_sa <- .align2(prev_sa, best_sa)
    if (isTRUE(al_sa$ok)) {
      p_cmp_sa <- tempfile(fileext = ".png")
      grDevices::png(p_cmp_sa, width = png_width, height = png_height)
      stats::ts.plot(al_sa$prev, al_sa$new, col = c("black","firebrick"), lwd = 2,
                     ylab = "", main = "Seasonally Adjusted Levels \u2014 current vs New")
      graphics::legend("topleft", legend = c("current","New"),
                       col = c("black","firebrick"), lty = 1, lwd = 2, bty = "n")
      grDevices::dev.off()
      img_cmp_sa <- htmltools::img(src = knitr::image_uri(p_cmp_sa),
                                   style = "max-width:100%;height:auto;border-radius:12px;")
    } else {
      img_cmp_sa <- htmltools::tags$p(class = "sub",
                                      paste0("Comparison vs current skipped (", al_sa$reason, ")."))
    }
    
    # SA growth comparison (percent change)
    g_prev <- .pc_or_null(prev_sa)
    g_new  <- .pc_or_null(best_sa)
    al_pc  <- .align2(g_prev, g_new)
    if (isTRUE(al_pc$ok)) {
      p_cmp_pc <- tempfile(fileext = ".png")
      grDevices::png(p_cmp_pc, width = png_width, height = png_height)
      stats::ts.plot(al_pc$prev, al_pc$new, col = c("gray25","royalblue"), lwd = 2,
                     ylab = "", main = "SA Growth (percent change) \u2014 current vs New")
      graphics::abline(h = 0, col = "gray80", lwd = 1)
      graphics::legend("topleft", legend = c("current","New"),
                       col = c("gray25","royalblue"), lty = 1, lwd = 2, bty = "n")
      grDevices::dev.off()
      img_cmp_pc <- htmltools::img(src = knitr::image_uri(p_cmp_pc),
                                   style = "max-width:100%;height:auto;border-radius:12px;")
      stats_tbl_html <- .build_growth_stats_table(al_pc$prev, al_pc$new)
    } else {
      img_cmp_pc <- htmltools::tags$p(class = "sub",
                                      paste0("Growth comparison skipped (", al_pc$reason, ")."))
    }
  }
  
  # 5) Plots \u2014 respect DNA (show raw series panels if DNA)
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  p_best_main <- tempfile(fileext = ".png")
  p_best_mon  <- tempfile(fileext = ".png")
  
  if (!no_sa && !is.null(best)) {
    grDevices::png(p_best_main, width = png_width, height = png_height);  graphics::plot(best);      grDevices::dev.off()
    grDevices::png(p_best_mon,  width = png_width, height = png_height);  stats::monthplot(best);     grDevices::dev.off()
    img_best_main <- htmltools::img(src = knitr::image_uri(p_best_main), style = "max-width:100%;height:auto;border-radius:12px;")
    img_best_mon  <- htmltools::img(src = knitr::image_uri(p_best_mon),  style = "max-width:100%;height:auto;border-radius:12px;")
  } else {
    img_best_main <- img_best_mon <- NULL
  }
  
  img_prev_main <- img_prev_mon <- NULL
  if (!is.null(current_model)) {
    p_prev_main <- tempfile(fileext = ".png")
    p_prev_mon  <- tempfile(fileext = ".png")
    grDevices::png(p_prev_main, width = png_width, height = png_height); graphics::plot(current_model);      grDevices::dev.off()
    grDevices::png(p_prev_mon,  width = png_width, height = png_height); stats::monthplot(current_model);     grDevices::dev.off()
    img_prev_main <- htmltools::img(src = knitr::image_uri(p_prev_main), style = "max-width:100%;height:auto;border-radius:12px;")
    img_prev_mon  <- htmltools::img(src = knitr::image_uri(p_prev_mon),  style = "max-width:100%;height:auto;border-radius:12px;")
  }
  
  # 6) Narrative & tables (make TD/amp lines DNA-aware)
  if (no_sa) {
    qs_line  <- glue::glue(
      "Existence of seasonality (QS on original): ",
      "X-11 = {.flagP(best_r$QSori_p_x11, 0.10)}, ",
      "SEATS = {.flagP(best_r$QSori_p_seats, 0.10)} \u2192 overall = {.flagP(best_r$QSori_p, 0.10)}."
    )
    ids_line <- glue::glue("Identifiable seasonality (IDS): {best_r$IDS}.")
    m7_line  <- glue::glue(
      "M7 (X-11): { .num(best_r$M7,2) } \u2192 ",
      "{ ifelse(best_r$M7 < 0.9,'clear seasonality', ifelse(best_r$M7 < 1.05,'weak seasonality','no seasonality')) }."
    )
    lb_line  <- "Residual autocorrelation (Ljung-Box): n/a (no seasonal adjustment)."
  } else {
    qs_line  <- glue::glue(
      "Residual seasonality (QS on SA): ",
      "X-11 = {.flagP(best_r$QS_p_x11, 0.10)}, ",
      "SEATS = {.flagP(best_r$QS_p_seats, 0.10)} \u2192 overall = {.flagP(best_r$QS_p, 0.10)}."
    )
    ids_line <- glue::glue("Identifiable seasonality (IDS): {best_r$IDS}.")
    m7_line  <- glue::glue(
      "M7 (X-11): { .num(best_r$M7,2) } \u2192 ",
      "{ ifelse(best_r$M7 < 0.9,'clear seasonality', ifelse(best_r$M7 < 1.05,'weak seasonality','no seasonality')) }."
    )
    lb_line  <- glue::glue("Residual autocorrelation (Ljung-Box): {.flagP(best_r$LB_p, 0.05)}")
  }
  
  if (dna) {
    td_line  <- "Trading-day regressor: n/a (no seasonal adjustment performed)."
    amp_line <- "Seasonal amplitude/volatility reduction: n/a (no seasonal adjustment performed)."
  } else {
    td_line <- if (!best_has_td) {
      "Trading-day regressor: not included."
    } else {
      glue::glue(
        "Trading-day regressor: {htmltools::htmlEscape(best_td_name)}; ",
        "p = {.flagP(best_r$td_p, 0.05)}."
      )
    }
    amp_line <- glue::glue(
      "Seasonal amplitude: { .num(best_r$seasonal_amp_abs,2) } ",
      "({ .pct(best_r$seasonal_amp_pct,1) } of level). ",
      "Volatility reduction (SA vs raw): { .pct(best_r$vola_reduction_pct,1) }."
    )
  }
  
  dist_line <- if (!is.null(prev_sa)) {
    glue::glue(
      "Difference vs current SA (L1): { .num(best_r$dist_sa_L1,2) }. ",
      "{if (!is.na(best_r$corr_seas)) glue::glue('Seasonal factor correlation vs current: { .num(best_r$corr_seas,3) }.') else ''}"
    )
  } else {
    "No current model supplied; baseline distances omitted."
  }
  
  best_ol_txt <- if (nrow(best_outliers)) {
    paste(glue::glue("{best_outliers$type}{best_outliers$period} ({.num(best_outliers$coef,3)})"), collapse = ", ")
  } else "none detected."
  prev_ol_txt <- if (!is.null(current_model) && !is.null(prev_outliers) && nrow(prev_outliers)) {
    paste(glue::glue("{prev_outliers$type}{prev_outliers$period} ({.num(prev_outliers$coef,3)})"), collapse = ", ")
  } else if (!is.null(current_model)) "none detected." else NULL
  
  # --- Top candidates: best first, include incumbent (\u2B50) -------------------
  top_tbl_node <- .build_top_candidates_table(res, current_model = current_model, y = y, n = 10L)
  
  # ---- IDs for copy buttons --------------------------------------------------
  new_code_id  <- "cp-new-model"
  prev_code_id <- "cp-current-model"
  
  # 7) Build HTML -----------------------------------------------------------
  exist_card  <- .build_existence_card(res)
  engine_card <- .build_engine_choice_card(res)
  best_tf     <- .transform_label(best, fallback = res$transform)
  
  page <- htmltools::tagList(
    htmltools::tags$html(lang = "en",
                         htmltools::tags$head(
                           htmltools::tags$meta(charset="utf-8"),
                           htmltools::tags$title(title),
                           htmltools::tags$style(htmltools::HTML("
  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Inter, 'Helvetica Neue', Arial, sans-serif; color:#111827; margin:0; background:#f7f7fb;}
  .wrap { max-width: 1100px; margin: 40px auto; padding: 0 20px; }
  h1 { font-size: 28px; margin: 0 0 12px; }
  .sub { color:#6b7280; margin-bottom:24px; }
  .card { background:white; border-radius:16px; padding:22px; box-shadow: 0 1px 3px rgba(0,0,0,0.08); margin-bottom:18px;}
  .grid { display:grid; grid-template-columns: 1fr 1fr; gap:14px; }
  .nowrap { white-space: nowrap; } 
  .mono { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace; font-size: 12px; white-space: pre-wrap; background:#0f172a; color:#e5e7eb; padding:14px; border-radius:12px; }
  .pill { display:inline-block; font-size:12px; padding:4px 10px; border-radius:999px; background:#eef2ff; color:#3730a3; margin-left:8px;}
  .kv { display:flex; flex-wrap:wrap; gap:10px 18px; font-size:14px; }
  .kv div { background:#f3f4f6; padding:8px 10px; border-radius:10px; }
  table.tbl { border-collapse:collapse; width:100%; }
  table.tbl th, table.tbl td { padding:8px 10px; border-bottom:1px solid #eee; font-size: 13px; text-align: right;}
  table.tbl th:first-child, table.tbl td:first-child { text-align:left; }
  table.tbl td:nth-child(2),
  table.tbl th:nth-child(2) { min-width: 110px; }  

/* colored rows in 'Top candidates' */
.tbl-colored tr.row-best    { background: #ecfdf5; }  /* green tint */
.tbl-colored tr.row-prev    { background: #eff6ff; }  /* blue tint */
.tbl-colored tr.row-airline { color:#b91c1c; font-weight:600; }

/* small legend chips */
.legend { margin-top:10px; display:flex; gap:10px; flex-wrap:wrap; }
.chip { padding:4px 10px; border-radius:999px; font-size:12px; border:1px solid #e5e7eb; }
.chip-best { background:#ecfdf5; color:#065f46; border-color:#a7f3d0; }
.chip-prev { background:#eff6ff; color:#1e40af; border-color:#bfdbfe; }
.tbl-note { color:#6b7280; margin-top:6px; }

/* copy button */
.copy-wrap { position: relative; }
.copy-btn {
  position: absolute; top: 8px; right: 8px;
  padding: 6px 10px; border: 1px solid #ddd; border-radius: 6px;
  background: #f7f7f7; cursor: pointer; font-size: 12px;
}
.copy-btn:hover { background: #eee; }
.copy-btn.copied { background: #e6ffed; border-color: #a4e8b1; }
        ")),
                           htmltools::tags$script(htmltools::HTML("
document.addEventListener('click', function(e){
  const btn = e.target.closest('.copy-btn');
  if (!btn) return;
  const sel = btn.getAttribute('data-target');
  const node = document.querySelector(sel);
  if (!node) return;
  const text = node.innerText;

  function done(b){
    const old = b.textContent;
    b.textContent = 'Copied!';
    b.classList.add('copied');
    setTimeout(()=>{ b.textContent = old; b.classList.remove('copied'); }, 1200);
  }
  function fallbackCopy(t, b){
    const ta = document.createElement('textarea');
    ta.value = t; ta.style.position = 'fixed'; ta.style.top = '-1000px';
    document.body.appendChild(ta); ta.focus(); ta.select();
    try { document.execCommand('copy'); done(b); } catch(err) { console.error(err); }
    document.body.removeChild(ta);
  }
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text).then(()=> done(btn)).catch(()=> fallbackCopy(text, btn));
  } else {
    fallbackCopy(text, btn);
  }
});
        "))
                         ),
                         htmltools::tags$body(
                           htmltools::div(class="wrap",
                                          # Title card
                                          htmltools::div(class="card",
                                                         htmltools::tags$h1(title),
                                                         htmltools::div(class="sub",
                                                                        paste0("Frequency: ", res$frequency, " \u2022 Transform: ", best_tf))
                                          ),
                                          
                                          .build_existence_card(res),
                                          htmltools::div(class="card",
                                                         htmltools::tags$h2("Summary"),
                                                         htmltools::div(class="kv",
                                                                        htmltools::div(htmltools::HTML(paste0("<b>Seasonality (robust)</b>: <span class='pill'>", res$seasonality$overall$call_overall[1], "</span>"))),
                                                                        if (!is.null(decision_reason)) htmltools::div(htmltools::HTML(paste0("<b>Reason</b>: ", decision_reason))),
                                                                        htmltools::HTML(paste0("<b>Best candidate (by score)</b>: ", if (no_sa) "n/a" else sprintf("<span class='mono nowrap'>%s</span>", htmltools::htmlEscape(best_arima_disp)))),
                                                                        htmltools::div(htmltools::HTML(paste0("<b>TD</b>: ",         if (no_sa) "n/a" else ifelse(best_r$with_td,"yes","no")))),
                                                                        htmltools::div(htmltools::HTML(paste0("<b>AICc</b>: ",       if (no_sa) "n/a" else .num(best_r$AICc,2)))),
                                                                        htmltools::div(htmltools::HTML(paste0("<b>Ljung-Box p</b>: ", if (no_sa) "n/a" else .flagP(best_r$LB_p, 0.05)))),
                                                                        htmltools::div(htmltools::HTML(paste0("<b>Engine</b>: ",     if (no_sa) "n/a" else best_engine))),
                                                                        htmltools::div(htmltools::HTML(paste0("<b>Decision</b>: <span class='pill'>", decision_txt, "</span>")))
                                                                        
                                                         )
                                          ),
                                          
                                          htmltools::div(
                                            class="card",
                                            htmltools::tags$h2("Why this model?"),
                                            .build_selection_rationale(res, current_model, override_best_arima = best_arima_disp)
                                          ),
                                          
                                          .build_engine_choice_card(res),
                                          
                                          if (isTRUE(render_alt) && !is.null(current_model)) htmltools::div(class="card",
                                                                                                            htmltools::tags$h2("Comparison \u2014 New vs current"),
                                                                                                            htmltools::tags$h3("Seasonally Adjusted Levels"),
                                                                                                            img_cmp_sa,
                                                                                                            htmltools::tags$h3("SA Growth (percent change)"),
                                                                                                            img_cmp_pc,
                                                                                                            htmltools::tags$h3("Descriptive Statistics (SA Growth)"),
                                                                                                            stats_tbl_html
                                          ),
                                          
                                          # Copy-paste blocks (NEW) \u2014 with Copy buttons
                                          if (!dna) htmltools::div(
                                            class="card",
                                            htmltools::tags$h2("Copy-paste model (New)"),
                                            htmltools::div(
                                              class = "copy-wrap",
                                              htmltools::tags$button(
                                                class = "copy-btn",
                                                `data-target` = paste0("#", new_code_id),
                                                title = "Copy to clipboard", "Copy"
                                              ),
                                              htmltools::tags$pre(
                                                id = new_code_id,
                                                class = "mono",
                                                htmltools::HTML(htmltools::htmlEscape(best_spec_code))
                                              )
                                            )
                                          ) else htmltools::div(
                                            class="card",
                                            htmltools::tags$h2("Copy-paste model (New)"),
                                            htmltools::tags$p("Tests indicate no material seasonality. No new seasonal adjustment is proposed."),
                                            htmltools::div(
                                              class = "copy-wrap",
                                              htmltools::tags$button(
                                                class = "copy-btn",
                                                `data-target` = paste0("#", new_code_id),
                                                title = "Copy to clipboard", "Copy"
                                              ),
                                              htmltools::tags$pre(
                                                id = new_code_id,
                                                class = "mono",
                                                htmltools::HTML(htmltools::htmlEscape(best_spec_code))
                                              )
                                            )
                                          ),
                                          
                                          if (!is.null(prev_spec_code)) htmltools::div(
                                            class="card",
                                            htmltools::tags$h2("Copy-paste model (current)"),
                                            htmltools::div(
                                              class = "copy-wrap",
                                              htmltools::tags$button(
                                                class = "copy-btn",
                                                `data-target` = paste0("#", prev_code_id),
                                                title = "Copy to clipboard", "Copy"
                                              ),
                                              htmltools::tags$pre(
                                                id = prev_code_id,
                                                class = "mono",
                                                htmltools::HTML(htmltools::htmlEscape(prev_spec_code))
                                              )
                                            )
                                          ),
                                          
                                          htmltools::div(
                                            class = "card",
                                            htmltools::tags$h2("Diagnostics (best model)"),
                                            htmltools::tags$p(htmltools::HTML(qs_line)),
                                            htmltools::tags$p(ids_line),
                                            htmltools::tags$p(m7_line),
                                            htmltools::tags$p(htmltools::HTML(lb_line)),
                                            htmltools::tags$p(htmltools::HTML(td_line)),
                                            htmltools::tags$p(htmltools::HTML(ol_rule_line)),
                                            htmltools::tags$p(amp_line),
                                            htmltools::tags$p(dist_line)
                                          ),
                                          
                                          htmltools::div(class="card",
                                                         htmltools::tags$h2("Plots \u2014 Best model"),
                                                         htmltools::div(class="grid", img_best_main, img_best_mon)
                                          ),
                                          
                                          if (!is.null(current_model)) htmltools::div(class="card",
                                                                                      htmltools::tags$h2("Plots \u2014 current model"),
                                                                                      htmltools::div(class="grid", img_prev_main, img_prev_mon)
                                          ),
                                          
                                          if (isTRUE(render_alt) && !is.null(current_model)) htmltools::div(class="card",
                                                                                                            htmltools::tags$h2("Model coefficients (Alt vs Neu)"),
                                                                                                            .build_compare_html(current_model, best)
                                          ),
                                          
                                          htmltools::div(class="card",
                                                         htmltools::tags$h2("Outliers"),
                                                         htmltools::tags$p(htmltools::HTML(ol_rule_line)),
                                                         htmltools::tags$p(htmltools::HTML(paste0("<b>Best model</b>: ", best_ol_txt))),
                                                         if (!is.null(prev_ol_txt)) htmltools::tags$p(htmltools::HTML(paste0("<b>Previous model</b>: ", prev_ol_txt)))
                                          ),
                                          
                                          htmltools::div(class="card", htmltools::tags$h2("Interpretation of results"), results_para),
                                          
                                          htmltools::div(class="card",
                                                         htmltools::tags$h2("Top candidates (head)"),
                                                         top_tbl_node,
                                                         htmltools::tags$p(class="sub",
                                                                           "This table lists the highest-ranked specifications by the composite score used in the analysis. ",
                                                                           "Columns include: AICc; Ljung-Box p (residual autocorrelation); QS p-values from X-11 and SEATS; ",
                                                                           "overall QS (the minimum of those two); trading-day p (if a TD regressor is present); ",
                                                                           "volatility reduction of SA vs. original (based on percent-change standard deviations); ",
                                                                           "seasonal amplitude as % of the original level; L1 distance of the new SA vs. the current SA (if available); ",
                                                                           "and mean absolute history-change revision (Rev. MAE)."
                                                         )
                                          )
                           )
                         )
    )
  )
  
  htmltools::save_html(page, file = outfile)
  message("Report written: ", normalizePath(outfile))
  invisible(list(report = outfile, res = res))
}


#' Seasonal Adjustment: full HTML report
#'
#' User-facing wrapper that runs the automatic seasonal analysis and writes a
#' human-readable HTML report with plots, tables, diagnostics, and
#' copy-pasteable `seas()` calls.
#'
#' Notes:
#' - If the provided *current* specification equals the selected *best* model,
#'   the report omits the "Alternative model" comparison section.
#' - The "Top candidates" table starts with the best model and always includes
#'   the current model (flagged with a star ★) when one is supplied.
#'
#' @param y Time series to be analysed. Can be a `ts` object or anything that
#'   `tsbox::ts_ts()` can convert to `ts`.
#' @param current_model Optional incumbent [seasonal::seas()] model to compare
#'   against the newly selected specification.
#' @param td_usertype Character string passed as `regression.usertype` when
#'   trading-day regressors are used (default `"td"`).
#' @param td_candidates Optional named list of trading-day candidate regressors
#'   (e.g. `list(wd = wd.m, wd1 = wd1.m)`), each aligned with `y`.
#' @param use_fivebest Logical. If `TRUE`, include the "five best" automatic
#'   specs in the candidate grid.
#' @param title Title of the report shown in the HTML page.
#' @param outfile Path to the HTML file to be written.
#' @param png_width,png_height Width and height (in pixels) of PNG plots
#'   embedded in the report.
#' @param print_to_console Logical. If `TRUE`, do not create an HTML file but
#'   print the copy-pasteable model code to the console and return it.
#' @param print_which Which code blocks to print when `print_to_console = TRUE`.
#'   One of `"new"`, `"current"` or `"both"`.
#' @param include_easter Controls inclusion of Easter regressors:
#'   `"auto"` (default) lets the selector decide, `"always"` always includes
#'   Easter, `"off"` never includes Easter. A logical value is also accepted
#'   and mapped to `"auto"`/`"off"`.
#' @param easter_len Integer, length (in days) of the Easter effect when
#'   included.
#' @param engine Preferred decomposition engine for candidate models.
#'   One of `"seats"`, `"x11"` or `"auto"`.
#' @param w_engine Numeric weight for the engine choice component in the
#'   composite ranking score.
#' @param outlier_types Character vector of outlier types to detect, typically
#'   a subset of `c("AO", "LS", "TC")`.
#' @param outlier_method Character scalar giving the outlier detection method
#'   passed to `seasonal::seas()` (e.g. `"AddOne"`).
#' @param outlier_critical Numeric critical value for outlier detection
#'   (e.g. a t-/z-threshold).
#' @param outlier_alpha Optional numeric significance level; if supplied, it is
#'   converted to a two-sided normal cutoff for `outlier_critical`.
#'
#' @return Invisibly, a list with elements `report` (path to the HTML file) and
#'   `res` (the corresponding [auto_seasonal_analysis()] result).
#' @examplesIf requireNamespace("seasonal", quietly = TRUE)
#' # sa_report_html(y = AirPassengers, outfile = tempfile(fileext = ".html"))
#' @seealso [auto_seasonal_analysis()], [sa_existence_card()], [sa_top_candidates_table()]
#' @export
sa_report_html <- function(
    y,
    current_model = NULL,
    td_usertype = "td",
    td_candidates = NULL,
    use_fivebest = TRUE,
    title = "Seasonal Adjustment Report",
    outfile = "sa_report.html",
    png_width = 1400, png_height = 900,
    print_to_console = FALSE,
    print_which = c("new", "current", "both"),
    include_easter = c("auto", "always", "off"),
    easter_len = 15L,
    engine = c("seats", "x11", "auto"),
    w_engine = 1,
    outlier_types    = c("AO","LS","TC"),
    outlier_method   = "AddOne",
    outlier_critical = 4,
    outlier_alpha    = NULL
) {
  sa_issue_report_html(
    y = y,
    current_model = current_model,
    td_usertype   = td_usertype,
    td_candidates = td_candidates,
    use_fivebest  = use_fivebest,
    title         = title,
    outfile       = outfile,
    png_width     = png_width,
    png_height    = png_height,
    print_to_console = print_to_console,
    print_which      = match.arg(print_which),
    include_easter   = include_easter,
    easter_len       = easter_len,
    engine           = match.arg(engine),
    w_engine         = w_engine,
    outlier_types    = outlier_types,
    outlier_method   = outlier_method,
    outlier_critical = outlier_critical,
    outlier_alpha    = outlier_alpha
  )
}


#' Top candidates table (HTML tag)
#'
#' Build a colour-coded HTML table of the highest-ranked candidate models,
#' always placing the **best** model at the top and **including the current
#' model** (if supplied) even if it is not in the top `n`. The airline
#' reference model ARIMA (0 1 1)(0 1 1) is also appended if absent.
#'
#' Rows are lightly shaded: ✓ best (green), ★ current (blue), airline (red).
#'
#' @param res Result of [auto_seasonal_analysis()].
#' @param current_model Optional fitted [seasonal::seas] model to mark as "current".
#' @param y Optional original series (needed to compute a few current-model diagnostics).
#' @param n Number of top rows to display (default 5). The airline/current rows
#'   may be appended even if not in the top `n`.
#' @return An `htmltools` tag (`<table>`) you can insert into reports.
#' @export
sa_top_candidates_table <- function(res, current_model = NULL, y = NULL, n = 5) {
  .build_top_candidates_table(res, current_model = current_model, y = y, n = n)
}

#' Internal: build Top Candidates HTML table
#' @keywords internal
#' @noRd
.build_top_candidates_table <- function(res, current_model = NULL, y = NULL, n = 5) {
  .num   <- function(x, d = 3) ifelse(is.na(x), "\u2014", sprintf(paste0("%.", d, "f"), x))
  .fmtP  <- function(p) ifelse(is.na(p), "\u2014", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  html_escape <- function(x) htmltools::htmlEscape(x)
  normalize <- function(s) gsub("\\s+", " ", trimws(ifelse(is.na(s), "", as.character(s))))
  
  airline_arima <- "(0 1 1)(0 1 1)"
  stopifnot(is.data.frame(res$table), nrow(res$table) >= 1)
  
  # Always work from a coalesced table so we have the widest set of columns
  tbl <- tryCatch(.coalesce_arima_cols(res$table), error = function(e) res$table)
  
  # ---------- NEW: build a robust ARIMA display string per row ----------
  .arima_from_row <- function(row){
    pick <- function(nm) if (nm %in% names(row)) row[[nm]][1] else NA
    
    # 1) direct strings if present
    cand <- c(pick("ARIMA_disp"), pick("arima"))
    cand <- cand[is.character(cand) & !is.na(cand) & nzchar(cand)]
    if (length(cand)) return(normalize(cand[1]))
    
    # 2) numeric columns (common names)
    gi <- function(nm) suppressWarnings(as.integer(pick(nm)))
    v1 <- c(gi("p"),gi("d"),gi("q"),gi("P"),gi("D"),gi("Q"))
    if (all(is.finite(v1))) return(sprintf("(%d %d %d)(%d %d %d)", v1[1],v1[2],v1[3],v1[4],v1[5],v1[6]))
    
    # 3) other numeric naming schemes seen in some pipelines
    v2 <- c(gi("arima_p"),gi("arima_d"),gi("arima_q"),gi("sarima_p"),gi("sarima_d"),gi("sarima_q"))
    if (all(is.finite(v2))) return(sprintf("(%d %d %d)(%d %d %d)", v2[1],v2[2],v2[3],v2[4],v2[5],v2[6]))
    
    # 4) parse from id-like fields
    for (nm in c("spec_id","model_label","id","label")) {
      s <- pick(nm)
      if (is.character(s) && !is.na(s) && nzchar(s)) {
        m <- regmatches(s, regexpr("\\(\\s*\\d+\\s+\\d+\\s+\\d+\\s*\\)\\(\\s*\\d+\\s+\\d+\\s+\\d+\\s*\\)", s, perl = TRUE))
        if (!identical(m, character(0))) return(normalize(m[1]))
      }
    }
    "n/a"
  }
  # compute safe arima strings for *all* rows once
  tbl$arima <- vapply(seq_len(nrow(tbl)), function(i) .arima_from_row(tbl[i, , drop = FALSE]), character(1))
  # ----------------------------------------------------------------------
  
  disp_cols <- c(
    "model_label","arima","with_td","AICc","LB_p",
    "QSori_p","QS_p_x11","QS_p_seats","QS_p",
    "td_p","vola_reduction_pct","seasonal_amp_pct","dist_sa_L1","rev_mae"
  )
  
  # Base: top-n (already ranked best-first)
  top <- tbl |>
    dplyr::select(dplyr::any_of(disp_cols)) |>
    dplyr::slice(1:min(n, nrow(tbl)))
  
  # Add airline row if available and not already included
  if (!any(normalize(top$arima) == normalize(airline_arima), na.rm = TRUE)) {
    air_full <- dplyr::filter(tbl, normalize(.data$arima) == normalize(airline_arima))
    if (nrow(air_full)) {
      air_row <- air_full |>
        dplyr::select(dplyr::any_of(disp_cols)) |>
        dplyr::slice(1)
      top <- dplyr::bind_rows(top, air_row)
    }
  }
  
  # Add 'current' row (with diagnostics) if not already there
  prev_arima <- NULL
  if (!is.null(current_model)) {
    prev_arima <- tryCatch(.arima_string(current_model), error = function(e) NULL)
    if (!is.null(prev_arima) && !any(normalize(top$arima) == normalize(prev_arima), na.rm = TRUE)) {
      qb  <- tryCatch(.qs_on_sa_both(current_model),
                      error = function(e) tibble::tibble(QS_p_x11 = NA_real_, QS_p_seats = NA_real_, QS_p = NA_real_))
      qo  <- tryCatch(.qs_original(current_model),
                      error = function(e) tibble::tibble(QSori_p = NA_real_))
      LBp <- tryCatch(.lb_p(current_model),  error = function(e) NA_real_)
      AICc<- tryCatch(.aicc(current_model),  error = function(e) NA_real_)
      
      rp <- tryCatch(.reg_pvals(current_model), error = function(e) tibble::tibble(var = character(), p = numeric()))
      is_td_row <- if (nrow(rp)) grepl("xreg|wd|weekday|trading|td1coef|td", rp$var, ignore.case = TRUE) & !.is_arima_term(rp$var) else rep(FALSE, 0)
      has_td <- any(is_td_row)
      td_p   <- if (has_td) suppressWarnings(min(rp$p[is_td_row], na.rm = TRUE)) else NA_real_
      
      fin <- if (!is.null(y)) tryCatch(.diagnostics_finance(current_model, y), error = function(e) NULL) else NULL
      vol_red <- if (!is.null(fin) && "vola_reduction_pct" %in% names(fin)) fin$vola_reduction_pct[1] else NA_real_
      seas_amp<- if (!is.null(fin) && "seasonal_amp_pct"   %in% names(fin)) fin$seasonal_amp_pct[1]   else NA_real_
      
      prev_row <- tibble::tibble(
        model_label = "current",
        arima       = normalize(prev_arima),
        with_td     = has_td,
        AICc        = AICc,
        LB_p        = LBp
      ) |>
        dplyr::bind_cols(qo, qb) |>
        dplyr::mutate(
          td_p = td_p,
          vola_reduction_pct = vol_red,
          seasonal_amp_pct   = seas_amp,
          dist_sa_L1 = NA_real_,
          rev_mae    = NA_real_
        ) |>
        dplyr::select(dplyr::any_of(disp_cols))
      
      top <- dplyr::bind_rows(top, prev_row)
    }
  }
  
  # Row flags/classes \u2014 ALWAYS length nrow(top)
  n_top <- nrow(top)
  best_label <- if ("model_label" %in% names(tbl)) tbl$model_label[1] else NA_character_
  
  if ("model_label" %in% names(top) && !is.na(best_label)) {
    top$is_best <- top$model_label == best_label
  } else {
    top$is_best <- seq_len(n_top) == 1L
  }
  has_prev_label <- if ("model_label" %in% names(top)) top$model_label == "current" else rep(FALSE, n_top)
  prev_match     <- if (!is.null(prev_arima)) (normalize(top$arima) == normalize(prev_arima)) else rep(FALSE, n_top)
  top$is_prev    <- has_prev_label | prev_match
  top$is_airline <- normalize(top$arima) == normalize(airline_arima)
  
  # ---- Render table ----
  th <- c("Label","ARIMA","TD","AICc","LB p",
          "QSori p","QS X-11 p","QS SEATS p","QS (min)","TD p",
          "Volatility \u2193 %","Seasonal amp %","L1 vs prev SA","Rev MAE")
  header <- htmltools::tags$tr(lapply(th, htmltools::tags$th))
  
  body_rows <- lapply(seq_len(n_top), function(i) {
    r <- top[i,]
    row_class <- if (isTRUE(r$is_best)) "row-best"
    else if (isTRUE(r$is_prev)) "row-prev"
    else if (isTRUE(r$is_airline)) "row-airline"
    else ""
    
    htmltools::tags$tr(
      class = row_class,
      htmltools::tags$td(html_escape(r$model_label)),
      htmltools::tags$td(htmltools::tags$span(style = "white-space:nowrap;", html_escape(r$arima))),
      htmltools::tags$td(ifelse(isTRUE(r$with_td), "yes", "no")),
      htmltools::tags$td(.num(r$AICc, 2)),
      htmltools::tags$td(.fmtP(r$LB_p)),
      htmltools::tags$td(.fmtP(r$QSori_p)),
      htmltools::tags$td(.fmtP(r$QS_p_x11)),
      htmltools::tags$td(.fmtP(r$QS_p_seats)),
      htmltools::tags$td(.fmtP(r$QS_p)),
      htmltools::tags$td(.fmtP(r$td_p)),
      htmltools::tags$td(.num(r$vola_reduction_pct, 1)),
      htmltools::tags$td(.num(r$seasonal_amp_pct, 2)),
      htmltools::tags$td(.num(r$dist_sa_L1, 2)),
      htmltools::tags$td(.num(r$rev_mae, 3))
    )
  })
  
  table_tag <- htmltools::tags$table(
    class = "tbl tbl-colored",
    htmltools::tags$thead(header),
    htmltools::tags$tbody(body_rows)
  )
  
  legend <- htmltools::div(
    class = "legend",
    htmltools::span(class="chip chip-best",    "\u2705 best"),
    htmltools::span(class="chip chip-prev",    "\u2B50 current"),
    htmltools::span(class="chip chip-airline", "Airline model")
  )
  
  styles <- htmltools::tags$style(htmltools::HTML(
    ".tbl-colored .row-airline td { background:#fff1f2; }
     .chip-airline{ background:#fee2e2 !important; color:#991b1b !important; border:1px solid #fecaca !important; }
     .tbl-colored th, .tbl-colored td { vertical-align: middle; }
     .tbl-colored th:nth-child(2), .tbl-colored td:nth-child(2){ white-space:nowrap; min-width:200px; }"
  ))
  
  foot <- htmltools::tags$p(
    class = "tbl-note",
    htmltools::HTML(
      "Note: <span class='chip chip-airline'>Airline model</span> is ARIMA (0 1 1)(0 1 1). ",
      "The <span class='chip chip-prev'>current model</span> is added even if it was not in the candidate grid; ",
      "distances vs. current SA and revision metrics are not applicable for that row."
    )
  )
  
  htmltools::tagList(styles, table_tag, legend, foot)
}




# ---- Narrative helpers -------------------------------------------------------

.build_selection_rationale <- function(res, current_model = NULL, override_best_arima = NULL) {
  # locals
  get1 <- function(row, nm, default = NA) if (nm %in% names(row)) row[[nm]][1] else default
  P    <- function(x) .fmtP(ifelse(is.null(x), NA_real_, x))
  N    <- function(x, d = 2) .num(ifelse(is.null(x), NA_real_, x), d)
  
  tbl <- tryCatch(.coalesce_arima_cols(res$table), error = function(e) res$table)
  br  <- dplyr::slice(tbl, 1)
  sr  <- if (nrow(tbl) >= 2) dplyr::slice(tbl, 2) else NULL
  
  arima_best <- if (!is.null(override_best_arima) && nzchar(override_best_arima)) {
    override_best_arima
  } else {
    .sanitize_arima(.coalesce_arima_str(br))
  }
  
  with_td <- isTRUE(get1(br, "with_td", default = FALSE))
  
  prev_arima <- if (!is.null(current_model)) tryCatch(.arima_string(current_model), error = function(e) NULL) else NULL
  prev_is_best <- FALSE
  if ("is_current" %in% names(tbl)) {
    prev_is_best <- isTRUE(br$is_current[1])
  } else if (!is.null(prev_arima)) {
    prev_is_best <- isTRUE(get1(br, "arima") == prev_arima || get1(br, "ARIMA_disp") == prev_arima)
  }
  
  gap_score <- if (!is.null(sr)) get1(sr, "score") - get1(br, "score") else NA_real_
  gap_aicc  <- if (!is.null(sr)) get1(sr, "AICc")  - get1(br, "AICc")  else NA_real_
  
  qs_x11   <- get1(br, "QS_p_x11")
  qs_seats <- get1(br, "QS_p_seats")
  qs_min   <- get1(br, "QS_p")
  lb_p     <- get1(br, "LB_p")
  vola_red <- get1(br, "vola_reduction_pct")
  seas_amp <- get1(br, "seasonal_amp_pct")
  
  # >>> The runner-up ARIMA \u2014 robust row-wise pick <<<
  ru_arima   <- if (!is.null(sr)) .sanitize_arima(.coalesce_arima_str(sr)) else NULL
  ru_with_td <- if (!is.null(sr)) isTRUE(get1(sr, "with_td", default = FALSE)) else NULL
  
  htmltools::tagList(
    htmltools::tags$p(
      htmltools::HTML(
        paste0(
          "<b>Selection rationale.</b> The chosen specification is <code>ARIMA ",
          htmltools::htmlEscape(arima_best), "</code> ",
          if (with_td) "with trading-day (TD) regressor" else "without TD",
          ", selected because it achieved the <b>lowest composite score</b> among candidates",
          if (!is.na(gap_score)) paste0(" (margin vs. next best: ", sprintf("%.3f", gap_score), ", lower is better)") else "",
          ". The composite score prioritises residual seasonality (QS), residual autocorrelation (Ljung-Box), model parsimony (AICc), ",
          "stability under history extension (revision MAE), and proximity to the incumbent seasonal adjustment (L1 distance)."
        )
      )
    ),
    htmltools::tags$p(
      htmltools::HTML(
        paste0(
          "Diagnostics for the winner: QS(X-11) p = ", P(qs_x11),
          ", QS(SEATS) p = ", P(qs_seats),
          " \u2192 overall QS = ", P(qs_min), "; Ljung-Box p = ", P(lb_p),
          "; volatility reduction (SA vs. original) = ", N(vola_red, 1), "%; seasonal amplitude = ",
          N(seas_amp, 2), "%."
        )
      )
    ),
    if (!is.null(sr)) htmltools::tags$p(
      htmltools::HTML(
        paste0(
          "Against the runner-up (<code>ARIMA ", htmltools::htmlEscape(ru_arima), "</code> ",
          if (isTRUE(ru_with_td)) "with TD" else "without TD", "), the winning model ",
          if (!is.na(gap_aicc)) paste0("also improves AICc (\u0394AICc = ", sprintf("%.2f", gap_aicc), "); ") else "",
          "and achieves a lower revision MAE (where available)."
        )
      )
    ) else NULL,
    if (!is.null(current_model)) htmltools::tags$p(
      htmltools::HTML(
        paste0(
          "Relative to the <b>current model</b>",
          if (!is.null(prev_arima)) paste0(" (<code>ARIMA ", htmltools::htmlEscape(prev_arima), "</code>)") else "",
          ", the L1 distance of SA levels for the new candidate is ",
          N(get1(br, "dist_sa_L1"), 2),
          if (is.finite(get1(br, "corr_seas"))) paste0("; correlation of seasonal factors \u2248 ", N(get1(br, "corr_seas"), 3)) else "",
          if (isTRUE(prev_is_best)) " \u2014 the current specification already matches the selected best, so the alternative comparison section is omitted." else "."
        )
      )
    ) else NULL
  )
}

.build_results_commentary <- function(prev_sa, best_sa, best_outliers, prev_outliers = NULL) {
  parts <- list()
  
  if (!is.null(prev_sa) && !is.null(best_sa)) {
    g_prev <- tsbox::ts_pc(prev_sa); g_new <- tsbox::ts_pc(best_sa)
    s_prev <- .sa_pc_stats(g_prev);   s_new <- .sa_pc_stats(g_new)
    
    parts <- c(parts, list(
      htmltools::tags$p(
        htmltools::HTML(
          paste0(
            "<b>SA growth comparison.</b> Mean pct-change: new = ", sprintf("%.3f", s_new["mean"]),
            ", current = ", sprintf("%.3f", s_prev["mean"]),
            "; standard deviation: new = ", sprintf("%.3f", s_new["sd"]),
            ", current = ", sprintf("%.3f", s_prev["sd"]), ". ",
            "Overall, the two SA series track each other closely in levels (see panel), ",
            "while growth volatility is ", ifelse(s_new["sd"] <= s_prev["sd"], "lower", "higher"),
            " under the new model."
          )
        )
      )
    ))
  }
  
  ol_new  <- if (nrow(best_outliers)) paste0(nrow(best_outliers), " (", paste(best_outliers$type, collapse = ", "), ")") else "none"
  ol_prev <- if (!is.null(prev_outliers)) if (nrow(prev_outliers)) paste0(nrow(prev_outliers), " (", paste(prev_outliers$type, collapse = ", "), ")") else "none" else NULL
  
  parts <- c(parts, list(
    htmltools::tags$p(
      htmltools::HTML(
        paste0("<b>Outliers.</b> New model: ", ol_new,
               if (!is.null(ol_prev)) paste0("; current model: ", ol_prev) else "", ".")
      )
    )
  ))
  
  htmltools::tagList(parts)
}

# return a small HTML table (as a string) with stats for prev vs new
.build_growth_stats_table <- function(g_prev, g_new) {
  s_prev <- .sa_pc_stats(g_prev)
  s_new  <- .sa_pc_stats(g_new)
  df <- tibble::tibble(
    Metric   = c("Mean", "Std. Dev.", "Median", "Min", "Max"),
    current  = sprintf("%.3f", s_prev[c("mean","sd","median","min","max")]),
    New      = sprintf("%.3f", s_new [c("mean","sd","median","min","max")])
  )
  htmltools::HTML(knitr::kable(df, format = "html", table.attr = 'class="tbl"'))
}
