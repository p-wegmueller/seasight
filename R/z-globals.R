utils::globalVariables(c(
  # existing columns
  "score", "model_label", "with_td", "AICc", "LB_p",
  "QS_p", "QS_p_x11", "QS_p_seats",
  "QSori_p", "QSori_p_x11", "QSori_p_seats",
  "rev_mae", "dist_sa_L1", "dist_seas_RMS",
  "corr_seas", "seasonal_amp_pct", "vola_reduction_pct",
  "IDS", "M7", "SEATS_model_switch",
  
  # dplyr / tidyeval helpers
  ".data",
  
  # ranking / ID flags
  "score_rank", "spec_id",
  "is_best", "is_incumbent",
  "seasonal_ids", "seasonal_qsori", "seasonal_m7s",
  
  # columns used in top-candidates / report tables
  "Best", "Incumbent", "Model", "TD",
  "QS_X11", "QS_SEATS", "QS_overall",
  "Amp_pct", "Vola_red", "Dist_L1",
  "QS_SA_x11", "QS_SA_seats", "QS_SA_min",
  "QSori_min", "QSori_seats", "QSori_x11"
))
