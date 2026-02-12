test_that("SEATS seasonal component is NA for X-11 models (not 'absent')", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("seasonal")
  
  m_x11 <- seasonal::seas(AirPassengers, x11 = "")
  expect_true(is.na(seasight:::.seats_has_seasonal(m_x11)))
})
