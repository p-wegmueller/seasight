test_that(".has_seats_model_switch_msg never crashes on broken seas objects", {
  m <- structure(list(), class = "seas")
  expect_silent({
    z <- seasight:::.has_seats_model_switch_msg(m)
    expect_true(is.na(z) || is.logical(z))
  })
})

test_that("QS helpers never crash on broken seas objects", {
  m <- structure(list(), class = "seas")
  expect_silent({
    a <- seasight:::.qs_on_sa_both(m)
    b <- seasight:::.qs_original(m)
    expect_true(is.data.frame(a) && nrow(a) == 1)
    expect_true(is.data.frame(b) && nrow(b) == 1)
  })
})
