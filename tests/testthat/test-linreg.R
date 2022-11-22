test_that("SLR works", {
  expect_equal(linreg(rep(0,10), 1:10, mtcars), c(0, 0))
})
