test_that("SLR works", {
  expect_equal(linreg(am, mpg, mtcars), c(-0.59149275, 0.04966211))
})
