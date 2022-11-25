test_that("linreg works", {
  expect_equal(as.vector(as.matrix(linreg("hp",
                                          list(cyl = "cyl"),
                                          mtcars))),
               as.vector(summary(lm(hp ~ cyl,
                                    data = mtcars))$coefficients))

  expect_equal(as.vector(as.matrix(linreg("notcar",
                                          cyl,
                                          mtcars))),
               "y is not found in dataset")

  expect_equal(as.vector(as.matrix(linreg(mtcars$hp,
                                          list(cyl = mtcars$cyl)))),
               as.vector(summary(lm(hp ~ cyl,
                                    data = mtcars))$coefficients))

  expect_equal(as.vector(as.matrix(linreg(mtcars$hp,
                                          mtcars$cyl))),
               as.vector(summary(lm(mtcars$hp ~ mtcars$cyl))$coefficients))
})
