#' @title Linear Regression
#' @description Runs linear regression model
#' @param y outcome or dependent variable
#' @param x matrix of covariates or independent variables
#' @param dataset dataset containing y and x
#' @return a table of results
#' @examples
#' linreg(am, mpg, mtcars)
#' @export

linreg = function(y, x, dataset) {
#  Y = dataset[[substitute(y)]]
#  X = cbind(1, dataset[[substitute(x)]])
  Y = y
  X = cbind(1, x)
  beta = solve(t(X)%*%X)%*%t(X)%*%Y
  return(as.vector(beta))
}

