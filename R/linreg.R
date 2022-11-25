#' @title Linear Regression
#' @description This functions fits a linear regression model and provides parameter estimates and significance.
#' @param y a numeric vector or name of a column vector in the specified dataset
#' @param x a list of one or more numeric vectors or names of a column vector in the specified dataset
#' @param dataset (optional) a dataset containing the column vectors listed
#' @return The function outputs a table of beta estimates, SE, t statistics, and p values.
#' @examples
#' linreg(mtcars$mpg, list(cyl = mtcars$cyl, disp = mtcars$disp))
#' linreg("mpg", list(cyl = "cyl", disp = mtcars$dis), mtcars)
#' @importFrom stats pt
#' @importFrom bench mark
#' @export

linreg = function(y, x, dataset=NULL) {
  # look for y in dataset, otherwise in environment
  if(!is.null(dataset) && is.character(y)) {
    if (y %in% colnames(dataset)) {
      Y = dataset[[y]]
    } else {
      return ("y is not found in dataset")
    }
  } else {
    Y = y
  }

  # look for x in dataset, otherwise in environment
  X = 1 # intercept
  if (is.list(x)) {
    for (i in 1:length(x)) {
      if (!is.null(dataset) && names(x)[i] %in% colnames(dataset)) {
        X = cbind(X, dataset[[ names(x)[i] ]])
      } else {
        X = cbind(X, x[[i]])
      }
    }
    xvar = names(x)
  } else {
    if (!is.null(dataset) && x %in% colnames(dataset)) {
      X = cbind(X, dataset[[x]])
      xvar = x
    } else {
      X = cbind(X, x)
      xvar = deparse(substitute(x))
    }
  }

  # calculate beta estimate, SE, t statistic, and p
  n = nrow(X)
  p = ncol(X)
  beta = c(solve(t(X)%*%X) %*% t(X) %*% Y) #estimate
  Yhat = X %*% beta
  res = Y - Yhat
  sigma2 = t(res)%*%res / (n-p)
  betavar = diag(solve(t(X)%*%X)) * c(sigma2)
  betase = sqrt(betavar) #SE
  tstat = c(beta / betase) #t
  prob = c(2*(1 - pt(q = abs(tstat), df = n-p))) #p

  # generate output table
  final = data.frame(cbind(Est = beta,
                           SE = betase,
                           t = tstat,
                           p = prob))
  rownames(final) = c("Intercept", names(x))

  return(final)
}
