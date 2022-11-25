#' @title Linear Regression
#' @description Runs linear regression model
#' @param y outcome or dependent variable
#' @param x matrix of covariates or independent variables
#' @param dataset dataset containing y and x
#' @return a table of results
#' @examples
#' linreg("hp", list(cyl = "cyl"), mtcars)
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
  for (i in 1:length(x)) {
    if (!is.null(dataset) && names(x)[i] %in% colnames(dataset)) {
      X = cbind(X, dataset[[ names(x)[i] ]])
    } else {
      X = cbind(X, x[[i]])
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
