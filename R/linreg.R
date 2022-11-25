#' @title Linear Regression
#' @description Runs linear regression model
#' @param y outcome or dependent variable
#' @param x matrix of covariates or independent variables
#' @param dataset dataset containing y and x
#' @return a table of results
#' @examples
#' linreg(rep(0,10), 1:10, mtcars)
#' @importFrom stats pt
#' @export

linreg = function(y, x, dataset=NULL) {
  # look for y in dataset, otherwise in environment
  if(!is.null(dataset) && y %in% colnames(dataset)) {
    Y = dataset[[y]]
  } else {
    Y = y
  }

  # look for x and var names in dataset, otherwise in environment
  X = 1
  for (i in 1:length(x)) {
    if (!is.null(dataset) && names(x)[i] %in% colnames(dataset)) {
      X = cbind(X, dataset[[ names(x)[i] ]])
    } else {
      X = cbind(X, x[[i]])
    }
  }

  # calculate beta estimate, SE, and p
  n = nrow(X)
  p = ncol(X)
  beta = c(solve(t(X)%*%X) %*% t(X) %*% Y) #estimate
  Yhat = X %*% beta
  res = Y - Yhat
  sigma2 = t(res)%*%res / (n-p)
  betavar = diag(solve(t(X)%*%X)) * c(sigma2)
  betase = sqrt(betavar) #SE
  tstat = c(beta / betase)
  prob = c(2*(1 - pt(q = abs(tstat), df = n-p))) #p

  # generate output table
  final = data.frame(cbind(Est = beta,
                           SE = betase,
                           t = tstat,
                           p = prob))
  rownames(final) = c("Intercept", names(x))

  return(final)
}
