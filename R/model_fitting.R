#' Obtain the MLE via Pseudo-Inverse
#'
#' This function finds the MLE estimate for the regression coefficients via pseudo-inverse.
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#'
#' @return A vector of linear regression coefficients.
#'
find_mle_pseudoinverse <- function(design, outcome) {
  svd_result <- svd(t(design) %*% design)
  d <- svd_result$d
  d[d != 0] <- 1/d[d != 0]
  pseudo_inverse <- svd_result$v %*% diag(d) %*% t(svd_result$u)
  regression_coef <- pseudo_inverse %*% t(design) %*% matrix(outcome, ncol = 1)
  return(as.vector(regression_coef))
}

#' Obtain the MLE via BFGS
#'
#' This function finds the MLE estimate for the regression coefficients via BFGS.
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#'
#' @return A vector of linear regression coefficients.
#'
find_mle_bfgs <- function(design, outcome) {
  n_pred <- dim(design)[2]
  optim_results <- optim(par = rep(0, n_pred), fn = log_likelihood_func, gr = log_likelihood_gradient,
    method = "BFGS", design = design, outcome = outcome, control = list(fnscale = -1))
  regression_coef <- optim_results$par
  return(regression_coef)
}

#' Calculate the log likelihood under a linear model
#'
#' This function calculates the log likelihood of the data for linear regression.
#' Only terms related to regression coefficients are calculated.
#'
#' @param regression_coef vector, coefficients in the linear regression.
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param noise_var numeric, the variance of the random noise in the model. The default is 1.
#'
#' @return The log likelihood value related to the regression coefficients.
#'
log_likelihood_func <- function(regression_coef, design, outcome, noise_var = 1) {
  residual <- outcome - design %*% regression_coef
  log_likelihood <- -sum(residual^2)/(2 * noise_var)
  return(log_likelihood)
}

#' Calculate the gradient of log likelihood under a linear model
#'
#' This function calculates the gradient vector of log likelihood for a linear regression.
#'
#' @param regression_coef vector, coefficients in the linear regression.
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param noise_var numeric, the variance of the random noise in the model. The default is 1.
#'
#' @return An n by 1 matrix of gradient of log likelihood.
#'
log_likelihood_gradient <- function(regression_coef, design, outcome, noise_var = 1) {
  gradient <- t(design) %*% (outcome - design %*% regression_coef)/noise_var
  return(gradient)
}
