#' Obtain the MLE via BFGS for Logistic Regression
#'
#' This function finds the MLE estimate for the logistic regression coefficients via BFGS.
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#'
#' @return A vector of logistic regression coefficients.
#'
find_mle_bfgs_logit <- function(design, outcome) {
  n_pred <- ncol(design)
  optim_results <- optim(par = rep(0, n_pred), fn = log_likelihood_func_logit, gr = log_likelihood_gradient_logit,
    method = "BFGS", design = design, outcome = outcome, control = list(fnscale = -1))
  regression_coef <- optim_results$par
  return(regression_coef)
}

#' Calculate the log likelihood under a Logit Model
#'
#' This function calculates the log likelihood of the data for logistic regression.
#' Only terms related to regression coefficients are calculated.
#'
#' @param regression_coef vector, coefficients in the logistic regression.
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#'
#' @return The log likelihood value related to the logistic regression coefficients.
#'
log_likelihood_func_logit <- function(regression_coef, design, outcome) {
  linear_predictor <- design %*% regression_coef
  log_likelihood <- sum(outcome * linear_predictor - log(1 + exp(linear_predictor)))
  return(log_likelihood)
}

#' Calculate the gradient of log likelihood under a Logit Model
#'
#' This function calculates the gradient vector of log likelihood for logistic regression.
#'
#' @param regression_coef vector, coefficients in the logistic regression.
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#'
#' @return An n by 1 matrix of gradient of log likelihood under a logit model.
#'
log_likelihood_gradient_logit <- function(regression_coef, design, outcome) {
  linear_predictor <- design %*% regression_coef
  gradient <- t(design) %*% (outcome - logistic_func(linear_predictor))
  return(gradient)
}

#' Logistic Function
#'
#' @param x numeric vector, the position to be evaluated on.
#'
#' @return A numeric vector of logistic function values.
#'
logistic_func <- function(x) {
  return(1/(1 + exp(-x)))
}
