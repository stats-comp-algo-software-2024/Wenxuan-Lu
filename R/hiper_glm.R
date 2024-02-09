#' High Performance Generalized Linear Model Constructor
#'
#' This function creates a high performance GLM model.
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param model string, model type, chosen from c('linear'). The default is 'linear'.
#' @param option list, an appropriate list of control options. The default is NULL.
#' (1) mle_solver: chosen from c('OLS', BFGS'). The default is 'OLS'.
#'
#' @return An object of class 'hglm'.
#'
#' @export
#'
hiper_glm <- function(design, outcome, model = 'linear', option=NULL) {
  # parse option
  option_names = names(option)
  mle_solver = 'OLS'
  if ('mle_solver' %in% option_names) {
    if (option$mle_solver %in% c('OLS', 'BFGS')) {
      mle_solver = option$mle_solver
    } else {
      warning("Invalid 'mle_solver' option. Please choose from c('OLS', BFGS'). The default 'OLS' is used.")
    }
  }

  # model fitting
  if (model == 'linear') {
    if (mle_solver == 'OLS') { # OLS via pseudo-inverse
      svd_result = svd(t(design) %*% design)
      d = svd_result$d
      d[d!=0] = 1/d[d!=0]
      pseudo_inverse = svd_result$v %*% diag(d) %*% t(svd_result$u)
      beta = pseudo_inverse %*% t(design) %*% matrix(outcome, ncol=1)

    } else if (mle_solver == 'BFGS') {
      n_pred = dim(design)[2]
      optim_results = optim(par=rep(0, n_pred) , fn=log_likelihood_func, gr=log_likelihood_gradient, method='BFGS', design=design, outcome=outcome, control=list(fnscale=-1))
      beta = optim_results$par

    } else { # invalid 'model' parameter
      stop("Invalid 'model' parameter. Please choose from c('linear').")
    }
  }

  # prep to return
  coef = as.vector(beta)
  if (!is.null(colnames(design))) {
    names(coef) = colnames(design)
  }
  hglm_obj = structure(list(coef=coef), class='hglm')

  return(hglm_obj)
}

#' Calculate the log likelihood under a linear model
#'
#' This function calculates the log likelihood of the data for linear regression
#'
#' @param beta vector, coefficients in the linear regression
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param noise_var numeric, the variance of the random noise in the model. The default is 1.
#'
#' @return The log likelihood of data under the linear model
#'
log_likelihood_func<-function(beta, design, outcome, noise_var = 1) {
  n = dim(design)[1]
  beta = as.matrix(beta, ncol=1)
  outcome = as.matrix(outcome, ncol=1)
  residues = outcome - design %*% beta
  log_likelihood = -n/2 * log(2*pi*noise_var) - sum(residues^2) / (2 * noise_var)
  log_likelihood = as.vector(log_likelihood)
  return(log_likelihood)
}

#' Calculate the gradient of log likelihood under a linear model
#'
#' This function calculates the gradient vector of log likelihood for a linear regression
#'
#' @param beta vector, coefficients in the linear regression
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param noise_var numeric, the variance of the random noise in the model. The default is 1.
#'
#' @return a vector of gradient of log likelihood
#'
log_likelihood_gradient<-function(beta, design, outcome, noise_var = 1) {
  beta = as.matrix(beta, ncol=1)
  outcome = as.matrix(outcome, ncol=1)
  gradient = t(design) %*% (outcome - design %*% beta) / noise_var
  gradient = as.vector(gradient)
  return(gradient)
}
