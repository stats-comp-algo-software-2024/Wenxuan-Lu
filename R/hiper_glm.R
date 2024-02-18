#' High Performance Generalized Linear Model Constructor
#'
#' This function creates and fits a high performance GLM model.
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param model string, model type, chosen from c('linear', 'logit'). The default is 'linear'. The 'logit' option assumes the outcomes to be binary.
#' @param option list, an appropriate list of control options. The default is NULL.
#' For a linear model,
#' (1) mle_solver: chosen from c('OLS', 'BFGS'). The default is 'OLS'.
#' For a logistic model,
#' (1) mle_solver: chosen from c('Newton', 'BFGS'). The default is 'Newton'.
#'
#' @return An object of class hglm.
#'
#' @export
#'
hiper_glm <- function(design, outcome, model = "linear", option = NULL) {
  option <- parse_model_and_option(model, option)

  if (model == "linear") {
    if (option$mle_solver == "OLS") {
      regression_coef <- find_mle_pseudoinverse_linear(design, outcome)
    } else if (option$mle_solver == "BFGS") {
      regression_coef <- find_mle_bfgs_linear(design, outcome)
    }
  } else if (model == "logit") {
    if (option$mle_solver == "Newton") {
      regression_coef <- find_mle_newton_logit(design, outcome)
    } else if (option$mle_solver == "BFGS") {
      regression_coef <- find_mle_bfgs_logit(design, outcome)
    }
  }

  hglm_obj <- prepare_output(design, regression_coef)
  return(hglm_obj)
}

