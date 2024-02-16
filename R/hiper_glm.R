#' High Performance Generalized Linear Model Constructor
#'
#' This function creates and fits a high performance GLM model.
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param model string, model type, chosen from c('linear'). The default is 'linear'.
#' @param option list, an appropriate list of control options. The default is NULL.
#' (1) mle_solver: chosen from c('OLS', 'BFGS'). The default is 'OLS'.
#'
#' @return An object of class hglm.
#'
#' @export
#'
hiper_glm <- function(design, outcome, model = "linear", option = NULL) {
  option <- parse_options(option)

  if (model == "linear") {
    if (option$mle_solver == "OLS") {
      regression_coef <- find_mle_pseudoinverse(design, outcome)
    } else if (option$mle_solver == "BFGS") {
      regression_coef <- find_mle_bfgs(design, outcome)
    }
  } else {
    stop("Invalid \"model\" parameter. Please choose from c(\"linear\").")
  }

  hglm_obj <- prepare_output(design, regression_coef)
  return(hglm_obj)
}

