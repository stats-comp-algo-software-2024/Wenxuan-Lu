#' Parse the Control Options
#'
#' This function parses and validates the control options for hiper_glm().
#' The valid control options are described in hiper_glm().
#' Terminate the program if an option is invalid.
#'
#' @param option list, a list of control options passed to hiper_glm().
#'
#' @return An appropriate list of control options.
#'
parse_options <- function(option) {
  option_names <- names(option)
  if (is.null(option_names)) {
    option <- list(mle_solver = "OLS")
    return(option)
  }

  if ("mle_solver" %in% option_names) {
    if (!option$mle_solver %in% c("OLS", "BFGS")) {
      stop("Invalid \"mle_solver\" option. Please choose from c(\"OLS\", \"BFGS\").")
    }
  } else {
    option_names$mle_solver <- "OLS"
  }

  return(option)
}

#' Prepare the output
#'
#' This function prepares the output for hiper_glm().
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param regression_coef vector, a vector of linear regression coefficients.
#'
#' @return An object of class hglm.
#'
prepare_output <- function(design, regression_coef) {
  if (!is.null(colnames(design))) {
    names(regression_coef) <- colnames(design)
  }
  hglm_obj <- structure(list(coef = regression_coef), class = "hglm")
  return(hglm_obj)
}
