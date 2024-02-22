#' Parse the Model Type and Control Options
#'
#' This function parses and validates the model type and control options for hiper_glm().
#' The valid model type and control options are described in hiper_glm().
#' Terminate the program if either the model type or a control option is invalid.
#'
#' @param model string, the model type parameter passed to hiper_glm().
#' @param option list, a list of control options passed to hiper_glm().
#'
#' @return An appropriate list of control options.
#'
parse_model_and_option <- function(model, option) {
  if (!(model %in% c("linear", "logit"))) {
    stop("Invalid \"model\" parameter. Please choose from c(\"linear\", \"logit\").")
  }

  if (is.null(option)) {
    if (model == "linear") {
      option <- list(mle_solver = "OLS")
    } else if (model == "logit") {
      option <- list(mle_solver = "Newton")
    }
    return(option)
  }

  option_names <- names(option)
  if ("mle_solver" %in% option_names) {
    if (model == "linear") {
      if (!option$mle_solver %in% c("OLS", "BFGS")) {
        stop("Invalid \"mle_solver\" option. Please choose from c(\"OLS\", \"BFGS\").")
      }
    } else if (model == "logit") {
      if (!option$mle_solver %in% c("Newton", "BFGS")) {
        stop("Invalid \"mle_solver\" option. Please choose from c(\"BFGS\", \"Newton\").")
      }
    }
  } else {
    if (model == "linear") {
      option_names$mle_solver <- "OLS"
    } else if (model == "logit") {
      option_names$mle_solver <- "Newton"
    }
  }

  return(option)
}

#' Prepare for the Output
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
