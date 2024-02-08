#' High Performance Generalized Linear Model Constructor
#'
#' This function creates a high performance GLM model.
#'
#' @param design matrix, the design matrix containing covariates for the model.
#' @param outcome vector, the outcome variable for the model.
#' @param model string, model type, chosen from c('linear'). The default is 'linear'.
#' @param options list, an appropriate list of control options. The default is an empty list.
#' (1) mle_solver: chosen from c('OLS', BFGS'). The default is 'OLS'.
#'
#' @return An object of class 'hglm'.
#'
#' @export
#'
hiper_glm <- function(design, outcome, model = 'linear', options=list()) {
  # parse options
  option_names = names(options)
  mle_solver = 'OLS'
  if ('mle_solver' %in% option_names) {
    if (option_names$mle_solver %in% c('OLS', 'BFGS')) {
      mle_solver = option_names$mle_solver
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
      coef = pseudo_inverse %*% t(design) %*% matrix(outcome, ncol=1)

    } else if (mle_solver == 'BFGS') {


    }

  } else { # invalid 'model' parameter
    stop("Invalid 'model' parameter. Please choose from c('linear').")
  }

  # prep to return
  coef = as.vector(coef)
  if (!is.null(colnames(design))) {
    names(coef) = colnames(design)
  }
  hglm_obj = structure(list(coef=coef), class='hglm')

  return(hglm_obj)
}
