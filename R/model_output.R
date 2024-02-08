#' Coefficients Extraction
#'
#' This function extracts the coefficients from a hglm model.
#'
#' @param hglm object, an object of class 'hglm'.
#'
#' @export
#'
coef.hglm <- function(hglm) {
  return(hglm$coef)
}


#' Variance-Covariance Matrix Calculation
#'
#' @param hglm object, an object of class 'hglm'.
#'
#' @export
#'
vcov.hglm <- function(hglm) {
  warning("The function vcov() is not implemented.")
}


#' Print Method for hglm
#'
#' Displays the hglm object.
#'
#' @param hglm object, an object of class 'hglm'.
#'
#' @export
#'
print.hglm <- function(hglm) {
  warning("The function print() is not implemented.")
}
