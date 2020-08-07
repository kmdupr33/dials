#' Parameter objects for Regularized Discriminant Models
#'
#' `discrim::discrim_regularized()` describes the effect of `frac_common_cov()` and
#' `frac_identity()`.
#'
#' @inheritParams Laplace
#'
#' @details
#' These  parameters can modulate a RDA model to go between linear and quadratic
#' class boundaries.
#' @examples
#' frac_common_cov()
#' @export
frac_common_cov <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.5,
    label = c(threshold = "Fraction of the Common Covariance Matrix"),
    finalize = NULL
  )
}

#' @export
#' @rdname frac_common_cov
frac_identity <- function(range = c(0, 1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.5,
    label = c(threshold = "Fraction of the Identity Matrix"),
    finalize = NULL
  )
}
