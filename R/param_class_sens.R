#' Cost parameter for minority class
#'
#' Used in `baguette::bag_tree()`.
#'
#' @inheritParams Laplace
#'
#' @details
#' This parameter reflects the cost of a misclassified sample relative to a
#' baseline cost of 1.0. For example, if the first level of an outcome factor
#' occurred rarely, it might help if this parameter were set to values greater
#' than 1.0. If the second level of the outcome factor is in the minority,
#' values less than 1.0 would cause the model to emphasize the minority class
#' more than the majority class.
#' @examples
#' class_cost()
#' @export
class_cost <- function(range = c(0, 5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(class_cost = "Class Cost")
  )
}
