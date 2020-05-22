#' Determine if a grid is regular
#'
#' This function contains a set of rules that were derived from simulated data
#' in order to determine if a grid is regular.
#' @param grid A tibble of grid points.
#' @return A single logical
#' @examples
#' set_1 <- parameters(penalty(), weight_func(), learn_rate())
#' set_2 <- parameters(neighbors(), min_n())
#'
#' is_regular_grid(grid_regular(set_1, levels = c(6, 2, 1)))
#' is_regular_grid(grid_regular(set_2, levels = c(10, 6)))
#'
#' set.seed(13411)
#' is_regular_grid(grid_max_entropy(set_1, size = 20))
#' is_regular_grid(grid_max_entropy(set_2, size = 10))
#' @export
#' @keywords internal
is_regular_grid <- function(grid) {
  num_points <- nrow(grid)
  p <- ncol(grid)

  if (p == 1) {
    return(TRUE)
  }

  pct_unique <- purrr::map_int(grid, ~ length(unique(.x)))/num_points
  max_pct_unique <- max(pct_unique, na.rm = TRUE)
  np_ratio <- p/num_points

  # Derived from simulation data and C5.0 tree
  if (max_pct_unique >  1/2) res <- FALSE
  if (max_pct_unique <= 1/2 & max_pct_unique <= 1/6) res <- TRUE
  if (max_pct_unique <= 1/2 & max_pct_unique >  1/6 & np_ratio >  0.05) res <- TRUE
  if (max_pct_unique <= 1/2 & max_pct_unique >  1/6 & np_ratio <= 0.05) res <- FALSE
  res
}
