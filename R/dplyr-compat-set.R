# ------------------------------------------------------------------------------
# Functions to extend dplyr verbs to work with parameter set objects

# ------------------------------------------------------------------------------
# These functions are for dplyr > 1.0.0. See ?dplyr::dplyr_extending

#' @export
dplyr_row_slice.parameters <- function(data, i, ...)  {
  vctrs::vec_slice(data, i)
}

#' @export
dplyr_col_modify.parameters <- function(.data, cols) {
  .data <- dplyr::dplyr_col_modify(tibble::as_tibble(.data), cols)
  .data <- check_new_names(.data)
  parameters_constr(
    name         = .data$name,
    id           = .data$id,
    source       = .data$source,
    component    = .data$component,
    component_id = .data$component_id,
    object       = .data$object
  )
}

dials_param_cols <- c('name', 'id', 'source', 'component', 'component_id', 'object')

has_req_names <- function(x) {
  all(dials_param_cols %in% names(x))
}

check_new_names <- function(x) {
  if (!has_req_names(x)) {
    rlang::abort(
      paste0(
        "A `parameters` object has required columns.\nMissing columns: ",
        paste0("'", dials_param_cols[!dials_param_cols %in% names(x)], "'",
               collapse = ", ")
      )
    )
  }
  extra_names <- names(x)[!names(x) %in% dials_param_cols]
  if (length(extra_names) != 0) {
    rlang::warn(
      paste0(
        "Extra names were added to the `parameters`, which has a specific data ",
        "structure. These columns will be removed: ",
        paste0("'", extra_names, "'", collapse = ", ")
      )
    )
  }
  invisible(x)
}

#' @export
`[.parameters` <- function(x, i, j, drop = FALSE) {
  res <- NextMethod()
  if (!has_req_names(res)) {
    res <- tibble::as_tibble(res)
  } else {
    check_new_names(res)
    res <-
      parameters_constr(
        name         = res$name,
        id           = res$id,
        source       = res$source,
        component    = res$component,
        component_id = res$component_id,
        object       = res$object
      )
  }
  res
}

# if (dplyr < 1.0.0) {
#   # define S3 methods
#   if (R == 3.6.0) {
#     # s3_register them
#   }
# } else {
#   # define new methods
# }


# ------------------------------------------------------------------------------
# Changes below for dplyr < 1.0.0 to extend S3 methods


## based on
## https://github.com/tidyverse/googledrive/commit/95455812d2e0d6bdf92b5f6728e3265bf65d8467#diff-ba61d4f2ccd992868e27305a9ab68a3c

#' base_classes <- c(class(tibble::tibble()))
#'

