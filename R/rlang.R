#' @export
symbolize <- function(value) {
  if (length(value) > 1) {
    dplyr::syms(value)
  } else {
    dplyr::sym(value)
  }
}
