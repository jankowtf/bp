
#' @export
pretty_get_mapping_table <- function(
  mapping_table_parent,
  values,
  accounting = FALSE,
  .col_from = bp::get_config("column_names/col_name") %>%
    bp::symbolize(),
  .col_to = if (!accounting) {
    bp::get_config("column_names/col_name_pretty") %>%
      bp::symbolize()
  } else {
    bp::get_config("column_names/col_name_pretty_accounting") %>%
      bp::symbolize()
  }
  # ...
) {
  col_from
  col_to

  # cols <- rlang::list2(...)
  mapping_table_parent %>%
    dplyr::select({{ .col_from }} , {{ .col_to }}) %>%
    dplyr::filter({{ .col_from }} %in% values)
}

#' @export
pretty_get_mapping_table_accounting <- function(
  mapping_table_parent,
  values,
  accounting = FALSE,
  .col_from = bp::get_config("column_names/col_name") %>%
    bp::symbolize(),
  .col_to =bp::get_config("column_names/col_name_pretty_accounting") %>%
    bp::symbolize()
  # ...
) {
  col_from
  col_to

  # cols <- rlang::list2(...)
  mapping_table_parent %>%
    dplyr::select({{ .col_from }} , {{ .col_to }}) %>%
    dplyr::filter({{ .col_from }} %in% values)
}
