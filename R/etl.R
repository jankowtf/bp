
# Trans -------------------------------------------------------------------

#' #' @export
#' trans_sum_y <- function(
#'   x,
#'   # .col_group = dplyr::sym("group"),
#'   .col_group = bp::get_config("column_names/.col_group") %>%
#'     bp::symbolize(),
#'   # .col_year = dplyr::sym("year"),
#'   .col_year = bp::get_config("column_names/.col_year") %>%
#'     bp::symbolize(),
#'   .suffix_m = bp::get_config("suffixes/month"),
#'   .suffix_y = bp::get_config("suffixes/year"),
#'   .suffix_mixed = bp::get_config("suffixes/mixed")
#' ) {
#'   # .col_group
#'   # .col_year
#'   .cols_group_by <- list(.col_group, .col_year)
#'   x %>%
#'     dplyr::select(-dplyr::ends_with(.suffix_y)) %>%
#'     # dplyr::group_by({{ .col_year }}) %>%
#'     dplyr::group_by(!!!.cols_group_by) %>%
#'     dplyr::summarize_if(is.double, sum, na.rm = TRUE) %>%
#'     dplyr::rename_at(vars(dplyr::ends_with(.suffix_m)),
#'       ~stringr::str_replace(.,
#'         stringr::str_glue("_{.suffix_m}$"),
#'         stringr::str_glue("_{.suffix_y}")
#'       )
#'     ) %>%
#'     dplyr::rename_at(vars(dplyr::ends_with(.suffix_mixed)),
#'       ~stringr::str_replace(.,
#'         stringr::str_glue("_{.suffix_mixed}$"),
#'         stringr::str_glue("_{.suffix_y}")
#'       )
#'     ) %>%
#'     dplyr::ungroup()
#' }

#' #' @export
#' trans_to_pretty <- function(x, col, lookup) {
#'   col <- dplyr::enquo(col)
#'   col_pretty <- dplyr::sym(stringr::str_c(col %>% dplyr::quo_name(), "_pretty"))
#'
#'   x %>%
#'     dplyr::mutate(
#'       {{ col_pretty }} := trans_to_pretty_lookup({{ col }}, .in = lookup)
#'     )
#' }
#'
#' #' @export
#' trans_to_pretty_lookup <- function(
#'   .x,
#'   .in
#' ) {
#'   .x %>%
#'     purrr::map_chr(~
#'       .in %>%
#'         dplyr::filter(name == .x) %>%
#'         dplyr::pull(name_pretty_accounting)
#'   )
#' }
