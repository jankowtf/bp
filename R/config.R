#' @export
# get_config <- function(value, sep = "/") {
#   value_0 <- value
#   ret <- if (value %>% stringr::str_detect(sep)) {
#     value <- value %>% stringr::str_replace(sep, ".")
#     configs <- config::get() %>% unlist()
#     configs[value]
#   } else {
#     config::get(value)
#   }
#
#   if (is.na(names(ret)) && is.na(ret)) {
#     stop(stringr::str_glue("No such config defined: {value_0}"))
#   }
#
#   ret
# }

#' @export
# get_config <- function(value, sep = "/") {
#   get_list_element_recursively(
#     config::get(),
#     stringr::str_split(value, sep, simplify = TRUE),
#     value
#   )
# }

#' @export
get_config <- function(
  value = character(),
  from = "config.yml",
  sep = "/"
) {
  configs <- getOption(from)
  if (is.null(configs)) {
    configs <- config::get(file = here::here(from))
  }

  # Early exit:
  if (!length(value)) {
    return(configs)
  }

  get_list_element_recursively(
    configs,
    stringr::str_split(value, sep, simplify = TRUE)
  )

  # value %>%
  #   purrr::map(~get_list_element_recursively(
  #     configs,
  #     stringr::str_split(., sep, simplify = TRUE)
  #   )
  # )
}

get_list_element_recursively <- function(
  lst,
  el,
  .el_trace = el,
  .level_trace = 1
) {
  # Reached leaf:
  if (!is.list(lst)) {
    return(lst)
  }

  # Element not in list:
  if (!(el[1] %in% names(lst))) {
    message("Current list branch:")
    message(str(lst))
    message("Trace of indexing vec (last element is invalid):")
    message(stringr::str_c(.el_trace[.level_trace], collapse = "/"))
    stop(stringr::str_glue("No such element in list: {el[1]}"))
  }

  # Index:
  lst <- lst[[ el[1] ]]

  if (!is.na(el[2])) {
    # Continue if there are additional elements in `el` vec
    Recall(lst, el[-1], .el_trace, .level_trace = 1:(.level_trace + 1))
  } else {
    # Otherwise return last indexing result:
    lst
  }
}

#' @export
load_configs <- function(dir = here::here()) {
  config_files <- fs::dir_ls(dir, type = "file", regexp = "\\.yml$")
  config_files %>%
    purrr::walk(function(.file) {
      message(stringr::str_glue("Loading config into options: {fs::path_file(.file)}"))
      arg_list <- list(config::get(file = .file)) %>%
        purrr::set_names(fs::path_file(.file))
      rlang::call2(quote(options), !!!arg_list) %>%
        rlang::eval_tidy()
    })
}
