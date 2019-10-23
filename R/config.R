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
  idx <- config_files %>%
    fs::path_file() %>%
    stringr::str_detect("^(_|\\.)", negate = TRUE)
  config_files <- config_files[idx]

  config_files %>%
    purrr::walk(function(.file) {
      message(stringr::str_glue("Loading config into options: {fs::path_file(.file)}"))
      arg_list <- list(config::get(file = .file)) %>%
        purrr::set_names(fs::path_file(.file))
      rlang::call2(quote(options), !!!arg_list) %>%
        rlang::eval_tidy()
    })
}

#' @export
assign_configs <- function(
  configs,
  # env = rlang::env_parent()
  env = rlang::caller_env(),
  from = "config.yml"
) {
  purrr::map2(names(configs), configs, function(.x, .y) {
    if (!is.list(.y)) {
      .y <- if (is.call(.y)) {
        rlang::eval_tidy(.y)
      } else if (is.character(.y)) {
        if (.y[[1]] %>% stringr::str_detect("/")) {
          # TODO-20190925-1: find better solution for vectorized input
          .y %>% purrr::map_chr(get_config, from = from)
        } else {
          .y
        }
      } else {
        stop("Unsupported value for .y")
      }
      .y <- if (length(.y) == 1) {
        rlang::sym(.y)
      } else {
        rlang::syms(.y)
      }
    }
    assign(.x, .y, envir = env)
  })
}
