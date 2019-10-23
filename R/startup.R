.onLoad <- function(libname, pkgname) {
  options(digits.secs = 3)
  Sys.setenv(TZ = "UTC")
  Sys.setenv(language = "en")

  # Main configs -----
  config_files <- fs::dir_ls(here::here(), type = "file", regexp = "\\.yml$")
  config_files %>%
    purrr::walk(function(.file) {
      message(stringr::str_glue("Loading config into options: {fs::path_file(.file)}"))
      arg_list <- list(config::get(file = .file)) %>%
        purrr::set_names(fs::path_file(.file))
      rlang::call2(quote(options), !!!arg_list) %>%
        rlang::eval_tidy()
    })

  # For plumber testing -----
  .__STATE__ <<- new.env(parent = emptyenv()) #create .state when package is first loaded

  invisible(TRUE)
}
