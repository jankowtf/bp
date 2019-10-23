Sys.setenv(R_CONFIG_ACTIVE = "stackoverflow")

get_config <- function(value, sep = "/") {
  get_list_element_recursively(
    config::get(),
    stringr::str_split(value, sep, simplify = TRUE),
    value
  )
}

get_list_element_recursively <- function(lst, el, .el_trace = el) {
  # Reached leaf:
  if (!is.list(lst)) {
    return(lst)
  }

  # Element not in list:
  if (!(el[1] %in% names(lst))) {
    message("Current list branch:")
    # print(lst)
    message(str(lst))
    message("Trace of indexing vec (last element is invalid):")
    message(stringr::str_c(.el_trace, collapse = "/"))
    stop(stringr::str_glue("No such element in list: {el[1]}"))
  }

  lst <- lst[[ el[1] ]]


  if (!is.na(el[2])) {
    # Continue if there are additional elements in `el` vec

    Recall(lst, el[-1], .el_trace)
  } else {
    # Otherwise return last indexing result:

    lst
  }
}

get_config("column_names")
# $col_id
# [1] "id"
#
# $col_value
# [1] "value"

get_config("column_names/col_id")
# [1] "id"

get_config("column_names/col_nonexisting")
# Current list branch:
#   List of 6
# $ col_group                 : chr "group"
# $ col_name                  : chr "name"
# $ col_name_pretty           : chr "name_pretty"
# $ col_name_pretty_accounting: chr "name_pretty_accounting"
# $ col_id                    : chr "id"
# $ col_value                 : chr "value"
#
# Trace of indexing vec (last element is invalid):
#   column_names/col_nonexisting
# Error in get_list_element_recursively(config::get(), stringr::str_split(value,  :
#     No such element in list: col_nonexisting

get_config("column_orders")
# $data_structure_a
# [1] "column_names/col_id"    "column_names/col_value"
#
# $data_structure_b
# [1] "column_names/col_value" "column_names/col_id"

get_config("column_orders/data_structure_a")
# [1] "column_names/col_id"    "column_names/col_value"

x <- yaml::yaml.load('
  column_names:
    col_id: "id"
    col_value: "value"
  column_orders:
    data_structure_a: [
      column_names/col_id,
      column_names/col_value
    ]
    data_structure_b: [
      column_names/col_value,
      column_names/col_id
    ]
  nested_list:
    element_1:
      element_2:
        value: "hello world"
  ')

get_list_element_recursively(x, c("column_names"))
# $col_id
# [1] "id"
#
# $col_value
# [1] "value"

get_list_element_recursively(x, c("column_names", "col_id"))
# [1] "id"

get_list_element_recursively(x, c("column_names", "col_notthere"))
# Current list branch:
#   List of 2
# $ col_id   : chr "id"
# $ col_value: chr "value"
#
# Trace of indexing vec (last element is invalid):
#   column_names/col_notthere
# Error in get_list_element_recursively(x$default, c("column_names", "col_notthere")) :
#   No such element in list: col_notthere
