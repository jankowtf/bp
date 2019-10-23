# configs <- config::get(file = here::here("config.yml"))
# configs <- config::get()

test_that("get_config() works (branch)", {
  res <- get_config("column_names")
  expect_is(res, "list")
  expect_true(length(res) > 0)
})

test_that("get_config() works (leaf)", {
  res <- get_config("column_names/col_group")
  expect_is(res, "character")
  expect_true(length(res) == 1)
})

test_that("get_config() works (all)", {
  res <- get_config()
  expect_is(res, "list")
  expect_true(length(res) > 1)
})

test_that("get_config() works (error)", {
  expect_error(get_config("notthere"), "No such element in list: notthere")
})
