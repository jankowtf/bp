
# Packages ----------------------------------------------------------------

renv::activate()
renv::install("devtools")
renv::install("rstudio/config")
renv::install("stringr")
renv::install("dplyr")
renv::install("here")

usethis::use_package("magrittr")
usethis::use_package("config")
usethis::use_package("stringr")
usethis::use_package("dplyr")
usethis::use_package("here")

# Temp packages -----------------------------------------------------------

renv::install("t-kalinowski/tfautograph")
renv::remove("tfautograph")

renv::install("reprex")
renv::remove("reprex")

# Usethis statements ------------------------------------------------------

usethis::use_testthat()


# Scratch -----------------------------------------------------------------

load_configs()
