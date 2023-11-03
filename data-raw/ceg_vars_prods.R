## Products and variables used by CEG Heritage Harbor

# These are internal
ceg_product_tbl <- readr::read_csv(here::here("data-raw/CEG Geospatial Data Sources - Products.csv"))
ceg_variable_tbl <- readr::read_csv(here::here("data-raw/CEG Geospatial Data Sources - Variables.csv"))

# External-facing "ceg_vars" object takes the form:
# ceg_vars$<product_name>$<variable_name>$<time_scale> and the value is the
# variable_id
ceg_vars <- list()
for (v in ceg_variable_tbl$variable_id) {
  prodid <- with(ceg_variable_tbl, product_id[variable_id == v])
  prodname <- with(ceg_product_tbl, product_descriptor[product_id == prodid])
  varname <- with(ceg_variable_tbl, variable_name[variable_id == v])
  tscale <- with(ceg_variable_tbl, time_scale[variable_id == v])
  ceg_vars[[prodname]][[varname]][[tscale]] <- v
}

usethis::use_data(ceg_product_tbl, ceg_variable_tbl,
                  overwrite = TRUE, internal = TRUE)
usethis::use_data(ceg_vars, overwrite = TRUE)
