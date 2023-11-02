## Products and variables used by CEG Heritage Harbor

# These are internal
ceg_product_tbl <- readr::read_csv(here::here("data-raw/CEG Geospatial Data Sources - Products.csv"))
ceg_variable_tbl <- readr::read_csv(here::here("data-raw/CEG Geospatial Data Sources - Variables.csv"))

# External-facing "ceg_vars" object takes the form:
# ceg_vars$<variable_name>$<time_scale> and the value is the variable_id
ceg_var_names <- unique(ceg_variable_tbl$variable_name)
ceg_vars <- purrr::map(ceg_var_names, \(v) {
  time_scales <- ceg_variable_tbl$time_scale[ceg_variable_tbl$variable_name == v]
  var_ids <- ceg_variable_tbl$variable_id[ceg_variable_tbl$variable_name == v]
  result <- as.list(var_ids)
  names(result) <- time_scales
  result
})
names(ceg_vars) <- ceg_var_names

usethis::use_data(ceg_product_tbl, ceg_variable_tbl,
                  overwrite = TRUE, internal = TRUE)
usethis::use_data(ceg_vars, overwrite = TRUE)
