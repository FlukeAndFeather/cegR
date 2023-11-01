## Products and variables used by ERD

# These are internal
erd_product_tbl <- readr::read_csv(here::here("data-raw/ERD Geospatial Data Sources - Products.csv"))
erd_variable_tbl <- readr::read_csv(here::here("data-raw/ERD Geospatial Data Sources - Variables.csv"))

# External-facing "erd_variables" object takes the form:
# erd_variables$<variable_name>$<time_scale> and the value is the variable_id
erd_var_names <- unique(erd_variable_tbl$variable_name)
erd_variables <- purrr::map(erd_var_names, \(v) {
  time_scales <- erd_variable_tbl$time_scale[erd_variable_tbl$variable_name == v]
  var_ids <- erd_variable_tbl$variable_id[erd_variable_tbl$variable_name == v]
  result <- as.list(var_ids)
  names(result) <- time_scales
  result
})
names(erd_variables) <- erd_var_names

usethis::use_data(erd_product_tbl, erd_variable_tbl,
                  overwrite = TRUE, internal = TRUE)
usethis::use_data(erd_variables, overwrite = TRUE)
