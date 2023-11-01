var_to_char <- function(var_id) {
  if (length(var_id) > 1) {
    stop("var_id must be a single variable. Did you forget to specify the time scale? E.g. erd_variables$sst when you meant erd_variables$sst$historical.")
  }
  i <- which(erd_variable_tbl$variable_id == var_id)
  name <- erd_variable_tbl$variable_name[i]
  time <- erd_variable_tbl$time_scale[i]
  stringr::str_glue("{name} ({time})")
}

prod_to_char <- function(prod_id) {
  i <- which(erd_product_tbl$product_id == prod_id)
  name <- erd_product_tbl$product_descriptor[i]
  time <- erd_product_tbl$time_scale[i]
  stringr::str_glue("{name} ({time})")
}

get_product_id <- function(var_id) {
  if (length(var_id) > 1) {
    stop("var_id must be a single variable. Did you forget to specify the time scale? E.g. erd_variables$sst when you meant erd_variables$sst$historical.")
  }
  i <- which(erd_variable_tbl$variable_id == var_id)
  erd_variable_tbl$product_id[i]
}
