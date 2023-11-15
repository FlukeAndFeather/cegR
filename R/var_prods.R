var_to_char <- function(var_id) {
  if (length(var_id) > 1) {
    stop("var_id must be a single variable. Did you forget to specify the time scale? E.g. cegr_vars$Altimetry$adt when you meant cegr_vars$Altimetry$adt$nrt.")
  }
  i <- which(ceg_variable_tbl$variable_id == var_id)
  name <- ceg_variable_tbl$variable_name[i]
  time <- ceg_variable_tbl$time_scale[i]
  stringr::str_glue("{name} ({time})")
}

prod_to_char <- function(prod_id) {
  i <- which(ceg_product_tbl$product_id == prod_id)
  name <- ceg_product_tbl$product_descriptor[i]
  time <- ceg_product_tbl$time_scale[i]
  stringr::str_glue("{name} ({time})")
}

get_product_id <- function(var_id) {
  if (length(var_id) > 1) {
    stop("var_id must be a single variable. Did you forget to specify the time scale? E.g. cegr_vars$Altimetry$adt when you meant cegr_vars$Altimetry$adt$nrt.")
  }
  i <- which(ceg_variable_tbl$variable_id == var_id)
  ceg_variable_tbl$product_id[i]
}

get_internal_name <- function(var_id) {
  if (length(var_id) > 1) {
    stop("var_id must be a single variable. Did you forget to specify the time scale? E.g. cegr_vars$Altimetry$adt when you meant cegr_vars$Altimetry$adt$nrt.")
  }
  i <- which(ceg_variable_tbl$variable_id == var_id)
  ceg_variable_tbl$internal_name[i]
}
