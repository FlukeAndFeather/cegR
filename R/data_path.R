
get_product_path <- function(var_id) {
  prod_id <- with(erd_variable_tbl, product_id[variable_id == var_id])
  if (prod_id %in% names(product_paths())) {
    product_paths()[[prod_id]]$path
  } else{
    stop(stringr::str_glue("'{var_to_char(var_id)}' is a part of the '{prod_to_char(prod_id)}' product. The path to '{prod_to_char(prod_id)}' has not been set yet. Call set_product_path('{prod_id}')"))
  }
}

set_product_path <- function(prod_id, prod_path = "") {
  if (prod_path == "") {
    prod_path <- file.choose()
  }
  if (!file.exists(prod_path)) {
    stop(stringr::str_glue("Can't set path for '{prod_to_char(prod_id)}' to {prod_path}; file does not exist."))
  }
  paths <- product_paths()
  paths[[prod_id]] <- list(
    product = prod_to_char(prod_id),
    path = prod_path
  )
  yaml::write_yaml(paths, paths_yml())
  message(stringr::str_glue("Path for '{prod_to_char(prod_id)}' set to {prod_path}."))
}

product_paths <- function() {
  result <- yaml::read_yaml(paths_yml())
  if (is.null(result)) {
    result <- list()
  }
  result
}

paths_yml <- function() {
  result <- system.file("paths.yml", package = "rerd")
  if (!file.exists(result)) {
    file.create(result)
  }
  result
}
