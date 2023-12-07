cegr_read <- function(cegr_var, lon, lat, t) {
  stopifnot(is_path_valid(cegr_var))

  if (grepl("ROMS", cegr_var)) {
    read_roms(cegr_var, lon, lat, t)
  } else {
    stop("Only reading ROMS variables implemented.")
  }
}

read_roms <- function(cegr_var, lon, lat, t) {
  var_split <- strsplit(cegr_var, ":")[[1]]
  roms_var <- var_split[3]
  roms_path <- cegr_paths[[paste(var_split[1:2], collapse = ":")]]
  roms_var_path <- dir(roms_path, pattern = rom)
  path <- cegr_paths[[cegr_var]]
}
