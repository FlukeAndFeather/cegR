#' Title
#'
#' @param cegr_var
#' @param lon
#' @param lat
#' @param t
#'
#' @return
#' @export
#'
#' @examples
cegr_read <- function(cegr_var, lon, lat, t) {
  stopifnot(is_path_valid(cegr_var))

  if (grepl("ROMS", cegr_var)) {
    read_roms(cegr_var, lon, lat, t)
  } else {
    stop("Only reading ROMS variables implemented.")
  }
}

read_roms <- function(cegr_var, lon, lat, t) {
  # Locate ROMS variable file
  var_split <- strsplit(cegr_var, ":")[[1]]
  roms_var <- var_split[3]
  roms_path <- cegr_paths[[paste(var_split[1:2], collapse = ":")]]
  roms_var_path <- dir(roms_path, pattern = roms_var, full.names = TRUE)

  # Read dimensions and data
  roms_nc <- ncdf4::nc_open(roms_var_path)
  roms_lon <- ncdf4::ncvar_get(roms_nc, "lon_rho")[, 1]
  roms_lat <- ncdf4::ncvar_get(roms_nc, "lat_rho")[1, ]
  roms_time <- ncdf4::ncvar_get(roms_nc, "time") %>%
    as.POSIXct(tz = "UTC", origin = "2011-1-2")
  roms_data <- ncdf4::ncvar_get(roms_nc, names(roms_nc$var)[3])

  # Extract data
  roms_idx <- list(
    lon_idx = sapply(lon, \(x) which.min(abs(x - roms_lon))),
    lat_idx = sapply(lat, \(x) which.min(abs(x - roms_lat))),
    time_idx = sapply(t, \(x) which.min(abs(x - roms_time)))
  )
  result <- purrr::pmap_dbl(roms_idx, \(lon_idx, lat_idx, time_idx) roms_data[lon_idx, lat_idx, time_idx])

  # Wrap up
  ncdf4::nc_close(roms_nc)
  result
}


