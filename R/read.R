read_var <- function(var_id, lon, lat, t) {
  product_path <- get_product_path(var_id)
  internal_name <- get_internal_name(var_id)
  var_rast <- terra::sds(product_path)[internal_name]
  time_ext <- range(terra::time(var_rast))
  pts <- terra::vect(cbind(lon, lat), crs = "EPSG:4326")

  if (!all(terra::relate(var_rast, pts, "covers"))) {
    stop(stringr::str_glue(
"Some points (lon, lat) fall outside the product spatial extent.
Product spatial extent:
{terra::ext(var_rast)}
Points spatial extent:
{terra::ext(pts)}"))
  }
  if (!all(t >= time_ext[1] & t <= time_ext[2])) {
    stop(stringr::str_glue(
"Some points (t) fall outside the product temporal extent.
Product temporal extent:
{time_ext[1]} - {time_ext[2]}
Points temporal extent:
{min(t)} - {max(t)}"))
  }

  var_pts <- terra::extract(var_rast, pts)
  t_idx <- sapply(t, \(x) which.min(abs(terra::time(var_rast) - x))) + 1
  purrr::map2_dbl(seq(length(pts)), t_idx, \(r, c) var_pts[r, c])
}
