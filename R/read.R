#' Read an environmental variable
#'
#' @param var_id `[chr(1)]` The variable id. Use `erd_variables` to look up
#'   variable ids by name and time scale, e.g., `erd_variables$o2$historical`.
#' @param lon `[dbl(n)]` Longitudes of points to extract. Must be the same
#'   length as `lat` and `t`, and must fall within the _spatial_ domain of the
#'   product containing variable `var_id`.
#' @param lat `[dbl(n)]` Latitudes of points to extract. Must be the same length
#'   as `lon` and `t`, and must fall within the _spatial_ domain of the product
#'   containing variable `var_id`.
#' @param t `[dbl(n)]` Times of points to extract. Must be the same length as
#'   `lon` and `lat`, and must fall within the _temporal_ domain of the product
#'   containing variable `var_id`.
#'
#' @return `[dbl(n)]` The values of the environmental variable extracted from
#'   the points (`lon`, `lat`) at the points in time closest to `t`.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1234)
#' x <- cumsum(c(-125, runif(9, -0.2, 0.2)))
#' y <- cumsum(c(33, runif(9, -0.2, 0.2)))
#' t <- as.POSIXct("2020-04-01", tz = "UTC") + (0:9) * 3600 * 24 * 7
#' read_var(erd_variables$o2$historical, x, y, t)
#' }
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
