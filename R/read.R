#' Read an environmental variable
#'
#' @param cegr_var `[chr(1)]` The variable id. Use `cegr_vars` to look up
#'   variable ids by product, name, and time scale, e.g.,
#'   ``cegr_vars$`Biogeochemistry Ocean Model`$o2$historical``. See
#'   \link{cegr_vars}.
#' @param lon `[dbl(n)]` Longitudes of points to extract, in (-180, 180). Must
#'   be the same length as `lat` and `t`. See \link{cegr_convert_lon}.
#' @param lat `[dbl(n)]` Latitudes of points to extract. Must be the same length
#'   as `lon` and `t`.
#' @param t `[POSIX(n)]` Times of points to extract. Must be the same length as
#'   `lon` and `lat`.
#' @param if_outside `[chr(1)]` What to do if `lon`, `lat`, or `t` fall outside
#'   the spatiotemporal domain of the product. If "warn" (default),
#'   `cegr_read()` returns NA for those points and throws a warning. "none" does
#'   the same, but without the warning. "error" throws an error.
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
#' cegr_read(cegr_vars$`Biogeochemistry Ocean Model`$o2$historical, x, y, t)
#' }
cegr_read <- function(cegr_var, lon, lat, t,
                      if_outside = c("warn", "error", "none")) {
  if (missing(if_outside)) {
    if_outside <- "warn"
  } else {
    if_outside <- match.arg(if_outside)
  }

  product_path <- get_product_path(cegr_var)
  internal_name <- get_internal_name(cegr_var)
  var_rast <- terra::sds(product_path)[internal_name]
  time_ext <- range(terra::time(var_rast))
  pts <- terra::vect(cbind(lon, lat), crs = "EPSG:4326")

  if (!all(terra::relate(var_rast, pts, "covers")) ||
      !all(t >= time_ext[1] & t <= time_ext[2])) {
    msg <- stringr::str_glue(
"Some points (lon, lat) fall outside the product spatialtemporal extent.
Product spatial extent:
{terra::ext(var_rast)}
Points spatial extent:
{terra::ext(pts)}
Product temporal extent:
{time_ext[1]} - {time_ext[2]}
Points temporal extent:
{min(t)} - {max(t)}"
)
    switch (if_outside,
            warn = warning(msg),
            error = stop(msg),
            none = NULL)
  }

  var_pts <- terra::extract(var_rast, pts)
  t_idx <- sapply(t, \(x) which.min(abs(terra::time(var_rast) - x))) + 1
  purrr::map2_dbl(seq(length(pts)), t_idx, \(r, c) var_pts[r, c])
}

#' Convert (0, 360) longitudes to (-180, 180)
#'
#' @param lon `[dbl]` A vector of longitudes in the (0, 360) domain.
#'
#' @return `[dbl]` The equivalent vector of longitudes in the (-180, 180) domain.
#' @export
#'
#' @examples
#' cegr_convert_lon(c(150, 210))
cegr_convert_lon <- function(lon) {
  if (!all(lon >= 0 & lon <= 360)) {
    stop("`cegr_convert_lon()` converts [0, 360] longitudes to [-180, 180]. Some of your longitudes are outside of [0, 360]. Maybe your longitudes are already in [-180, 180] or you're using projected coordinates?")
  }
  (lon + 180) %% 360 - 180
}
