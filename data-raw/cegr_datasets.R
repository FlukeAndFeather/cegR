## code to prepare `cegr_datasets` dataset goes here

# Recursive function to make leaves (e.g., c(")) into lists
rename_leaves <- function(l, path = NULL) {
  if (is.null(names(l))) {
    result <- as.list(rep(path, length(l)))
    names(result) <- l
    result
  } else {
    paths <- if(is.null(path)) {
      names(l)
    } else {
      paste(path, names(l), sep = ":")
    }
    mapply(rename_leaves, l, paths, SIMPLIFY = FALSE)
  }
}

cegr_datasets <- list(
  supercomputer = list(
    ROMS = c("bbv", "curl", "ild_05", "ssh", "sst", "su", "sustr", "sv", "svstr")
  ),
  annex = list(
    satellite = list(
      `Sea surface temperature` = list(
        nrt = c("sst", "sea ice"),
        historical = c("sst", "sea ice")
      ),
      Altimetry = list(
        nrt = c("adt", "sla", "ugos", "ugosa", "vgos", "vgosa"),
        historical = c("adt", "sla", "ugos", "ugosa", "vgos", "vgosa")
      ),
      `Ocean physics model` = list(
        nrt = c("mld", "salinity", "temperature", "ice thickness and concentration", "ssh", "uo", "vo"),
        historical = c("mld", "salinity", "temperature", "ice thickness and concentration", "ssh", "uo", "vo")
      ),
      `Biogeochemistry ocean model` = list(
        nrt = c("o2", "pp", "chl", "fe", "nitrate", "ph", "po4", "si", "co2"),
        historical = c("o2", "pp", "chl", "nitrate", "po4", "si")
      ),
      Chla = list(
        nrt = "chla",
        historical = "chla"
      )
    ),
    static = list(
      Bathymetry = "bathy"
    )
  )
) %>%
  rename_leaves()

cegr_paths <- readLines("data-raw/paths.txt") %>%
  strsplit("=") %>%
  sapply(\(x) {
    result <- list(x[2])
    names(result) = x[1]
    result
  })

usethis::use_data(cegr_datasets, overwrite = TRUE)
usethis::use_data(cegr_paths, overwrite = TRUE, internal = TRUE)
