##### PREAMBLE #####
rm(list = ls())
cat("\014")

# Set the appropriate working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Set the list of packages required
packageList <- c(
  "ggplot2", "leaflet", "rgdal", "raster", "rgeos",
  "maptools", "spdep", "leaflet", "RColorBrewer", "tidyverse", "sf",
  "furrr"
)


#### FUNCTIONS ####
land_coverage <- function(fsa_shape, cma_boundary_file) {

  # debug
  # fsa_shape=fsaBoundary_sf[which(fsaBoundary_sf$CFSAUID == "P0C"),]
  # cma_boundary_file=cmaBoundary_sf

  intersecting_cma <- st_intersects(st_geometry(fsa_shape), st_geometry(cma_boundary_file))
  cma_vector <- as.character(cma_boundary_file$CMANAME[intersecting_cma[[1]]])

  fractionCovered <- map(
    cma_vector,
    function(x) {
      as.numeric(sum(st_area(st_intersection(
        st_geometry(fsa_shape),
        st_geometry(cma_boundary_file[which(cma_boundary_file$CMANAME == x), ])
      )))) /
        as.numeric(sum(st_area(fsa_shape)))
    }
  )

  names(fractionCovered) <- cma_vector


  return(fractionCovered)
}

install_packages <- function(packageList) {
  newPackages <- packageList[!(packageList %in% installed.packages()[, "Package"])]

  if (length(newPackages)) {
    install.packages(newPackages)
  }

  lapply(packageList, library, character.only = TRUE)
}


#### MAIN ####

install_packages(packageList)

# Convert to SF Object
# Planar projection for Canadian spatial objects. This will allow us to buffer spatial object with errors (fix self intersecting geometries)

fsaBoundary_sf <- st_read("../2011_cart_fsa/gfsa000b11a_e.shp") %>%
  st_transform("+init=epsg:3347")

cmaBoundary_sf <- st_read("../2011_cart_cma/gcma000b11a_e.shp") %>%
  st_transform("+init=epsg:3347")

plan(multiprocess)

result <- future_map(as.character(fsaBoundary_sf$CFSAUID), function(x) {
  land_coverage(st_make_valid(fsaBoundary_sf[which(fsaBoundary_sf$CFSAUID == x), ]), st_make_valid(cmaBoundary_sf))
}, .progress = TRUE) %>%
  set_names(as.character(fsaBoundary_sf$CFSAUID))


final_df <- tibble(fsa = names(result)) %>%
  mutate(
    fsa_covered = map(fsa, function(x) result[[x]]),
    cma = map(fsa, function(x) names(result[[x]]))
  ) %>%
  unnest(cols = c(fsa_covered, cma)) %>%
  unnest(cols = c(fsa_covered)) %>%
  left_join(., {
    .
  } %>%
    group_by(fsa) %>%
    summarise(cma_count = n())) %>%
  right_join(tibble(fsa = as.character(fsaBoundary_sf$CFSAUID)))

max_df <- final_df %>%
  group_by(fsa) %>%
  summarise(max_coverage = max(fsa_covered)) %>%
  left_join(final_df %>%
    dplyr::select(c(fsa, fsa_covered, cma)), by = c("fsa" = "fsa", "max_coverage" = "fsa_covered"))

write.csv(final_df, "./output/fsa_to_cma.csv", row.names = FALSE)
write.csv(max_df, "./output/fsa_to_cma_max.csv", row.names = FALSE)
