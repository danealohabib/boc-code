##### PREAMBLE #####
rm(list = ls())
cat("\014")

# Set the appropriate working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Set the list of packages required
packageList <- c("ggplot2", "leaflet", "rgdal", "raster", "rgeos", "maptools", "spdep", "leaflet", "RColorBrewer", "parallel", "pbapply")

# Function to test of a package is installed
newPackages <- packageList[!(packageList %in% installed.packages()[, "Package"])]
if (length(newPackages)) {
  install.packages(newPackages)
}

# Install all packages that are not already installed
lapply(packageList, library, character.only = TRUE)

# Functions for parallel
overlay_wrapper <- function(branch_element, shape_file) {
  print(branch_element)
  ignore_flag <- FALSE

  element <- matrix(c(as.numeric(branch_element["duplicateFlag"]), as.numeric(branch_element["lat"]), as.numeric(branch_element["lng"])), nrow = 1)
  element <- as.data.frame(element, row.names = FALSE, byrow = FALSE)
  colnames(element) <- c("duplicateFlag", "lat", "lng")

  if (element[, "duplicateFlag"] == 1) {
    ignore_flag <- TRUE
  } else if ((is.na(element[, "lat"]) & is.na(element[, "lng"]))) {
    ignore_flag <- TRUE
  }

  if (ignore_flag == TRUE) {
    return("IGNORE")
  } else {
    coordinates(element) <- ~ lng + lat
    proj4string(element) <- proj4string(shape_file)
    overlay <- over(element, shape_file)
    return(as.character(overlay$CFSAUID))
  }
}

# Specify the files to be used as input. We need to import the FSA list and the branch locations from step1.
branch_file <- "../data/branch_locations_2008_2018_RAW.csv"
fsa_list <- "../data/fsa_list.csv"
output_directory <- "../data/"
branch_list_df <- read.csv(branch_file, stringsAsFactors = FALSE)

# Import the shapefiles
shp_2011_directory <- "../data/shape_fsa_2011"
shp_2016_directory <- "../data/shape_fsa_2016"

shp_2011 <- readOGR(shp_2011_directory, "gfsa000b11a_e")
shp_2016 <- readOGR(shp_2016_directory, "lfsa000b16a_e")

proj4string(shp_2011)
proj4string(shp_2016)

# convert the projection coordinates to the same at 2011
shp_2016 <- spTransform(shp_2016, proj4string(shp_2011))

cores <- detectCores()
coresAvailiable <- cores - 2
cl <- makeCluster(coresAvailiable)
clusterEvalQ(cl, {
  packageList <- c("ggplot2", "leaflet", "rgdal", "raster", "rgeos", "maptools", "spdep", "leaflet", "RColorBrewer", "parallel", "pbapply")
  lapply(packageList, require, character.only = TRUE)
  sessionInfo()
})


clusterExport(cl, c("overlay_wrapper", "shp_2011", "shp_2016", "branch_list_df"))

fsa_list_2011 <- pbapply(branch_list_df, 1, function(x) overlay_wrapper(x, shp_2011), cl = cl)
fsa_list_2016 <- pbapply(branch_list_df, 1, function(x) overlay_wrapper(x, shp_2016), cl = cl)

stopCluster(cl)

fsa_list_2011 <- as.data.frame(fsa_list_2011)
fsa_list_2016 <- as.data.frame(fsa_list_2016)

branch_list_df[, "exact_fsa_2011"] <- fsa_list_2011[, "fsa_list_2011"]
branch_list_df[, "exact_fsa_2016"] <- fsa_list_2016[, "fsa_list_2016"]

write.csv(branch_list_df, paste(output_directory, "/", "branch_locations_2008_2018_FSA_ID.csv", sep = ""), row.names = FALSE)
