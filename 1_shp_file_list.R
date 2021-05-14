# Standard functions to clear the environment and the console
rm(list = ls())
cat("\014")

# Set the appropriate working directory (***** NEED TO SOURCE THE FILE FIST *****)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Set the list of packages required
packageList <- c("rgdal", "raster", "rgeos", "maptools", "spdep", "parallel", "pbapply", "data.table")

# Function to test of a package is installed
newPackages <- packageList[!(packageList %in% installed.packages()[, "Package"])]
if (length(newPackages)) {
  install.packages(newPackages)
}

# Additional Functions
extract_shapeinfo <- function(fsa, shapefile, year) {
  shapfile_subset <- shapefile[which(shapefile$CFSAUID == fsa), ]
  area <- area(shapfile_subset) / (1000^2)
  centroid_df <- gCentroid(shapfile_subset, byid = TRUE)
  cent_lat <- centroid_df@coords[2]
  cent_lng <- centroid_df@coords[1]
  result_list <- list(
    year = year, fsa = fsa, area = area,
    centroid_lat = cent_lat, centroid_lng = cent_lng
  )
  return(result_list)
}

# Install all packages that are not already installed
lapply(packageList, library, character.only = TRUE)

# Set the files to import
shp_2011_directory <- "../data/shape_fsa_2011"
shp_2016_directory <- "../data/shape_fsa_2016"

shp_2011 <- readOGR(shp_2011_directory, "gfsa000b11a_e")
shp_2016 <- readOGR(shp_2016_directory, "lfsa000b16a_e")

# Print the projection information
proj4string(shp_2011)
proj4string(shp_2016)

# Convert the projection coordinates to the same at 2011 (Lat/Lng)
shp_2016 <- spTransform(shp_2016, proj4string(shp_2011))


shp_2011_fsa_list <- as.list(as.character(shp_2011$CFSAUID))
shp_2016_fsa_list <- as.list(as.character(shp_2016$CFSAUID))

# Parallel Implementation
cores <- detectCores()
coresAvailiable <- cores - 2
cl <- makeCluster(coresAvailiable)
clusterEvalQ(cl, {
  packageList <- c("rgdal", "raster", "rgeos", "maptools", "spdep", "parallel", "pbapply")
  lapply(packageList, require, character.only = TRUE)
  sessionInfo()
})


clusterExport(cl, c("extract_shapeinfo", "shp_2011", "shp_2016"))

shp_2011_list <- pblapply(shp_2011_fsa_list, function(x) extract_shapeinfo(x, shp_2011, 2011), cl = cl)
shp_2016_list <- pblapply(shp_2016_fsa_list, function(x) extract_shapeinfo(x, shp_2016, 2016), cl = cl)
# extract_shapeinfo("K1T", shp_2011, 2011)

stopCluster(cl)

shp_2011_df <- rbindlist(shp_2011_list, fill = TRUE)
shp_2016_df <- rbindlist(shp_2016_list, fill = TRUE)

all_fsa <- union(shp_2011_df$fsa, shp_2016_df$fsa)

fsa_list_2011to2016 <- data.frame(
  fsa = character(),
  area_2011 = numeric(),
  area_2016 = numeric(),
  cent_lat_2011 = numeric(),
  cent_lat_2016 = numeric(),
  cent_lng_2011 = numeric(),
  cent_lng_2016 = numeric(),
  y_2011 = numeric(),
  y_2016 = numeric(),
  stringsAsFactors = FALSE
)

row_num <- 0
for (i in all_fsa) {
  row_num <- row_num + 1
  row_2011 <- which(shp_2011_df$fsa == i)
  row_2016 <- which(shp_2016_df$fsa == i)

  if (length(row_2011) > 0 & length(row_2016) > 0) {
    fsa_list_2011to2016[row_num, "fsa"] <- i
    fsa_list_2011to2016[row_num, "area_2011"] <- shp_2011_df$area[row_2011]
    fsa_list_2011to2016[row_num, "area_2016"] <- shp_2016_df$area[row_2016]
    fsa_list_2011to2016[row_num, "cent_lat_2011"] <- shp_2011_df$centroid_lat[row_2011]
    fsa_list_2011to2016[row_num, "cent_lat_2016"] <- shp_2016_df$centroid_lat[row_2016]
    fsa_list_2011to2016[row_num, "cent_lng_2011"] <- shp_2011_df$centroid_lng[row_2011]
    fsa_list_2011to2016[row_num, "cent_lng_2016"] <- shp_2016_df$centroid_lng[row_2016]
    fsa_list_2011to2016[row_num, "y_2011"] <- 1
    fsa_list_2011to2016[row_num, "y_2016"] <- 1
  } else if (length(row_2011) > 0) {
    fsa_list_2011to2016[row_num, "fsa"] <- i
    fsa_list_2011to2016[row_num, "area_2011"] <- shp_2011_df$area[row_2011]
    fsa_list_2011to2016[row_num, "cent_lat_2011"] <- shp_2011_df$centroid_lat[row_2011]
    fsa_list_2011to2016[row_num, "cent_lng_2011"] <- shp_2011_df$centroid_lng[row_2011]
    fsa_list_2011to2016[row_num, "y_2011"] <- 1
    fsa_list_2011to2016[row_num, "y_2016"] <- 0
  } else if (length(row_2016) > 0) {
    fsa_list_2011to2016[row_num, "fsa"] <- i
    fsa_list_2011to2016[row_num, "area_2016"] <- shp_2016_df$area[row_2016]
    fsa_list_2011to2016[row_num, "cent_lat_2016"] <- shp_2016_df$centroid_lat[row_2016]
    fsa_list_2011to2016[row_num, "cent_lng_2016"] <- shp_2016_df$centroid_lng[row_2016]
    fsa_list_2011to2016[row_num, "y_2011"] <- 0
    fsa_list_2011to2016[row_num, "y_2016"] <- 1
  }
}

write.csv(fsa_list_2011to2016, "../data/fsa_list.csv", row.names = FALSE)
