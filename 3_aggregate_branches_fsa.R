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

# functions
lodaing_bar <- function(i, total) {
  percent <- round(i * 100 / total, 0)
  bar_num <- percent %/% 5
  empty_num <- 20 - bar_num
  loading_bars <- strrep("=", bar_num)
  empty_bars <- strrep(" ", empty_num)
  full_string <- paste("[", loading_bars, empty_bars, "] -- ", percent, "%", sep = "")
  cat("\r", full_string)
}

# Install all packages that are not already installed
lapply(packageList, library, character.only = TRUE)

# Specify the files to be used as input. We need to import the FSA list and the branch locations from step1.
branch_file <- "../data/branch_locations_2008_2018_FSA_ID.csv"
fsa_list <- "../data/fsa_list.csv"
output_directory <- "../data/"
branch_list_df <- read.csv(branch_file, stringsAsFactors = FALSE)

unique_id <- unique(branch_list_df$ID_Num)
unique_id_first <- as.numeric(substr(unique_id, 1, 1))
credit_union_list <- unique_id[which(unique_id_first == 8)]

# THE TOP 5 BANKS ARE CODED AS FOLLOWS - BMO 1 - SCOTIA 2 - RBC 3 - TD 4 - EXCLUDE NATIONAL 5 - CIBC 10.
# NOTE - THE other_list IS REALLY A NEGATION, WE FILTER BY CHECKING IF THE BRANCH IS NOT IN other_list
other_list <- c(1, 2, 3, 4, 10, credit_union_list)

fsa_list_df <- read.csv(fsa_list, stringsAsFactors = FALSE)

final_df_1 <- data.frame(
  year = numeric(),
  fsa = character(),
  fsa_km = numeric(),
  centroid_lat = numeric(),
  centroid_lng = numeric(),
  branch_count = numeric(),
  branch_density_km2 = numeric(),
  bmo_count = numeric(),
  scotia_count = numeric(),
  rbc_count = numeric(),
  td_count = numeric(),
  cibc_count = numeric(),
  oth_count = numeric(),
  cred_count = numeric(),
  stringsAsFactors = FALSE
)

final_df_2 <- data.frame(
  year = numeric(),
  fsa = character(),
  fsa_km = numeric(),
  centroid_lat = numeric(),
  centroid_lng = numeric(),
  branch_count = numeric(),
  branch_density_km2 = numeric(),
  bmo_count = numeric(),
  scotia_count = numeric(),
  rbc_count = numeric(),
  td_count = numeric(),
  cibc_count = numeric(),
  oth_count = numeric(),
  cred_count = numeric(),
  stringsAsFactors = FALSE
)

final_df_3 <- data.frame(
  year = numeric(),
  fsa = character(),
  fsa_km = numeric(),
  centroid_lat = numeric(),
  centroid_lng = numeric(),
  branch_count = numeric(),
  branch_density_km2 = numeric(),
  bmo_count = numeric(),
  scotia_count = numeric(),
  rbc_count = numeric(),
  td_count = numeric(),
  cibc_count = numeric(),
  oth_count = numeric(),
  cred_count = numeric(),
  stringsAsFactors = FALSE
)


years <- seq(2008, 2018, 1)

row_num <- 0
for (i in years) {
  for (j in 1:nrow(fsa_list_df)) {
    row_num <- row_num + 1
    lodaing_bar(row_num, length(years) * nrow(fsa_list_df))

    fsa_id <- row_num %% nrow(fsa_list_df)
    if (fsa_id == 0) {
      fsa_id <- nrow(fsa_list_df)
    }

    final_df_1[row_num, "year"] <- i
    final_df_2[row_num, "year"] <- i
    final_df_3[row_num, "year"] <- i

    final_df_1[row_num, "fsa"] <- fsa_list_df[fsa_id, "fsa"]
    final_df_2[row_num, "fsa"] <- fsa_list_df[fsa_id, "fsa"]
    final_df_3[row_num, "fsa"] <- fsa_list_df[fsa_id, "fsa"]

    final_df_1[row_num, "fsa_km"] <- fsa_list_df[fsa_id, "area_2011"]
    final_df_2[row_num, "fsa_km"] <- fsa_list_df[fsa_id, "area_2011"]
    final_df_3[row_num, "fsa_km"] <- fsa_list_df[fsa_id, "area_2016"]

    final_df_1[row_num, "centroid_lat"] <- fsa_list_df[fsa_id, "cent_lat_2011"]
    final_df_2[row_num, "centroid_lat"] <- fsa_list_df[fsa_id, "cent_lat_2011"]
    final_df_3[row_num, "centroid_lat"] <- fsa_list_df[fsa_id, "cent_lat_2016"]

    final_df_1[row_num, "centroid_lng"] <- fsa_list_df[fsa_id, "cent_lng_2011"]
    final_df_2[row_num, "centroid_lng"] <- fsa_list_df[fsa_id, "cent_lng_2011"]
    final_df_3[row_num, "centroid_lng"] <- fsa_list_df[fsa_id, "cent_lng_2016"]

    final_df_1[row_num, "branch_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0))
    final_df_2[row_num, "branch_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0))
    final_df_3[row_num, "branch_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_2016 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0))

    ###################

    final_df_1[row_num, "branch_density_km2"] <- final_df_1[row_num, "branch_count"] / final_df_1[row_num, "fsa_km"]

    final_df_2[row_num, "branch_density_km2"] <- final_df_2[row_num, "branch_count"] / final_df_2[row_num, "fsa_km"]

    final_df_3[row_num, "branch_density_km2"] <- final_df_3[row_num, "branch_count"] / final_df_3[row_num, "fsa_km"]

    ###################

    final_df_1[row_num, "bmo_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 1))

    final_df_2[row_num, "bmo_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 1))

    final_df_3[row_num, "bmo_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_20116 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 1))

    ###################

    final_df_1[row_num, "scotia_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 2))
    final_df_2[row_num, "scotia_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 2))
    final_df_3[row_num, "scotia_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_2016 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 2))
    ##################

    final_df_1[row_num, "rbc_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 3))

    final_df_2[row_num, "rbc_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 3))

    final_df_3[row_num, "rbc_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_2016 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 3))
    ##################

    final_df_1[row_num, "td_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 4))

    final_df_2[row_num, "td_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 4))

    final_df_3[row_num, "td_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_2016 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 4))
    ##################

    final_df_1[row_num, "cibc_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 10))

    final_df_2[row_num, "cibc_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 10))

    final_df_3[row_num, "cibc_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_2016 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num == 10))
    ##################

    final_df_1[row_num, "cred_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num %in% credit_union_list))

    final_df_2[row_num, "cred_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num %in% credit_union_list))

    final_df_3[row_num, "cred_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_2016 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      branch_list_df$ID_Num %in% credit_union_list))

    ##################

    final_df_1[row_num, "oth_count"] <- length(which(branch_list_df$Year == final_df_1[row_num, "year"] &
      branch_list_df$FSA == final_df_1[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      !(branch_list_df$ID_Num %in% other_list)))

    final_df_2[row_num, "oth_count"] <- length(which(branch_list_df$Year == final_df_2[row_num, "year"] &
      branch_list_df$exact_fsa_2011 == final_df_2[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      !(branch_list_df$ID_Num %in% other_list)))

    final_df_3[row_num, "oth_count"] <- length(which(branch_list_df$Year == final_df_3[row_num, "year"] &
      branch_list_df$exact_fsa_2016 == final_df_3[row_num, "fsa"] &
      branch_list_df$duplicateFlag == 0 &
      !(branch_list_df$ID_Num %in% other_list)))
  }
}

write.csv(final_df_1, paste(output_directory, "/", "branches_aggregate_fsa_postal.csv", sep = ""), row.names = FALSE)
write.csv(final_df_2, paste(output_directory, "/", "branches_aggregate_fsa_2011_shape.csv", sep = ""), row.names = FALSE)
write.csv(final_df_3, paste(output_directory, "/", "branches_aggregate_fsa_2016_shape.csv", sep = ""), row.names = FALSE)
