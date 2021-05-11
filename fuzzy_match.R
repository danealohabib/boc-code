library(tidyverse)
library(stringdist)
library(geosphere)

# Constants
OUTPUT_NAME <- "mc_fcac_joined.csv"
INPUT_FCAC <- "./data/data_fcac.csv"
INPUT_MC <- "./data/mc_data_2.csv"
# Specify the mc target name column
MC_NAME_TARGET <- "owner_name_2"
# The variable to tweak the output
# (TRUE = retain all columns, FALSE = retain only distinct column names)
DROP_COLUMNS <- FALSE
# The variable-flag to save fuzzy matched entries rejected by distance check
SAVE_REJECTED <- TRUE
# The output file name for rejected entries (will be saved if SAVE_REJECTED == TRUE)
REJECTED_OUPUT_NAME <- "rejected_by_distance.csv"
# The target lookup distance [m]
GEO_DISTANCE <- 2000
# The addresses strings distance threshold to reject by geo_distance
ADDR_DIST_THRESH <- 0.2 # Replace with 0 to suppress this check

# The list of reserved owner names
EXACT_OWNERS <- c(
  "TORONTO DOMINION BANK", "ROYAL BANK OF CANADA",
  "NATIONAL BANK OF CANADA", "CANADIAN WESTERN BANK", "CIBC",
  "SCOTIABANK", "BANK OF MONTREAL", "LAURENTIAN BANK OF CANADA",
  "HSBC BANK CANADA"
)

# Load data
fcac <- read_csv(INPUT_FCAC)
mc <- read_csv(INPUT_MC)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data preprocessing
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prepare fcac and mc addresses
fcac <- fcac %>%
  mutate(address_fcac_clean = tolower(address_fcac)) %>%
  mutate(address_fcac_clean = gsub("avenue", "av", address_fcac_clean)) %>%
  mutate(address_fcac_clean = gsub("street", "st", address_fcac_clean)) %>%
  mutate(address_fcac_clean = gsub("road", "rd", address_fcac_clean)) %>%
  mutate(address_fcac_clean = gsub("drive", "dr", address_fcac_clean))

mc <- mc %>%
  mutate(address_mc_clean = tolower(address_mc)) %>%
  mutate(address_mc_clean = gsub("avenue", "av", address_mc_clean)) %>%
  mutate(address_mc_clean = gsub("street", "st", address_mc_clean)) %>%
  mutate(address_mc_clean = gsub("road", "rd", address_mc_clean)) %>%
  mutate(address_mc_clean = gsub("drive", "dr", address_mc_clean))


# Add a row number as index to fcac table
fcac <- fcac %>%
  mutate(row_n = 1:n())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate GEO_DISTANCE bounding box coordinates
# for each entries in `mc` table
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dist_to_bound <- function(coords, dist) {
  # The function computes the coordinates of a
  # square with sides equal to 2 x `dist` and center
  # in `coord`. Coordiantes are: min and max longitude
  # of left and right sides of a square, and min, max latitude
  # of top and bottom sides.
  # @output: a numeric vector with [lon_min, lon_max, lat_min, lat_max]
  out <- c(
    destPoint(coords, 270, dist)[1],
    destPoint(coords, 90, dist)[1],
    destPoint(coords, 180, dist)[2],
    destPoint(coords, 0, dist)[2]
  )
  return(out)
}

# Create bounding box coords matrix for `mc` table
bbox_df <- apply(mc, 1, function(x) {
  coords <- as.numeric(c(x["longitude_mc"], x["latitude_mc"]))
  return(dist_to_bound(coords, GEO_DISTANCE + 1))
})

# Bind bounding box coordinates to mc table
bbox_df <- t(bbox_df)
colnames(bbox_df) <- c("lon_min", "lon_max", "lat_min", "lat_max")
mc <- mc %>%
  bind_cols(as_tibble(bbox_df))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

qgram_diss <- function(str1, str2) {
  qgrm_1 <- dimnames(qgrams(str1, q = 3))[[2]]
  qgrm_1_l <- length(qgrm_1)
  similarity <- sapply(str2, function(x) sum(qgrm_1 %in% dimnames(qgrams(x, q = 3))[[2]]))

  # The procedure for longest number match check
  str1_numbers <- regmatches(str1, gregexpr("\\b\\d{1,}\\b", str1))
  number_match <- 0
  if (length(str1_numbers[[1]])) {
    str1_max <- max(unlist(str1_numbers))
    r_string <- paste0("\\b", str1_max, "\\b")
    number_match <- sapply(grepl(r_string, str2), function(x) {
      return(ifelse(x, 2, -(qgrm_1_l %/% 4)))
    })
  }


  similarity <- 1 - (similarity + number_match) / qgrm_1_l
  diss <- sapply(similarity, function(x) {
    return(min(x, 1))
  })
  # Prevent negative distances (0 is min)
  return(diss)
}


dist_to_proportion <- function(dist, string_a, string_b) {
  # Compute a proportion of changes symbols between string_a and string_b
  # We use Levenstain distance for fuzzy match
  # Levenstain distance = number of deletions, insertions and
  # substitutions necessary to turn string A into string B.
  # The value depends on string length, but we want something comparable among
  # different strings. Therefore we will find a ratio:
  # Levenstain distance / length of longst string

  # Compute strings length
  str_lengt <- c(nchar(string_a), nchar(string_b))
  return(dist / max(str_lengt))
}

string_dist_procedure <- function(mc_address, fcac_sub) {
  # Compute string distance between address in mc entry
  # and all addresses in fcac subset
  # addr_dist <- stringdist(mc_address, fcac_sub$address_fcac, method = "osa")
  addr_dist <- qgram_diss(mc_address, fcac_sub$address_fcac_clean)
  # Get the index of the address with minimal distance from target
  indx <- fcac_sub$row_n[which.min(addr_dist)]
  # Get the distance (measure of matching)
  min_dist <- min(addr_dist)
  # Get the most similar fcac address
  fcac_address <- fcac_sub$address_fcac[which.min(addr_dist)]
  # Compute a proportion of changed characters between addreses
  # change_ratio <- dist_to_proportion(min_dist, mc_address, fcac_address)
  change_ratio <- min_dist
  return(list(
    indx = indx,
    change_ratio = change_ratio
  ))
}

fuzzy_match <- function(row) {
  # The function which will perform fuzzy matching in apply loop.
  # @input: a row from mc table.
  # Steps:
  # get owner name and fsa form mc row
  # get a subset of fcac rows which match name and fsa
  # compute qgram distances between address in mc and all addresses in
  # fcac subset and get the row with the lowest distance = best match
  # check if the distance between matched addresses is lower than GEO_DISTANCE
  # @output: the index of the best matching fcac row and corresponding distance

  mc_owner <- row[MC_NAME_TARGET]
  # mc_owner <- row["owner_name"]
  mc_fsa <- row["fsa"]
  mc_address <- row["address_mc_clean"]
  mc_coords <- as.numeric(row[c("longitude_mc", "latitude_mc")])
  mc_min_lon <- as.numeric(row["lon_min"])
  mc_max_lon <- as.numeric(row["lon_max"])
  mc_min_lat <- as.numeric(row["lat_min"])
  mc_max_lat <- as.numeric(row["lat_max"])

  # Prepare a subset of fcac with matching name and fca
  fcac_sub <- fcac %>%
    filter(owner_name == mc_owner, fsa == mc_fsa | is.na(fsa))

  # In a situation when for some owner_name all fsa = NA,
  # the resulting fca_subset contains only NA values. Check such a situation
  # and prepare subset basing on owner_name only (do not use fsa)
  if (all(!complete.cases(fcac_sub)) & nrow(fcac_sub)) {
    # STEP 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # step 1 and 2 will be further
    # The case when all FSA for some owner_namae == NA
    fcac_sub <- fcac %>%
      # Get only entries with matched owner_name within bounding box of mc
      # Here we permit exact owner_name match OR
      # `owner_name` not in EXACT_OWNERS
      filter(
        owner_name == mc_owner | !(owner_name %in% EXACT_OWNERS),
        mc_max_lat >= latitude_fcac & latitude_fcac >= mc_min_lat,
        mc_max_lon >= longitude_fcac & longitude_fcac >= mc_min_lon
      )

    # If `fcac_sub` is empty then there is not
    # `owner_name` match within GEO_DISTANCE
    # Therefore Return NA
    if (nrow(fcac_sub)) {
      # retain only entries exactly within GEO_DISTANCE
      geo_dists <- distGeo(
        mc_coords,
        as.matrix(fcac_sub[, c("longitude_fcac", "latitude_fcac")])
      )
      fcac_sub <- fcac_sub[geo_dists <= GEO_DISTANCE, ]
      if (!nrow(fcac_sub)) {
        return(c(NA, NA))
      }
      # Compute string distance between address in mc entry
      # and all addresses in fcac subset
      string_dist_res <- string_dist_procedure(mc_address, fcac_sub)
      indx <- string_dist_res$indx
      change_ratio <- string_dist_res$change_ratio
      # Return matched index, change ratio, distance check
      return(c(indx, change_ratio))
    } else {
      return(c(NA, NA))
    }
  } else {
    # Just in case if the `fcac_sub` contains NA
    fcac_sub <- fcac_sub %>%
      filter(!is.na(fsa))
  }

  # Check if subset is not empty
  if (nrow(fcac_sub)) {
    # STEP 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Compute string distance between address in mc entry
    # and all addresses in fcac subset
    string_dist_res <- string_dist_procedure(mc_address, fcac_sub)
    indx <- string_dist_res$indx
    change_ratio <- string_dist_res$change_ratio

    # Return matched index, change ratio, distance check
    return(c(indx, change_ratio))
  } else {
    # STEP 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The fcac subset is empty.
    # The second case = no exact match of `owner_name` and `fsa`.
    # Look for matching `fsa`,
    # then subset entries with geo_distance < GEO_DISTANCE
    # perform fuzzy_matching

    # Get fcac_rows with matching `fsa` and coordinates within `mc` bounding box
    fcac_sub <- fcac %>%
      filter(
        fsa == mc_fsa,
        mc_max_lat >= latitude_fcac & latitude_fcac >= mc_min_lat,
        mc_max_lon >= longitude_fcac & longitude_fcac >= mc_min_lon
      )
    # As we did not find exact `owner_name` matches on previous step, we should
    # look only for owner names that are not in EXACT_OWNERS
    fcac_sub <- fcac_sub %>%
      filter(!(owner_name %in% EXACT_OWNERS))
    # If fcac_sub is empty then there is not `fsa` match within GEO_DISTANCE
    # Return NA
    if (nrow(fcac_sub)) {
      # retain only entries exactly within GEO_DISTANCE
      geo_dists <- distGeo(
        mc_coords,
        as.matrix(fcac_sub[, c("longitude_fcac", "latitude_fcac")])
      )
      fcac_sub <- fcac_sub[geo_dists <= GEO_DISTANCE, ]
      if (!nrow(fcac_sub)) {
        return(c(NA, NA))
      }
      # Compute string distance between address in mc entry
      # and all addresses in fcac subset
      string_dist_res <- string_dist_procedure(mc_address, fcac_sub)
      indx <- string_dist_res$indx
      change_ratio <- string_dist_res$change_ratio
      # Return matched index, change ratio, distance check
      return(c(indx, change_ratio))
    } else {
      return(c(NA, NA))
    }
  }
}

# Apply the fuzzy_match function to each row in mc table
res_raw <- apply(mc, 1, fuzzy_match)

# Prepare an expanded version of fcac table according to matched indexes
res_fcac <- fcac[t(res_raw)[, 1], ]

# Drop owner_name and fsa if DROP_COLUMNS is TRUE
if (DROP_COLUMNS) {
  res_fcac <- res_fcac %>%
    select(-one_of(c("fsa", "owner_name")))
} else {
  res_fcac <- res_fcac %>%
    mutate(fsa_fcac = fsa, owner_name_fcac = owner_name) %>%
    select(-one_of(c("fsa", "owner_name")))
}

res_fcac <- res_fcac %>%
  # change year to year_fcac
  mutate(year_fcac = year) %>%
  # Drop row_n column from results
  select(-one_of(c("row_n", "year"))) %>%
  # Add a column with distances between addresses
  mutate(addr_dist = t(res_raw)[, 2]) %>%
  # Convert addr_dist to strictly positive
  mutate(addr_dist = ifelse(addr_dist < 0, 0, addr_dist))


# Prepare df with entries rejected by distance
result_tmp <- mc %>%
  bind_cols(res_fcac) %>%
  # Add geographical closeness metrics
  rowwise() %>%
  mutate(geo_dist_m = distGeo(
    c(longitude_mc, latitude_mc),
    c(longitude_fcac, latitude_fcac)
  )) %>%
  ungroup()

rejected_df <- result_tmp %>%
  dplyr::filter((geo_dist_m >= GEO_DISTANCE) & !(addr_dist < ADDR_DIST_THRESH)) %>%
  select(all_of(MC_NAME_TARGET), address_mc, address_fcac, geo_dist_m, addr_dist)

# Save rejected entries if SAVE_REJECTED == TRUE
if (SAVE_REJECTED) {
  write.csv(rejected_df,
    file = paste("./output/", REJECTED_OUPUT_NAME, sep = "")
  )
}

# Replace rows with geo_dist > 3000 with NA
res_fcac[result_tmp$geo_dist_m >= GEO_DISTANCE &
  !(result_tmp$addr_dist < ADDR_DIST_THRESH) &
  !is.na(res_fcac$address_fcac) &
  !is.na(result_tmp$geo_dist_m), ] <- NA
# Combine mc and matched rows
result <- mc %>%
  bind_cols(res_fcac) %>%
  select(-address_mc_clean, -address_fcac_clean) %>%
  # Add geographical closeness metrics
  rowwise() %>%
  mutate(geo_dist = distGeo(
    c(longitude_mc, latitude_mc),
    c(longitude_fcac, latitude_fcac)
  )) %>%
  ungroup() %>%
  mutate(error_match = ifelse(major_banks_cba == 1 & owner_name_fcac == "CU", 1, 0)) %>%
  filter(addr_dist < .5)

result %>%
  filter(!is.na(address_fcac)) %>%
  filter(addr_dist < .5) %>%
  select(address_mc, address_fcac, major_banks_cba, owner_name, owner_name_fcac, addr_dist, geo_dist, error_match) %>%
  View()
# Save results
write.csv(result, file = paste("./output/", OUTPUT_NAME, sep = ""))

result %>%
  filter(!is.na(address_fcac)) %>%
  filter(addr_dist < .5) %>%
  select(address_mc, address_fcac, major_banks_cba, owner_name, owner_name_fcac, addr_dist, geo_dist, error_match) %>%
  filter(error_match == 0) %>%
  filter(major_banks_cba == 1)

# Print a statistics on matched and rejected entries
pref_retained <- "Numer of retained matched fcac entries: "
pref_rejected <- "Numer of fcac entries rejected by distance: "
print(paste0(pref_retained, sum(!is.na(result$fsa_fcac))))
print(paste0(pref_rejected, nrow(rejected_df)))
