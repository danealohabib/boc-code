# load packages
pacman::p_load(tidyverse, sf, janitor, nngeo, scales, reticulate, here)

# run python script to scrape data
reticulate::source_python("misc_script/water_access/scrape_data.py")

# Source functions
source(file = here("functions.R"))

# import data scraped from py script
water_access <- read_csv("misc_script/water_access/data/result.csv") %>%
  clean_names() %>%
  rowid_to_column(., "water_id")

# import distance measures 
distance <- read_csv("data/processed/band/band_summary_stats.csv") %>%
  clean_names()

# import first nation shape files
first_nation_4269_shape <- st_read(
  "data/first_nation",
  "Premiere_Nation_First_Nation"
) %>%
  st_transform(., crs = 4326) %>%
  rowid_to_column(., "row_id") %>%
  clean_names()

# load census boundry files
census_subdivision <- st_read("data/shape_files/census_subdivision/lcsd000b16a_e.shp") %>%
  clean_names() %>%
  st_transform(., crs = 4326)

census_subdivision <- census_subdivision %>%
  mutate(reserve_csds = ifelse(csdtype %in% reserve_csds, "reserve csd", "other csd"))

water_access_sf <- water_access %>%
  st_as_sf(., coords = c("longitude", "lattitude"), crs = 4326) # %>% rowid_to_column(., "water_id")

# nearest water access
water_access_list <- st_nn(water_access_sf, first_nation_4269_shape,
  returnDist = T, k = 1, progress = TRUE
)

water_access_df <- tibble(
  geo_water = unlist(water_access_list[[2]])
) %>%
  rowid_to_column(., "water_id") %>%
  mutate(
    row_id = unlist(water_access_list[[1]]),
    geo_water_km = geo_water / 1000
  ) %>%
  inner_join(water_access) %>%
  inner_join(distance)

# map water locations to census boundary
water_csd_sf <- st_join(water_access_sf, census_subdivision, join = st_intersects)

water_access_df %>%
  ggplot(aes(category, geo_wo_nw, color = category)) +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15) +
  coord_flip() +
  geom_jitter(size = 2, alpha = 0.25, width = 0.10) +
  scale_color_manual(values = c("#00BA38", "#F8766D")) +
  theme_light() +
  guides(fill = FALSE) +
  labs(
    y = "geographic distance to cash",
    x = "long-term drinking water advisory",
    caption = "each point represents a unique band office"
  )

water_access_df %>%
  mutate(travel_500_wo_nw = round(travel_500_wo_nw, 1)) %>%
  ggplot(aes(category, travel_500_wo_nw, color = category)) +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15) +
  coord_flip() +
  geom_jitter(size = 2, alpha = 0.25, width = 0.10) +
  scale_color_manual(values = c("#00BA38", "#F8766D")) +
  theme_light() +
  scale_y_log10(labels = comma) +
  guides(fill = FALSE) +
  geom_hline(yintercept = 100, linetype = "dashed", size = 0.8) +
  labs(
    y = "travel distance to cash",
    x = "long-term drinking water advisory",
    caption = "each point represents a unique band office"
  )

water_access_df %>%
  group_by(category) %>%
  summarise(
    mean_distance_cash = mean(geo_wo_nw),
    median_distance_cash = median(geo_wo_nw),
    sd_distance_cash = sd(geo_wo_nw),
    max_distance_cash = max(geo_wo_nw)
  )
