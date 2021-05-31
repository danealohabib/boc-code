library(tidyverse)
library(janitor)

rbc <- read_csv("rbc.csv") %>%
  rename(n_atms = `Number of ATMs`) %>%
  clean_names()

rbc_clean <- rbc %>%
  separate(address, into = c("location", "address", "city"), sep = "\n", extra = "merge") %>%
  separate(city, into = c("city", "province", "postal_code"), sep = ",", fill = "right") %>%
  mutate(closed = ifelse(location == "Important", 1, 0),
         type = ifelse(type == "branchatm", "branch_atm", type),
         province = str_squish(province),
         postal_code = str_squish(postal_code),
         unique_id = str_extract(url, '(?<==).*')) %>% 
  select(url, unique_id, everything())

rbc_clean %>% 
  write_csv("rbc_clean.csv")
