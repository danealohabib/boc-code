library(tidyverse)
library(readxl)

# Create a function to clean up a given operating_name list
cleaners <- function(s) {
  s <- s %>%
    str_replace_all("\n", " ") %>%
    str_replace_all("\r", "") %>%
    str_replace_all("pharmacies in franchised locations \\(", "") %>%
    str_replace_all("\\)", "") %>%
    str_split("[/,]")
  s <- s[[1]] %>%
    trimws()

  return(s)
}

df <- read_excel("Copied_PDF.xlsx") %>%
  # Rename as appropriate
  rename(
    operating_name = `Operating Name`,
    legal_name = `Legal Name`
  ) %>%
  # Since we're about to split on ',', save Bed, Bath & Beyond
  mutate(operating_name = str_replace_all(operating_name, "Bed, Bath", "Bed Bath")) %>%
  # Clean up the legal name a bit
  mutate(legal_name = str_replace_all(legal_name, "\r\n", " ")) %>%
  # Get category by locating the missing operating names
  mutate(category_num = cumsum(is.na(operating_name))) %>%
  # And propogate to the others in the category
  group_by(category_num) %>%
  mutate(category = first(legal_name)) %>%
  # and get rid of the rows with just the category name
  filter(!is.na(operating_name)) %>%
  # And don't need category_num any more
  ungroup(category_num) %>%
  select(-category_num)

df <- df %>%
  mutate(rows = row_number()) %>%
  group_by(rows) %>%
  # In each row, create a new row for each operating name
  do(tibble(
    legal_name = .$legal_name[1],
    operating_name = cleaners(.$operating_name),
    category = .$category[1]
  )) %>%
  # and now we don't need rows
  ungroup() %>%
  select(-rows)

head(df)

df2 <- tibble(
  name = c(unique(df$legal_name), unique(df$operating_name))
)

head(df2)

saveRDS(df, "Table1Processed.RData")
saveRDS(df2, "Table2Processed.RData")
write_excel_csv(df, "Table1Processed.csv")
write_excel_csv(df2, "Table2Processed.csv")
