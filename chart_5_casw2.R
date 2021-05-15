# load and install packages
if (!require("pacman")) install.packages("pacman")

# set working directory to cas w2 folder
setwd("C:/Methods-of-Payment surveys/2020/CAS Wave 2")

# import packages
pacman::p_load(tidyverse, haven, labelled, scales)

# import
data_vol <- read_stata("Tables\\Diary\\data\\denom vol_2020.dta", n_max = 10000000) %>% to_factor(., strict = TRUE)

data_val <- read_stata("Tables\\Diary\\data\\denom val_2020.dta", n_max = 10000000) %>% to_factor(., strict = TRUE)

# process data 
data_vol_processed <- data_vol %>% 
  select(year, denom, cash, debit, ctd, credit, ctc, other) %>% 
  pivot_longer(cols = -c(year, denom)) %>% 
  mutate(name = str_to_title(name)) %>% 
  mutate(name = ifelse(name == "Ctc", "CTCC", name)) %>%
  mutate(name = ifelse(name == "Ctd", "CTDC", name)) %>%
  mutate(name = as.factor(name)) %>% 
  mutate(name_2 = fct_relevel(name, c('Cash', 'Credit', 'CTCC', 'Debit', "CTDC", "Other"))) %>%
  mutate(type = "volume")

data_val_processed <- data_val %>% 
  select(year, denom, cash, debit, ctd, credit, ctc, other) %>% 
  pivot_longer(cols = -c(year, denom)) %>% 
  mutate(name = str_to_title(name)) %>% 
  mutate(name = ifelse(name == "Ctc", "CTCC", name)) %>%
  mutate(name = ifelse(name == "Ctd", "CTDC", name)) %>%
  mutate(name = as.factor(name)) %>% 
  mutate(name_2 = fct_relevel(name, c('Cash', 'Credit', 'CTCC', 'Debit', "CTDC", "Other"))) %>%
  mutate(type = "value")

# plot

# vol
data_vol_processed %>% 
  ggplot(aes(as.factor(year), value, fill = forcats::fct_rev(name_2))) + 
  geom_col() +
  labs(fill = "", y = "Share (%)", x = "") +
  facet_wrap(~denom, nrow = 1) +
  scale_fill_manual(values = c("gold1", "lightgreen", "darkgreen","lightcoral", "red3", "dodgerblue3"),
                    guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~denom, nrow = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12)) 

ggsave("Tables\\Diary\\output\\denom_vol.png", height=5.5,width=9,dpi=600,units= "in")

data_val_processed %>% 
  ggplot(aes(as.factor(year), value, fill = forcats::fct_rev(name_2))) + 
  geom_col() +
  labs(fill = "", y = "Share (%)", x = "") +
  facet_wrap(~denom, nrow = 1) +
  scale_fill_manual(values = c("gold1", "lightgreen", "darkgreen","lightcoral", "red3", "dodgerblue3"),
                    guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~denom, nrow = 1) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 12)) +
  scale_y_continuous(expand = c(0, 0))

ggsave("Tables\\Diary\\output\\denom_val.png", height=5.5,width=9,dpi=600,units= "in")
