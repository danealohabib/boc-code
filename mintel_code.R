
rm(list = ls(all = TRUE))
gc(reset = TRUE)
set.seed(12345)

library(data.table)
library(ggplot2)

# (1) reading data--------------------------------------------------------------
dt1 <- fread("data/ia_20201103_123857.csv", sep = ",", skip = 14)
dt1 <- dt1[, 25:94]

dt2 <- fread("data/ia_20201104_121112.csv", sep = ",", skip = 14)
dt2 <- dt2[, 26:95]

dt3 <- fread("data/ia_20201104_122558.csv", sep = ",", skip = 16, fill = TRUE)
dt3 <- dt3[, 24:93]

dt4 <- fread("data/ia_20201103_120948.csv", sep = ",", skip = 14, fill = TRUE)
dt4 <- dt4[, 15:84]

# binding data together
dt <- rbindlist(list(
  dt1, dt2, dt3, dt4
))

# remove extraneous data from memory
rm(dt1, dt2, dt3, dt4)

# adding key columns for plotting to match old/reference data
dt[, `:=`(
  year = as.integer(`Year`),
  annualfee = as.numeric(`Annual Fee ($)`),
  rewards = as.logical(ifelse(`Rewards` == "Yes", TRUE, ifelse(`Rewards` == "No", FALSE, NA))),
  averagepurchaseregularapr = as.numeric(`Purchase Regular APR (%)`)
)][, annualfee := ifelse(annualfee > 100, 100, annualfee)]

# saving new data in a single file
fwrite(dt, "output/data/canada_credit_card_2014_2020.csv")

# (2) plots generation----------------------------------------------------------

# (a) distribution of ARP fees
ggplot(dt, aes(x = averagepurchaseregularapr)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), fill = "steelblue4", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.10)) +
  ggtitle("Distribution of ARP") +
  xlab("APR") +
  ylab("Fraction") +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# save pdf of plot
ggsave("output/plots/figure_01.pdf", dpi = 600)

# (b) distribution of annual fees
ggplot(dt, aes(x = annualfee)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), fill = "steelblue4", binwidth = 2) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.10)) +
  annotate("text",
    x = 0, y = 0.78, hjust = 0, parse = TRUE,
    label = 'bold(NOTE): "Annual fee is truncated and summed at $100"'
  ) +
  ggtitle("Distribution of Annual Fees") +
  xlab("Annual Fee") +
  ylab("Fraction") +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# save pdf of plot
ggsave("output/plots/figure_02.pdf", dpi = 600)

# (c) fraction of low rate offers

# create data for plot
dt_c <- dt[, .(lowrate = sum(ifelse(averagepurchaseregularapr <= 15, 1, 0), na.rm = TRUE) / .N), by = year]

# plot low rate
ggplot(dt_c, aes(x = year, y = lowrate)) +
  geom_line(color = "steelblue4") +
  scale_x_continuous(breaks = seq(2014, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(0, 0.05, by = 0.01)) +
  # annotate("text", x = 2014, y = 0.028, hjust = 0, parse = TRUE, label = 'bold(NOTE): "A low rate card has an APR less than or equal to 15%"') +
  ggtitle("Fraction of Low Rate Offers") +
  xlab("Year") +
  ylab("Fraction") +
  labs(caption = "A low rate card has an APR less than or equal to 15%") +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# save figure 3
ggsave("output/plots/figure_03.pdf", dpi = 600)

# (d) fraction of reward offers

# create data for plot
dt_d <- dt[, .(fraction = sum(rewards, na.rm = TRUE) / .N), by = year]

# create plot
ggplot(dt_d, aes(x = year, y = fraction)) +
  geom_line(color = "steelblue4") +
  scale_x_continuous(breaks = seq(2014, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(0, 0.20, by = 0.05)) +
  ggtitle("Fraction of Reward Offers") +
  xlab("Year") +
  ylab("Fraction") +
  theme_light() +
  theme(panel.grid.minor = element_blank())

# save figure 4
ggsave("output/plots/figure_04.pdf", dpi = 600)
