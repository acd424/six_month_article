library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# sci notation
options(scipen = 9999999)

# Google mobility data downloaded 27 October 2020.
# https://datastudio.google.com/reporting/a529e043-e2b9-4e6f-86c6-ec99a5d7b9a4/page/yY2MB?s=ho2bve3abdM
mob_df <- read_csv("data/Global_Mobility_Report.csv")

# UK only
table(mob_df$country_region)

uk_df <- mob_df %>% 
  filter(country_region == "United Kingdom")

# Identify Scotland subregions (read in manual list).
scot_ni_df <- read_csv("data/scot_ni_subregions.csv", col_names = F)

scot_ni_df <- scot_ni_df %>% 
  rename(sub_region = X1,
         country = X2) 

# Filter them out
`%nin%` <- Negate(`%in%`)
ew_df <- uk_df %>%
  filter(sub_region_1 %nin% scot_ni_df$sub_region,
         sub_region_1 != "Greater Manchester")

# Wide to long
ew_long_df <- ew_df %>%
  rename(`Retail & recreation` = retail_and_recreation_percent_change_from_baseline,
         `Grocery & pharmacy` = grocery_and_pharmacy_percent_change_from_baseline,
         Parks = parks_percent_change_from_baseline,
         `Transit stations` = transit_stations_percent_change_from_baseline,
         Workplaces = workplaces_percent_change_from_baseline,
         Residential = residential_percent_change_from_baseline) %>%
  select(-country_region_code, -country_region, -sub_region_2, -metro_area:-census_fips_code) %>% 
  pivot_longer(cols = c(-date, -sub_region_1), names_to = "type", values_to = "perc") %>% 
  drop_na(perc) # !!!!!!!

# check for potential outliers
outlier_df <- ew_long_df %>% 
  arrange(desc(perc))

ggplot(data = outlier_df) +
  geom_histogram(mapping = aes(x = perc), bins = 60)

# Calculate mean by day.
ew_meand_df <- ew_long_df %>% 
  group_by(date, type) %>% 
  summarise(perc = mean(perc)) %>% 
  ungroup()

# Plot function.
plot_fun <- function(x){
  ggplot(data = x) +
    theme_bw() +
    geom_line(mapping = aes(x = date, y = perc, group = type, colour = type)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
          legend.position = "bottom") 
}


# Plot.
p1 <- plot_fun(ew_meand_df) +
  labs(x = NULL, y = "Mean \n change \n from \n baseline \n (%)", colour = NULL) 

ggsave(plot = p1, filename = "visuals/mobility_day_mean_ew.png", height = 14, width = 20, units = "cm")

# Sum by day as per https://datastudio.google.com/reporting/a529e043-e2b9-4e6f-86c6-ec99a5d7b9a4/page/yY2MB?s=ho2bve3abdM.
ew_sumd_df <- ew_long_df %>% 
  group_by(date, type) %>% 
  summarise(perc = sum(perc)) %>% 
  ungroup()

p2 <- plot_fun(ew_sumd_df) +
  labs(x = NULL, y = "Sum \n change \n from \n baseline \n (%)", colour = NULL) 

ggsave(plot = p2, filename = "visuals/mobility_day_sum_ew.png", height = 14, width = 20, units = "cm")

# Calculate mean by month.
ew_meanm_df <- ew_long_df %>%
  separate(col = date, into = c("year","month","day"), sep = "-") %>% 
  rename(date = month) %>% 
  group_by(date, type) %>% 
  summarise(perc = median(perc)) %>% 
  ungroup()

# Plot.
p3 <- plot_fun(ew_meanm_df) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 7, linetype = "dashed") +
  annotate(geom = "curve", x = 3.5, xend = 1.1, y = 90, yend = 90, curvature = 0,
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "curve", x = 4.5, xend = 6.9, y = 90, yend = 90, curvature = 0,
           arrow = arrow(length = unit(1, "mm"))) +
  geom_text(x = 4, y = 90, label = "Study period", size = 2) +
  scale_x_discrete(labels = c(" ", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep",
                              "Oct")) +
  labs(x = NULL, y = "Median \n change \n from \n baseline \n (%)", colour = NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_text(hjust = 1.6),
        legend.position = "bottom")
p3

ggsave(plot = p3, filename = "visuals/mobility_month_median_ew.png", height = 10, width = 16, units = "cm")

# Sum by month.
ew_summ_df <- ew_long_df %>% 
  separate(col = date, into = c("year","month","day"), sep = "-") %>% 
  rename(date = month) %>% 
  group_by(date, type) %>% 
  summarise(perc = sum(perc)) %>% 
  ungroup()

# Plot
p4 <- plot_fun(ew_summ_df) +
  scale_x_discrete(labels = c(" ", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep",
                              "Oct")) +
  labs(x = NULL, y = "Sum \n change \n from \n baseline \n (%)", colour = NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x  = element_text(hjust = 1.75),
        legend.position = "bottom") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 7, linetype = "dashed") +
  annotate(geom = "curve", x = 3.5, xend = 1.1, y = 750000, yend = 750000, curvature = 0,
           arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "curve", x = 4.5, xend = 6.9, y = 750000, yend = 750000, curvature = 0,
           arrow = arrow(length = unit(1, "mm"))) +
  geom_text(x = 4, y = 750000, label = "Study period", size = 2) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom")

ggsave(plot = p4, filename = "visuals/mobility_month_sum_ew.png", height = 10, width = 16, units = "cm")



