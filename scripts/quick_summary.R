library(tidyverse)

dat <- read_csv("data/planted_forest_summary_20251105.csv")

colnames(dat)

sort(unique(dat$sciName)) 

genus_cts <- dat %>% 
  group_by(genus) %>%
  summarize(n = sum(feature_count)) %>%
  arrange(-n)

genus_cts %>%
  View()

top_genera <- genus_cts %>%
  head(17)

write_csv(top_genera, "./data/top_genera.csv")

dat %>% 
  group_by(genus, species) %>%
  summarize(n = sum(feature_count)) %>%
  arrange(-n) %>%
  View()

