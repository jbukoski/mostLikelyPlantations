library(tidyverse)

dat <- read_csv("../../mel_plantations/planted_forest_summary_20251105.csv")

colnames(dat)

sort(unique(dat$sciName)) 

dat %>% 
  group_by(genus) %>%
  summarize(n = sum(feature_count)) %>%
  arrange(-n) %>%
  View()

dat %>% 
  group_by(genus, species) %>%
  summarize(n = sum(feature_count)) %>%
  arrange(-n) %>%
  View()

