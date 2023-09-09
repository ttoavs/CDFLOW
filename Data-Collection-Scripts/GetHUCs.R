library(nhdplusTools)
library(sf)
library(tidyverse)

sites <- read_csv("Data/Created/Site-Lookup.csv")
sites <- sites %>%
  mutate(HUC_10 = substr(HUC_12, 1, 10),
         HUC_8 = substr(HUC_12, 1, 8),
         HUC_6 = substr(HUC_12, 1, 6),
         HUC_4 = substr(HUC_12, 1, 4),
         HUC_2 = substr(HUC_12, 1, 2))
HUC8s <- get_huc8(id = unique(sites$HUC_8))
sites_withHUC8 <- merge(HUC8s, sites, by.x = "huc8", by.y = "HUC_8")
test <- st_simplify(sites_withHUC8, dTolerance = 1000)
saveRDS(test, "Data/Created/sites_with_HUCS.RDS")
