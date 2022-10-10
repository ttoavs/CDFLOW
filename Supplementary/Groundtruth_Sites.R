library(sf)
library(tidyverse)
library(nhdplusTools)
library(progress)

set.seed(123)

Estimated <- readRDS("Estimated.RDS") 
SiteLookup <- readRDS("SiteLookup.RDS")

sites <- Estimated %>%
  distinct(MonitoringLocationIdentifier, lat, long)

sites <- merge(sites, SiteLookup, by = "MonitoringLocationIdentifier") 

keep <- sites[,c("lat","long","MonitoringLocationIdentifier")]

sites <- sites %>%
  st_as_sf(coords = c("long", "lat"), crs = 4269)

sites <- merge(sites, keep, by = "MonitoringLocationIdentifier")

sample <- sites[sample(nrow(sites), 50), ] 

buffer <- .025

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = nrow(sample),
                       complete = "=",   
                       incomplete = "-", 
                       current = "<",    
                       clear = FALSE,   
                       width = 100)

for(i in 1:nrow(sample)){
  
  area <- sf::st_as_sfc(sf::st_bbox(c(xmin = sample$long[i]-buffer, xmax =sample$long[i]+buffer,
                                      ymax = sample$lat[i]+buffer, ymin = sample$lat[i]-buffer), crs = 4269))
  
  possible_comids <- get_nhdplus(area)
  
  choosen_comid <- get_nhdplus(comid = sample$COMID[i])
  
  plot <- ggplot() +
    geom_sf(data = sample$geometry[i], aes(color="MonitoringLocationIdentifier")) +
    geom_sf(data = possible_comids$geometry, aes(color="Possible Comids")) +
    geom_sf(data = choosen_comid$geometry, aes(color="Choosen Comid"))
  
  ggsave(paste0("GroundtruthPlots/plot_",i,".png"), plot)
  pb$tick()
}

check_sheet <- data.frame(cbind(
  plot_num = seq(1,50,1),
  MonitoringLocationIdentifier = sample$MonitoringLocationIdentifier,
  COMID = sample$COMID,
  `CorrectAssignment(yes.no.undecided)` = ""
))

write.csv(check_sheet, "GroundtruthPlots/check_sheet.csv")
