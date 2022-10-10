library(tidyverse)

extraParameters <- readRDS("Supplementary/CDFLOW_EP.RDS")
CDFLOW <- read_csv("Final/CDFLOW.csv")

CDFLOW <- CDFLOW %>%
  filter(State=="MN")

test <- extraParameters[,-9]
vec <- paste0(test$COMID, test$ActivityStartDate, test$ActivityStartTime.Time)
test <- test[,8:15]
test[!is.na(test)] <- 1
test[is.na(test)] <- 0
test$count <- rowSums(test)
test$id <- vec

hist(test$count, breaks = 8)

count <- test %>%
  count(count)

extraParameters$id <- paste0(extraParameters$COMID, extraParameters$ActivityStartDate, extraParameters$ActivityStartTime.Time)
CDFLOW$id <- paste0(CDFLOW$COMID, CDFLOW$ActivityStartDate, CDFLOW$ActivityStartTime.Time)

check <- merge(CDFLOW, extraParameters, by = "id") %>%
  select(pCO2.uatm, CO2_uatm, id)

check <- merge(check, test, by = "id")

sum <- check %>%
  filter(count > 0)


sum$percentDiff <- (sum$CO2_uatm-sum$pCO2.uatm)/sum$pCO2.uatm

mean(sum$percentDiff)

median(sum$percentDiff)

hist(sum$percentDiff)

t.test(sum$pCO2.uatm, sum$CO2_uatm)
