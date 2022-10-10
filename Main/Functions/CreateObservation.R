# Function to create observation from water quality metrics taken at the same date, time, and location

library(tidyverse)


createOBSV <- function(dat) {
  
  df <- dat
  
  df$obs_id <- paste(df$MonitoringLocationIdentifier, "_", df$ActivityStartDate, "_", df$`ActivityStartTime.Time`)
  
  df$`Chem+unit` <- paste0(df$CharacteristicName, ".", df$ResultMeasure.MeasureUnitCode)
  
  df <- df %>% 
    select(obs_id, MonitoringLocationIdentifier, ActivityStartDate, `ActivityStartTime.Time`, `Chem+unit`, ResultMeasureValue) %>% #group by key id
    pivot_wider(names_from = `Chem+unit`, values_from = ResultMeasureValue)
  
  df <- df %>%
    drop_na(`Temperature, water.deg C`, `pH.std units`, `Alkalinity.ueq/kgw`)
  
  return(df)
}