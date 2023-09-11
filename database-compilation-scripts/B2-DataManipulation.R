library(tidyverse)
library(progress)
source("database-compilation-scripts/Functions/F-UnitFix.R")

data_vec <- list.files(path = "Data/Downloaded/Chemical", pattern = "*.csv")
Obsv_list <- list()

df_names <- c("obs_id","MonitoringLocationIdentifier","ActivityStartDate","ActivityStartTime.Time","pH.std units","Temperature, water.deg C","Alkalinity.ueq/kgw","State")
col_names <- data.frame(matrix(ncol = 8, nrow=0))
colnames(col_names) <- df_names
write.csv(col_names, "Data/Created/Raw-Obsv.csv", row.names = FALSE)

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(data_vec),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

for(i in 1:length(data_vec)){
  
  # step 1 read in data
  dat <- data.frame(read_csv(paste0("Data/Downloaded/Chemical/", data_vec[i]),
                             lazy = FALSE,
                             progress = FALSE,
                             col_select = c("MonitoringLocationIdentifier",
                                            "ActivityStartDate",
                                            "ActivityStartTime.Time",
                                            "CharacteristicName",
                                            "ResultMeasure.MeasureUnitCode",
                                            "ResultMeasureValue",
                                            "StatisticalBaseCode",
                                            "ResultDepthHeightMeasure.MeasureValue"),
                             col_types = list(
                               MonitoringLocationIdentifier = "c",
                               ActivityStartDate = "T",
                               ActivityStartTime.Time = "t",
                               CharacteristicName = "c",
                               ResultMeasure.MeasureUnitCode = "c",
                               ResultMeasureValue = "n",
                               StatisticalBaseCode = "c",
                               ResultDepthHeightMeasure.MeasureValue = "n")))
  
  # step 2 filter and format
  if(nrow(dat) >1){
    dat <- dat %>%
      mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
      filter(StatisticalBaseCode == "" | is.na(StatisticalBaseCode)) %>%
      filter(!(CharacteristicName == "Alkalinity" & ResultMeasureValue < 0)) %>%
      filter(!(CharacteristicName == "Alkalinity, total" & ResultMeasureValue < 0)) %>%
      filter(`ResultDepthHeightMeasure.MeasureValue` == "" |
               is.na(`ResultDepthHeightMeasure.MeasureValue`) | ResultDepthHeightMeasure.MeasureValue <= .1) %>%
      drop_na(ResultMeasureValue, ResultMeasure.MeasureUnitCode, CharacteristicName)
  }
  
  # step 3 fix units
  if(nrow(dat)>1){
    dat <- unit_fix(dat)
    dat <- dat %>%
      filter(
        (CharacteristicName=="Temperature, water"&ResultMeasure.MeasureUnitCode=="deg C")|
          (CharacteristicName=="pH"&ResultMeasure.MeasureUnitCode=="std units")|
          (CharacteristicName=="Alkalinity"&ResultMeasure.MeasureUnitCode=="ueq/kgw"))
  }
  
  # step 4 rid duplicates
  if(nrow(dat)>1){
    dat <- dat %>%
      mutate(dupe_id = paste0(MonitoringLocationIdentifier, "_",
                              CharacteristicName, "_",
                              ActivityStartDate, "_",
                              `ActivityStartTime.Time`) ) %>%
      group_by(dupe_id) %>%
      slice_sample() %>%
      ungroup()
  }
  
  # step 5 match and form observation
  if(nrow(dat)>1){
    dat <- dat %>%
      mutate(obs_id = paste0(MonitoringLocationIdentifier, "_",
                             ActivityStartDate, "_",
                             `ActivityStartTime.Time`),
             `Chem+unit` = paste0(CharacteristicName, ".",
                                  ResultMeasure.MeasureUnitCode)
      ) %>%
      select(obs_id,
             MonitoringLocationIdentifier,
             ActivityStartDate,
             `ActivityStartTime.Time`,
             `Chem+unit`,
             ResultMeasureValue) %>% #group by key id
      pivot_wider(names_from = `Chem+unit`,
                  values_from = ResultMeasureValue)
    
    vec <- c("Temperature, water.deg C", "pH.std units", "Alkalinity.ueq/kgw")
    
    if(all(vec %in% names(dat))){
      dat <- dat %>%
        drop_na(`Temperature, water.deg C`, `pH.std units`, `Alkalinity.ueq/kgw`)
    } else {
      dat <- tibble()
    }
  }
  
  # step 6 add to Obsv list
  if(nrow(dat)>0){
    dat <- dat %>%
      mutate(State = substr(data_vec[i], start = 1, stop = 2))
    dat[setdiff(names(col_names), names(dat))] <- NA
    col_names[setdiff(names(dat), names(col_names))] <- NA
    dat <- dat[,df_names]
    write.table(dat, "Data/Created/Raw-Obsv.csv", sep = ",", col.names = !file.exists("Data/Created/Raw-Obsv.csv"), append = T, row.names = FALSE)
  }
  pb$tick()
}