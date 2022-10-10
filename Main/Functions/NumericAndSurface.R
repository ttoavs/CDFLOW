# Function to clean up data. It will: get rid of non-numeric results, negative values, and non-surface measuments

library(tidyverse)

NumericAndSurface <- function(dat) {
  df <- dat %>%
    mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
    filter(StatisticalBaseCode == "" | is.na(StatisticalBaseCode)) %>%
    filter(!(CharacteristicName == "Alkalinity" & ResultMeasureValue < 0)) %>%
    filter(!(CharacteristicName == "Alkalinity, total" & ResultMeasureValue < 0)) %>%
    filter(`ResultDepthHeightMeasure.MeasureValue` == "" | is.na(`ResultDepthHeightMeasure.MeasureValue`)) %>%
    drop_na(ResultMeasureValue)
  return(df)
}
