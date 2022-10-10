UnitFix <- function(dat) {

  df <- dat

  #Temp
  df$ResultMeasureValue <- ifelse(df[,"ResultMeasure.MeasureUnitCode"]=="deg F", (df[,"ResultMeasureValue"]-32)*(5/9), df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse(df[,"ResultMeasure.MeasureUnitCode"]=="deg F", "deg C", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="mg/l"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="None"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="Deg"))
  #pH
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="None"&df[,"CharacteristicName"]=="pH"), "std units", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="Mole/l"))
  #Magnesium
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Magnesium"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Magnesium"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Magnesium"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Magnesium"&df$ResultMeasure.MeasureUnitCode=="mg/kg"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Magnesium"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Magnesium"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"CharacteristicName"]=="Magnesium"&df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Magnesium"&df[,"ResultMeasure.MeasureUnitCode"]=="mmol/kgw"), df[,"ResultMeasureValue"]/24.312, df[,"ResultMeasureValue"])
  #Calcium
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Calcium"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Calcium"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Calcium"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Calcium"&df$ResultMeasure.MeasureUnitCode=="mg/kg"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Calcium"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Calcium"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Calcium"&df$ResultMeasure.MeasureUnitCode=="mg/l CaCO3"))
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"CharacteristicName"]=="Calcium"&df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Calcium"&df[,"ResultMeasure.MeasureUnitCode"]=="mmol/kgw"), df[,"ResultMeasureValue"]/40.08, df[,"ResultMeasureValue"])
  #Sodium
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Sodium"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Sodium"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Sodium"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Sodium"&df$ResultMeasure.MeasureUnitCode=="mg/kg"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Sodium"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Sodium"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"CharacteristicName"]=="Sodium"&df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Sodium"&df[,"ResultMeasure.MeasureUnitCode"]=="mmol/kgw"), df[,"ResultMeasureValue"]/22.9898, df[,"ResultMeasureValue"])
  #Chloride
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Chloride"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Chloride"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Chloride"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Chloride"&df$ResultMeasure.MeasureUnitCode=="mg/kg"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Chloride"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Chloride"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"CharacteristicName"]=="Chloride"&df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Chloride"&df[,"ResultMeasure.MeasureUnitCode"]=="mmol/kgw"), df[,"ResultMeasureValue"]/35.453, df[,"ResultMeasureValue"])
  #Sulfate
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Sulfate"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Sulfate"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Sulfate"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Sulfate"&df$ResultMeasure.MeasureUnitCode=="mg/kg"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Sulfate"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Sulfate"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"CharacteristicName"]=="Sulfate"&df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Sulfate"&df[,"ResultMeasure.MeasureUnitCode"]=="mmol/kgw"), df[,"ResultMeasureValue"]/96.064, df[,"ResultMeasureValue"])
  #Potassium
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Potassium"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Potassium"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Potassium"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Potassium"&df$ResultMeasure.MeasureUnitCode=="mg/kg"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Potassium"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Potassium"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"CharacteristicName"]=="Potassium"&df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Potassium"&df[,"ResultMeasure.MeasureUnitCode"]=="mmol/kgw"), df[,"ResultMeasureValue"]/39.102, df[,"ResultMeasureValue"])
  #Carbon dioxide
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Carbon dioxide"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Carbon dioxide"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  #Total Alk
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3**"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]*19.982, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ueq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  #Alk
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]*19.982, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ueq/L"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  #Total ALk and Alk
  df$CharacteristicName <- ifelse(df[,"CharacteristicName"]=="Alkalinity, total","Alkalinity",df[,"CharacteristicName"])
  #Nitrate
  df <- df %>%
    filter(!(df$CharacteristicName=="Nitrate"&df$ResultMeasure.MeasureUnitCode=="mg N/l******"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Nitrate"&df$ResultMeasure.MeasureUnitCode=="mg/l as N"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Nitrate"&df$ResultMeasure.MeasureUnitCode=="ppb"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Nitrate"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l asNO3"&df[,"CharacteristicName"]=="Nitrate"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Nitrate"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Nitrate"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Nitrate"&df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"), df[,"ResultMeasureValue"]/62.0049, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Nitrate"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  #Ammonium
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Ammonium"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasureValue <- ifelse((df[,"CharacteristicName"]=="Ammonium"), df[,"ResultMeasureValue"]/18.04, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"CharacteristicName"]=="Ammonium"), "mmol/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Ammonium"&df$ResultMeasure.MeasureUnitCode=="ueq/L"))
  
  return(df)
}