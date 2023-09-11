# custom function ----
unit_fix <- function(dat) {
  
  df <- dat
  
  #Temp
  
  # convert F to C
  df$ResultMeasureValue <- ifelse(df[,"ResultMeasure.MeasureUnitCode"]=="deg F", (df[,"ResultMeasureValue"]-32)*(5/9), df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse(df[,"ResultMeasure.MeasureUnitCode"]=="deg F", "deg C", df[,"ResultMeasure.MeasureUnitCode"])
  
  # filter out
  out <- c("mg/l","mg/L","umol/l","None","ug/L","Deg","%","mS/cm","umho/cm","cfu/100ml","g/l","ft3/sec","days","in","m","uS/cm","NTU","cfu/100mL","cfs","g/L")
  
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode %in% out))
  
  #pH
  
  # assumed to be std
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="None"&df[,"CharacteristicName"]=="pH"), "std units", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="Normal"&df[,"CharacteristicName"]=="pH"), "std units", df[,"ResultMeasure.MeasureUnitCode"])
  
  # filter out
  
  out <- c("Mole/l","count","mV","mg/l","#/100 gal","ug/l","0/00","deg C","S/m","m","mg/L","NTU","Molar","ppm","kgal","ADMI value","nu","#/l","%","uS/cm","days","in","Mole/L","cfs","#/L","NA","ug/L","units/cm")
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode %in% out))
  
  
  #Total Alk
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/L"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/L"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # inconsistent name
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3**"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/kg"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/L"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert to miliequivelent per liter
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]*19.982, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  
  # inconsistent name
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ueq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]*1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  
  # filter out
  out <- c("%","NTU","ppm")
  df <- df %>%
    filter(!(df$CharacteristicName=="Alkalinity, total"&df$ResultMeasure.MeasureUnitCode %in% out))
  
  
  #Alk
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/L"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/L"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # inconsistent name
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/kg"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/L"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert to miliequivelent per liter
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]*19.982, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  
  # inconsistent name
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ueq/L"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  
  # convert
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]*1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  
  # filter out
  out <- c("%","NTU","ppm")
  df <- df %>%
    filter(!(df$CharacteristicName=="Alkalinity"&df$ResultMeasure.MeasureUnitCode %in% out))
  
  #Total ALk and Alk
  df$CharacteristicName <- ifelse(df[,"CharacteristicName"]=="Alkalinity, total","Alkalinity",df[,"CharacteristicName"])
  
  return(df)
}