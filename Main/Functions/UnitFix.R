# Function to fix units across temp, pH, and ALK

unit_fix <- function(dat) {
  
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
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="mS/cm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="umho/cm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="cfu/100ml"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="g/l"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="ft3/sec"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="days"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="in"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="m"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Temperature, water"&df$ResultMeasure.MeasureUnitCode=="uS/cm"))
  #pH
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="None"&df[,"CharacteristicName"]=="pH"), "std units", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="Normal"&df[,"CharacteristicName"]=="pH"), "std units", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="Mole/l"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="count"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="mV"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="mg/l"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="#/100 gal"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="ug/l"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="0/00"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="deg C"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="S/m"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="m"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="NTU"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="Molar"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="kgal"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="ADMI value"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="nu"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="units/cm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="#/l"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="uS/cm"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="days"))
  df <- df %>%
    filter(!(df$CharacteristicName=="pH"&df$ResultMeasure.MeasureUnitCode=="in"))
  #Total Alk
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3**"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/kg"&df[,"CharacteristicName"]=="Alkalinity, total"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]*19.982, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ueq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]*1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity, total"), df[,"ResultMeasureValue"]*1000, df[,"ResultMeasureValue"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Alkalinity, total"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Alkalinity, total"&df$ResultMeasure.MeasureUnitCode=="NTU"))
  #Alk
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/kg"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l CaCO3"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/kg"&df[,"CharacteristicName"]=="Alkalinity"), "mg/l", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]*19.982, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ueq/L"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity"), df[,"ResultMeasureValue"]*1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="meq/L"&df[,"CharacteristicName"]=="Alkalinity"), "ueq/kgw", df[,"ResultMeasure.MeasureUnitCode"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Alkalinity"&df$ResultMeasure.MeasureUnitCode=="%"))
  df <- df %>%
    filter(!(df$CharacteristicName=="Alkalinity"&df$ResultMeasure.MeasureUnitCode=="NTU"))
 
  #Total ALk and Alk
  df$CharacteristicName <- ifelse(df[,"CharacteristicName"]=="Alkalinity, total","Alkalinity",df[,"CharacteristicName"])
  df <- df %>%
    filter(!(df$CharacteristicName=="Alkalinity"&df$ResultMeasure.MeasureUnitCode=="ppm"))
  
  return(df)
}
