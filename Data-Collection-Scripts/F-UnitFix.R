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
  
  
  # Organic Carbon
  
  # To Filter out
  out <- c("%","none","None")
  df <- df %>%
    filter(!(df$CharacteristicName=="Organic carbon"&df$ResultMeasure.MeasureUnitCode %in% out))
  # g/kg
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="g/kg"&df[,"CharacteristicName"]=="Organic carbon"), df[,"ResultMeasureValue"]/12.011/1000000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="g/kg"&df[,"CharacteristicName"]=="Organic carbon"), "umol/l", df[,"ResultMeasure.MeasureUnitCode"])
  # mg/l
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Organic carbon"), df[,"ResultMeasureValue"]/12.011/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/l"&df[,"CharacteristicName"]=="Organic carbon"), "umol/l", df[,"ResultMeasure.MeasureUnitCode"])
  # mg/kg
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/kg"&df[,"CharacteristicName"]=="Organic carbon"), df[,"ResultMeasureValue"]/12.011/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/kg"&df[,"CharacteristicName"]=="Organic carbon"), "umol/l", df[,"ResultMeasure.MeasureUnitCode"])
  # ug/l
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Organic carbon"), df[,"ResultMeasureValue"]/12.011, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/l"&df[,"CharacteristicName"]=="Organic carbon"), "umol/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  # mg/L
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/L"&df[,"CharacteristicName"]=="Organic carbon"), df[,"ResultMeasureValue"]/12.011/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mg/L"&df[,"CharacteristicName"]=="Organic carbon"), "umol/l", df[,"ResultMeasure.MeasureUnitCode"])
  # ppm
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ppm"&df[,"CharacteristicName"]=="Organic carbon"), df[,"ResultMeasureValue"]/12.011/1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ppm"&df[,"CharacteristicName"]=="Organic carbon"), "umol/l", df[,"ResultMeasure.MeasureUnitCode"])
  # ug/L
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/L"&df[,"CharacteristicName"]=="Organic carbon"), df[,"ResultMeasureValue"]/12.011, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="ug/L"&df[,"CharacteristicName"]=="Organic carbon"), "umol/l", df[,"ResultMeasure.MeasureUnitCode"])
  
  
  # Specific Cond
  # filter out
  out <- c("mg/l","mg/L","umol/l","None","ug/L","%","deg C","NA","mmhos/cm","volts","mg/sec","NTU","ppth","mosm/kg","nu","S/m")
  df <- df %>%
    filter(!(df$CharacteristicName=="Specific conductance"&df$ResultMeasure.MeasureUnitCode %in% out))
  
  # mS/cm
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mS/cm"&df[,"CharacteristicName"]=="Specific conductance"), df[,"ResultMeasureValue"]*1000, df[,"ResultMeasureValue"])
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mS/cm"&df[,"CharacteristicName"]=="Specific conductance"), "uS/cm", df[,"ResultMeasure.MeasureUnitCode"])
  
  # mho/cm
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mho/cm"&df[,"CharacteristicName"]=="Specific conductance"), "uS/cm", df[,"ResultMeasure.MeasureUnitCode"])
  df$ResultMeasureValue <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="mho/cm"&df[,"CharacteristicName"]=="Specific conductance"), df[,"ResultMeasureValue"]*1000, df[,"ResultMeasureValue"])
  
  # umho
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="umho"&df[,"CharacteristicName"]=="Specific conductance"), "uS/cm", df[,"ResultMeasure.MeasureUnitCode"])
  
  # umho/cm
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="umho/cm"&df[,"CharacteristicName"]=="Specific conductance"), "uS/cm", df[,"ResultMeasure.MeasureUnitCode"])
  
  # uS/cm @25C
  df$ResultMeasure.MeasureUnitCode <- ifelse((df[,"ResultMeasure.MeasureUnitCode"]=="uS/cm @25C"&df[,"CharacteristicName"]=="Specific conductance"), "uS/cm", df[,"ResultMeasure.MeasureUnitCode"])
  
  return(df)
}