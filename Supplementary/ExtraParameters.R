#Intro ----
#This scripts includes the code that needs to be added to the main code in order to add additional parameters to the dataset. The only scripts that would need to be edited to reflect the following changes would by querying data and estimated oCO2 (phreeqc), so those are the only things included below. 

# Load libraries ----
library(dataRetrieval)
library(tidyverse)
library(phreeqc)
library(nhdplusTools)
library(progress)
library(janitor)
library(phreeqc)
library(sf)

# Load Functions ----
source("Supplementary/UnitFix_ExtraParameters.R")
source("Main/Functions/NumericAndSurface.R")
source("Main/Functions/DuplicateValues.R")
source("Main/Functions/CreateObservation.R")
source("Main/Functions/DatumFix.R")



# Data Query ---- 
#specify the arguments wanted. Need at least temp, pH, and alkalinity. PHREEQC will take the major dissolved inorganic species into account if given their values
args <- list(statecode = "MN", 
             siteType="Stream",
             sampleMedia="Water",
             startDateLo="1990-01-01", 
             startDateHi="2020-12-31",
             characteristicName = "pH",
             characteristicName = "Temperature%2C%20water",
             characteristicName = "Alkalinity",
             characteristicName = "Alkalinity%2C%20total",
             characteristicName = "Calcium",
             characteristicName = "Sulfate",
             characteristicName = "Magnesium",
             characteristicName = "Sodium",
             characteristicName = "Potassium",
             characteristicName = "Chloride",
             characteristicName = "Nitrate",
             characteristicName = "Phosphate",
             characteristicName = "Ammonium",
             characteristicName = "Carbon%20Dioxide%2C%20free%20CO2",
             characteristicName = "Carbon%20dioxide"
)

extracted <- FALSE
attempts <- 0

while(extracted==FALSE & attempts <= 20) {
  
  tryCatch(
    WQParameters <- readWQPdata(args),
    extracted <<- TRUE,
    error = function(e) { 
      extracted <<- FALSE
    }
  )
  
  if(!extracted) {attempts <- attempts + 1}
  if(!extracted & attempts > 20) { next }
}

saveRDS(WQParameters, "Supplementary/Test.RDS")
WQParameters <- readRDS("Supplementary/Test.RDS")

# Data Manipulation ----
# This follow a similar structure to the main code but has some additions to handle the extra parameters

#Create Numeric and rid non-surfave measuments
WQParameters <- NumericAndSurface(WQParameters)

#Fix units
WQParameters <- UnitFix(WQParameters)

#Duplicate values
WQParameters <- ridDuplicats(WQParameters)

#Create observations
WQParameters <- createOBSV(WQParameters)

#Get rid of obvious error in data
WQParameters <- WQParameters %>%
  filter(`pH.std units`<14, `pH.std units`>5.4, `Temperature, water.deg C`<100, `Temperature, water.deg C`>0) 

#Load phreeqc database
phrLoadDatabaseString(phreeqc.dat)

# turn on selected_output file (now takes two parameters)
phrSetSelectedOutputFileOn(1, TRUE)

# Estimate CO2 ----

# Function for witing solutions
fSoln <- function(vec, soln_n, Temp, pH, Alk, units, Mg, Cl, K, SO4, Ca, Na, NH4, NO3, PO4) {
  
  if (is.na(Temp))  Temp = 25.0
  if (is.na(pH))    pH = 7.0
  if (is.na(Alk))  Alk = 0.0
  if (is.na(Mg))  Mg = 0.0
  if (is.na(Cl))  Cl = 0.0
  if (is.na(K))  K = 0.0
  if (is.na(SO4))  SO4 = 0.0
  if (is.na(Ca))  Ca = 0 
  if (is.na(Na))  Na = 0
  if (is.na(NH4))  NH4 = 0 
  if (is.na(NO3))  NO3 = 0
  if (is.na(PO4))  PO4 = 0
  
  
  
  
  return(
    c(
      vec,
      paste('  SOLUTION  ', soln_n          ),
      paste('  temp      ', Temp            ),
      paste('  pH        ', pH              ),
      paste('  Alkalinity', Alk, ' ueq/kgw' ),
      paste('  units     ', units           ),
      paste('  Mg        ', Mg              ),
      paste('  Cl        ', Cl              ),
      paste('  K         ', K               ),
      paste('  S(6)      ', SO4             ),
      paste('  Ca        ', Ca              ),
      paste('  Na        ', Na              ),
      paste('  N(-3)     ', NH4             ),
      paste('  N(5)      ', NO3             ),
      paste('  P         ', PO4             )
    )
  )
}  

# Create selected_output definition and append to given vector
fSelOut <- function(vec) {
  return (
    c(
      vec,
      'SELECTED_OUTPUT 1                                         ',
      '    -file                selected_output_1.sel           ',
      '    -reset               false                            ',
      '    -percent_error       true                            ',
      'USER_PUNCH                                                ',
      '-heading CO2(aq)_mg/L CO2_uatm                           ',
      '10 PUNCH MOL("CO2") * GFW("CO2") * 1000                   ',
      '20 PUNCH SR("CO2(g)") * 1e6                                '
    )
  )
}

#separate data
mydata <- WQParameters

#In formatting the phreeqc code it will apply to all possible parameters but the queried data may not have return any values for that parameters. IF that is the case it creates an empty column so no error come up. 
cols <- c(
  `Temperature, water.deg C` = NA_real_,
  `pH.std units`= NA_real_, 
  `Alkalinity.ueq/kgw`=NA_real_,
  `Magnesium.mmol/kgw`=NA_real_,
  `Chloride.mmol/kgw`=NA_real_,
  `Potassium.mmol/kgw`=NA_real_,
  `Sulfate.mmol/kgw`=NA_real_,
  `Calcium.mmol/kgw`=NA_real_,
  `Sodium.mmol/kgw`=NA_real_,
  `Ammonium.mmol/kgw`=NA_real_,
  `Nitrate.mmol/kgw`=NA_real_,
  `Phosphate.mmol/kgw`=NA_real_
)

mydata <- add_column(mydata, !!!cols[setdiff(names(cols), names(mydata))])

# create a unique ID (solution number)
mydata$SEQUENCE <-seq.int(nrow(mydata))
mydata$units <- "mmol/kgw"

# create input (as a character vector)
input <- vector()

# add selected_output definition
input <- fSelOut(input)

# for each solution
for (i in 1:nrow(mydata)) {
  input <- fSoln(input,
                 soln  <- mydata[i, "SEQUENCE"],
                 Temp  <- mydata[i, "Temperature, water.deg C"],
                 pH    <- mydata[i, "pH.std units"],
                 Alk   <- mydata[i, "Alkalinity.ueq/kgw"],
                 units <- mydata[i, "units"],
                 Mg   <- mydata[i, "Magnesium.mmol/kgw"],
                 Cl   <- mydata[i, "Chloride.mmol/kgw"],
                 K   <- mydata[i, "Potassium.mmol/kgw"],
                 SO4   <- mydata[i, "Sulfate.mmol/kgw"],
                 Ca    <- mydata[i, "Calcium.mmol/kgw"],
                 Na    <- mydata[i, "Sodium.mmol/kgw"],
                 NH4    <- mydata[i, "Ammonium.mmol/kgw"],
                 NO3    <- mydata[i, "Nitrate.mmol/kgw"],
                 PO4    <- mydata[i, "Phosphate.mmol/kgw"]
  )
}

# turn on selected_output file (now takes two parameters)
phrSetSelectedOutputFileOn(1, TRUE)

# run
phrRunString(input)

#Pull values from output file
selout <- phrGetSelectedOutput()

#Put values into a dataframe
output <- data.frame(selout$n1)

#Add to dataset
final <- cbind(WQParameters, output)

#Get sit data
SiteData <- whatWQPsites(args)

#Keep relavent cols
SiteData <- SiteData %>%
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName)

#Merge site data to chem data
CDFLOW_sites <- merge(final, SiteData, by = "MonitoringLocationIdentifier")

#Function to make all GPS datums CRS: 4269
CDFLOW_sites <- DatumFix(CDFLOW_sites)

#Find unique MonitoringLocationIdentifiers
CDFLOW_sites <- CDFLOW_sites[!duplicated(CDFLOW_sites$MonitoringLocationIdentifier),]

#Make SF object
CDFLOW_sites <- CDFLOW_sites %>%
  st_as_sf(coords = c("long", "lat"), crs = 4269)

#Read in NHD
Flowlines <- read_sf("Data/RawData/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb", layer="NHDFlowline_Network")

#Fin nearest feature
nearest = st_nearest_feature(CDFLOW_sites, st_zm(Flowlines), check_crs = TRUE)
#Calc distance
dist = st_distance(CDFLOW_sites, st_zm(Flowlines[nearest,]), by_element=TRUE)
#Join by nearest feature
join = cbind(CDFLOW_sites, Flowlines[nearest,])
#Add distance column
join$dist <- dist
#limit to joins within 100 meters
join <- join %>%
  filter(as.numeric(dist)<=100)


SiteLookupEP <- st_drop_geometry(join) %>%
  select(COMID, MonitoringLocationIdentifier)

CDFLOW_EP <- merge(final, SiteLookupEP, by = "MonitoringLocationIdentifier")

saveRDS(CDFLOW_EP, "Supplementary/CDFLOW_EP.RDS")
saveRDS(CDFLOW_EP, "Supplementary/SiteLookupEP.RDS")
saveRDS(CDFLOW_EP, "Supplementary/join.RDS")

plot(st_zm(join$Shape))
