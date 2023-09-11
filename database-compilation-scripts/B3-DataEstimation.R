# Load libraries ----
library(phreeqc)
library(progress)
library(tidyverse)

# Bring in Data ----
df <- read_csv("Data/Created/Raw-Obsv.csv")

# Filter parameters ----
df <- df %>%
  filter(`pH.std units`<14, `pH.std units`>5.4, `Temperature, water.deg C`<100, `Temperature, water.deg C`>0)

# PHREEQC setup ----
phrLoadDatabaseString(phreeqc.dat)
phrSetSelectedOutputFileOn(1, TRUE)
fSoln <- function(vec, soln_n, Temp, pH, Alk) {
  
  if (is.na(Temp))  Temp = 25.0
  if (is.na(pH))    pH = 7.0
  if (is.na(Alk))  Alk = 0.0
  
  return(
    c(
      vec,
      paste('  SOLUTION  ', soln_n          ),
      paste('  temp      ', Temp            ),
      paste('  pH        ', pH              ),
      paste('  Alkalinity', Alk, ' ueq/kgw' )
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

# Create progress bar
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = nrow(df),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar



# Setting up csv's to write data to through  out loop, this is a much faster way to handle memory for this long of a loop

names <- c("CO2.mg/l", "pCO2.uatm", "obs_id", "MonitoringLocationIdentifier")
pCO2.df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(pCO2.df) <- names
write.csv(pCO2.df, "Data/Created/pCO2.csv", row.names = FALSE)

# estimate CO2
for(i in 1:nrow(df)) {
  
  skip_to_next <- FALSE
  input <- vector()
  input <- fSelOut(input)
  
  input <- fSoln(input,
                 soln  <- 1,
                 Temp  <- df$`Temperature, water.deg C`[i],
                 pH    <- df$`pH.std units`[i],
                 Alk   <- df$`Alkalinity.ueq/kgw`[i]
  )
  
  phrSetSelectedOutputFileOn(1, TRUE)
  
  tryCatch(
    phrRunString(input),
    error = function(e) { 
      skip_to_next <<- TRUE
    }
  )
  
  if(skip_to_next) { next }
  
  selout <- phrGetSelectedOutput()
  
  pCO2.temp <- data.frame(matrix(ncol = 4))
  colnames(pCO2.temp) <- names
  pCO2.temp$pCO2.uatm <- selout$n1$CO2_uatm
  pCO2.temp$`CO2.mg/l` <- selout$n1$CO2.aq._mg.L
  pCO2.temp$obs_id[1] <- df$obs_id[i]
  pCO2.temp$MonitoringLocationIdentifier[1] <- df$MonitoringLocationIdentifier[i]
  
  write.table(pCO2.temp, "Data/Created/pCO2.csv", sep = ",", col.names = !file.exists("Data/Created/pCO2.csv"), append = T, row.names = FALSE)
  
  pb$tick()
}
