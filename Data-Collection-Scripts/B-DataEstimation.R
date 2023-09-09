# Load libraries ----
library(phreeqc)
library(progress)
library(tidyverse)

# Bring in Data ----
df <- read_csv("Data/Created/Raw-Obsv.csv")

# Filter parameters ----
df <- df %>%
  filter(`pH.std units`<14, `pH.std units`>5.4, `Temperature, water.deg C`<100, `Temperature, water.deg C`>0, `Alkalinity.ueq/kgw`>1000)

# Calculate scaling parameters ----
df <- df %>%
  mutate(IonicStrength = 1.3e-5*df$`Specific conductance.uS/cm`) %>%
  mutate(pH_Correction = .06+.08*log(IonicStrength)) %>%
  mutate(corrected_pH = `pH.std units`-pH_Correction)

df <- df %>%
  mutate(OrgALK_DOC = `Organic carbon.umol/l`*.8*.1) %>%
  mutate(CarbALK_DOC = `Alkalinity.ueq/kgw`-OrgALK_DOC)

df <- df %>%
  mutate(DOC_TA = 381+1516*exp(-.0038*`Alkalinity.ueq/kgw`)) %>%
  mutate(OrgALK_TA = DOC_TA*.8*.1) %>%
  mutate(CarbALK_TA = `Alkalinity.ueq/kgw`-OrgALK_TA)

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

names <- c("uncorrected","CarbAlk(DOC)&pH","CarbAlk(TA)&pH","CarbAlk(DOC)","CarbAlk(TA)","pH", "obs_id", "MonitoringLocationIdentifier")

pCO2.df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(pCO2.df) <- names
##### Uncomment below ONLY when running for real
#write.csv(pCO2.df, "Data/Created/pCO2.csv", row.names = FALSE)
CO2.df <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(CO2.df) <- names
##### Uncomment below ONLY when running for real
#write.csv(pCO2.df, "Data/Created/CO2.csv", row.names = FALSE)

# Main program loop 
# This program is long and complicated of course there is a more concise and efficient way to do this buuut this works. 
# Essentially observations need to be calculated one at a time in order to make PHREEQC happy so this is how it handled. 


for(i in 1:nrow(df)) {
  
  skip_to_next <- FALSE
  input <- vector()
  input <- fSelOut(input)
  
  Temp <- df$`Temperature, water.deg C`[i]
  Alk1 <- df$`Alkalinity.ueq/kgw`[i]
  Alk2 <- df$CarbALK_DOC[i]
  Alk3 <- df$CarbALK_TA[i]
  pH1 <- df$`pH.std units`[i]
  pH2 <- df$corrected_pH[i]
  
  input <- fSoln(input,
                 soln  <- 1,
                 Temp  <- Temp,
                 pH    <- pH1,
                 Alk   <- Alk1
  )
  input <- fSoln(input,
                 soln  <- 2,
                 Temp  <- Temp,
                 pH    <- pH2,
                 Alk   <- Alk2
  )
  input <- fSoln(input,
                 soln  <- 3,
                 Temp  <- Temp,
                 pH    <- pH2,
                 Alk   <- Alk3
  )
  input <- fSoln(input,
                 soln  <- 4,
                 Temp  <- Temp,
                 pH    <- pH1,
                 Alk   <- Alk2
  )
  input <- fSoln(input,
                 soln  <- 5,
                 Temp  <- Temp,
                 pH    <- pH1,
                 Alk   <- Alk3
  )
  input <- fSoln(input,
                 soln  <- 6,
                 Temp  <- Temp,
                 pH    <- pH2,
                 Alk   <- Alk1
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
  
  pCO2.temp <- data.frame(matrix(ncol = 8))
  colnames(pCO2.temp) <- names
  pCO2.temp[1,1:6] <- selout$n1$CO2_uatm
  pCO2.temp$obs_id[1] <- df$obs_id[i]
  pCO2.temp$MonitoringLocationIdentifier[1] <- df$MonitoringLocationIdentifier[i]
  
  #write.table(pCO2.temp, "Data/Created/pCO2.csv", sep = ",", col.names = !file.exists("Data/Created/pCO2.csv"), append = T, row.names = FALSE)
  
  CO2.temp <- data.frame(matrix(ncol = 8))
  colnames(CO2.temp) <- names
  CO2.temp[1,1:6] <- selout$n1$CO2.aq._mg.L
  CO2.temp$obs_id[1] <- df$obs_id[i]
  CO2.temp$MonitoringLocationIdentifier[1] <- df$MonitoringLocationIdentifier[i]
  
  #write.table(CO2.temp, "Data/Created/CO2.csv", sep = ",", col.names = !file.exists("Data/Created/CO2.csv"), append = T, row.names = FALSE)
  
  pb$tick()
}

# Read in the created csv files, I only continue with pCO2 from here on out because that all I am interested in.
pCO2.df <- read_csv("Data/Created/pCO2.csv")


# Because PHREEQC is a pain in the ass I had to add dummy variables for corrections combinations that did not exists. So, Below I filtered out the pCO2 calculation that used dummy variables.

# Create df possible combinations from starting df
df_metrics <- df %>%
  dplyr::select(`Specific conductance.uS/cm`, `Organic carbon.umol/l`)
df_metrics <- data.frame(ifelse(is.na(df_metrics), 0, 1))
df_metrics$obs_id <- df$obs_id

# filter out observations that did not make it through estimation
df_metrics <- df_metrics %>%
  filter(obs_id %in% unique(pCO2.df$obs_id))

# Below I index and filter a df based on the conditions for all possible correction combinations
combo1 <- pCO2.df[which(df_metrics$Specific.conductance.uS.cm==0&df_metrics$Organic.carbon.umol.l==0),] %>%
  mutate(`CarbAlk(DOC)&pH` = NA, 
         `CarbAlk(TA)&pH` = NA,
         pH = NA,
         `CarbAlk(DOC)` = NA)
combo2 <- pCO2.df[which(df_metrics$Specific.conductance.uS.cm==1&df_metrics$Organic.carbon.umol.l==0),] %>%
  mutate(`CarbAlk(DOC)` = NA,
         `CarbAlk(DOC)&pH` = NA)
combo3 <- pCO2.df[which(df_metrics$Specific.conductance.uS.cm==0&df_metrics$Organic.carbon.umol.l==1),] %>%
  mutate(pH = NA,
         `CarbAlk(TA)&pH` = NA,
         `CarbAlk(DOC)&pH` = NA)
combo4 <- pCO2.df[which(df_metrics$Specific.conductance.uS.cm==1&df_metrics$Organic.carbon.umol.l==1),] %>%
  mutate()

#Finally combine for final estimated data frame!
pCO2_final_df <- bind_rows(combo1, combo2, combo3, combo4) %>%
  rename("est.uncorrected" = uncorrected,
         "est.alk(DOC)&pH" = `CarbAlk(DOC)&pH`,
         "est.alk(TA)&pH" = `CarbAlk(TA)&pH`,
         "est.alk(DOC)" = `CarbAlk(DOC)`,
         "est.alk(TA)" = `CarbAlk(TA)`,
         "est.pH" =  pH) %>%
  select(-(MonitoringLocationIdentifier))

# merge into filtered df
pCO2_final_df <- merge(pCO2_final_df, df, by = "obs_id")

# write csv for final estimated data
##### Uncomment below ONLY when running for real
#write.csv(pCO2_final_df, "Data/Created/estimated-pCO2.csv", row.names = FALSE)