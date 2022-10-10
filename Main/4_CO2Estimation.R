# Intro ----
# This script estimates pCO2 data from our observations.

# Load libraries ----
library(phreeqc)

# Load data ----
DataObsv <- readRDS("Data/CreatedData/DataObsv.RDS")

# Set up ----
df <- DataObsv
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

# Progress bar
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = nrow(df),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar
#Empty datafraem to add data to throughout the loop
estimated.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("error", "pCO2.mg","pCO2.uatm"))

# Loop for estimation ----
# NOTE this loop will take a long time so finish. The time is dependent on computer power but part of the reason it take so long each each observation is being preformed individualy. The reason it is done this way so to spot observations that throw an error and exclude them from the dataset. This process can be executed much faster by adding multiple solution in at a time but can gets stopped by errors. See script "ExtraParameters.R" for an example of bulk processing.
# Notice the trycatch, this will skip over observations that phreeqc spots an error for
for(i in 1:nrow(df)) {
  
  pb$tick()
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
  estimated.df[i,1:3] <- selout$n1
}

# Merge Data ----
Estimates <- bind_cols(DataObsv, estimated.df) %>%
  select(-error)

# Save Data ---- 
saveRDS(Estimates, "Data/CreatedData/Estimates.RDS")