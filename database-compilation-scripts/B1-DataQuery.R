####################################################
########### Data Query Script ######################
####################################################

# Load Libraries
library(dataRetrieval)
library(tidyverse)
library(progress)

# set up
# Create vector of state abbreviations
state_vec <- state.abb[!state.abb %in% c("HI","AK")]
state_vec[49] <- "DC"

# Create vector of start dates
StartDateVec <- c("1990-01-01","1991-01-01","1992-01-01","1993-01-01","1994-01-01","1995-01-01","1996-01-01","1997-01-01","1998-01-01","1999-01-01","2000-01-01","2001-01-01","2002-01-01","2003-01-01","2004-01-01","2005-01-01","2006-01-01","2007-01-01","2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01")
# Create vector of end dates
EndDateVec <- c("1990-12-31","1991-12-31","1992-12-31","1993-12-31","1994-12-31","1995-12-31","1996-12-31","1997-12-31","1998-12-31","1999-12-31","2000-12-31","2001-12-31","2002-12-31","2003-12-31","2004-12-31","2005-12-31","2006-12-31","2007-12-31","2008-12-31","2009-12-31","2010-12-31","2011-12-31","2012-12-31","2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-12-31")

# Data frame used for referencing values for main loop
sample.df <- tibble()

for(i in 1:length(state_vec)){
  temp.df <- data.frame(cbind(
    state = state_vec[i],
    start = StartDateVec,
    end = EndDateVec
  ))
  temp.df$sample <- paste0(temp.df$state,seq(1,31,1))
  temp.df$files <- paste0(temp.df$sample, ".csv")
  sample.df <- rbind(sample.df, temp.df)
}

# Check to see which files are already downloaded and set the difference

files <- list.files(path = "Data/Downloaded/Site", pattern = "*.csv")
missing <- setdiff(paste0(sample.df$sample, ".csv"), files)
sample.df <- sample.df %>%
  filter(files %in% missing)


# Loop set up
# Arguments that remain constant throughout the query loop
args <- list(statecode = "", 
             siteType="Stream",
             sampleMedia="Water",
             characteristicName = "pH",
             characteristicName = "Temperature%2C%20water",
             characteristicName = "Alkalinity",
             characteristicName = "Alkalinity%2C%20total"
)


# Create progress bar object
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = nrow(sample.df),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

# Keep track of attempted downloads
attempts.df <- data.frame(matrix(nrow = nrow(sample.df), ncol = 2))
colnames(attempts.df) <- c("sample","attempts")
attempts.df$sample <- sample.df$sample


# Download loop site data ----
for(i in 1:nrow(sample.df)) {
  
  # Update arguments for loop
  args$statecode <- sample.df$state[i] # state
  name <- sample.df$state[i] # keep for later
  args$startDateLo <- sample.df$start[i] # begin date
  args$startDateHi <- sample.df$end[i] # end date
  attempts <- 0 # set attemots to 0
  extracted <- FALSE # set extract to false
  
  # The download api is called in a while loop so that it keeps trying until it is successful or the maximum number of attempts is reached.
  while(extracted==FALSE & attempts < 50) {
    tryCatch(
      write.csv(readWQPdata(args), paste0("Data/Downloaded/Chemical/",sample.df$sample[i],".csv")),
      extracted <<- TRUE,
      error = function(e) { 
        extracted <<- FALSE
      }
    )
    if(!extracted) {
      attempts <- attempts + 1
    }
  }
  
  # update attempts data frame
  attempts.df$attempts <- attempts
  
  # progress bar update
  pb$tick()
}


#############################################################

# Download loop site data 
for(i in 1:nrow(sample.df)) {
  args$statecode <- sample.df$state[i]
  name <- sample.df$state[i]
  args$startDateLo <- sample.df$start[i]
  args$startDateHi <- sample.df$end[i]
  attempts <- 0
  extracted <- FALSE
  while(extracted==FALSE & attempts < 50) {
    tryCatch(
      write.csv(whatWQPsites(args), paste0("Data/Downloaded/Site/",sample.df$sample[i],".csv")),
      extracted <<- TRUE,
      error = function(e) { 
        extracted <<- FALSE
      }
    )
    if(!extracted) {
      attempts <- attempts + 1
    }
  }
  attempts.df$attempts <- attempts
  pb$tick()
}
