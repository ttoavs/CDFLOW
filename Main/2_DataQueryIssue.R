# Load Libraries ----
# NOTE, this script is need when data just wont download no matter how many time you try. The differnece is that will break it down into 31 queries per state. It is set up for site data because it is the only one that has given me trouble but the same idea would work for the chemical data.
library(dataRetrieval)
library(tidyverse)
library(progress)

# Set up ----
state_vec <- c("FL")
StartDateVec <- c("1990-01-01","1991-01-01","1992-01-01","1993-01-01","1994-01-01","1995-01-01","1996-01-01","1997-01-01","1998-01-01","1999-01-01","2000-01-01","2001-01-01","2002-01-01","2003-01-01","2004-01-01","2005-01-01","2006-01-01","2007-01-01","2008-01-01","2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01")
EndDateVec <- c("1990-12-31","1991-12-31","1992-12-31","1993-12-31","1994-12-31","1995-12-31","1996-12-31","1997-12-31","1998-12-31","1999-12-31","2000-12-31","2001-12-31","2002-12-31","2003-12-31","2004-12-31","2005-12-31","2006-12-31","2007-12-31","2008-12-31","2009-12-31","2010-12-31","2011-12-31","2012-12-31","2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-12-31")
#Arguments
args <- list(statecode = "", 
             siteType="Stream",
             sampleMedia="Water",
             startDateLo="", 
             startDateHi="",
             characteristicName = "pH",
             characteristicName = "Temperature%2C%20water",
             characteristicName = "Alkalinity",
             characteristicName = "Alkalinity%2C%20total"
)

# Loop to download
for(i in state_vec) {
  
  args$statecode <- i
  name <- i

  for(i in 1:length(StartDateVec)) {
    args$startDateLo <- StartDateVec[i]
    args$startDateHi <- EndDateVec[i]
    attempts <- 1
    extracted <- FALSE

    while(extracted==FALSE & attempts <= 100) {
      
      tryCatch(
        saveRDS(whatWQPsites(args), paste0("RawData/Partial Site/",name,i,".RDS")),
        extracted <<- TRUE,
        error = function(e) { 
          extracted <<- FALSE
        }
      )
      
      if(!extracted) {
        attempts <- attempts + 1
        }
      if(!extracted & attempts > 100) {
        next 
        }
    }
  }
}

