  
  ## Calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors.
  ## The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
  ## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data
  ## from the directory specified in the 'directory' argument and returns the mean of the pollutant
  ## across all of the monitors, ignoring any missing values coded as NA.
  
  
  pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating 
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating 
    ## the name of the pollutant for which we will calculate
    ## the mean; either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicatingi the monitor ID 
    ## numbers to be used
    
    ## Returns the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## The results is rounded to the 3 digit
    
    
    
    ## Get a list of all the 332 filenames (1:332)
    filenames <- list.files(path=directory, pattern="*.csv", full.names = TRUE)
    
    ## Initialize a vector to hold values
    vals <- vector()
    
    ## Loop over the passed id's
    for(i in id) {
      
      ## Pad the i to create a filename
      filename <- sprintf("%03d.csv", i) 
      filepath <- paste(directory, filename, sep="/")
      
      ## Load the data
      data <- read.csv(filepath)
      
      ## Select our column
      d <- data[,pollutant]
      
      ## Ignore NAs
      ##d <- d[!is.na(d)]
      
      ## append to our vector
      vals <- c(vals, d)
    }
    
    ## Returns the mean of all values
    mean(vals, na.rm = TRUE)
  
  }
  
  
  ## Example of results
    ## pollutantmean("~/Documents/Coursera/specdata", "sulfate") 
      ## 3.189
    ## pollutantmean("~/Documents/Coursera/specdata", "sulfate", 1:10)
      ## 4.064
    ## pollutantmean("~/Documents/Coursera/specdata", "nitrate", 70:72)
      ## 1.706
    ## pollutantmean("~/Documents/Coursera/specdata", "nitrate", 23)
      ## 1.281