
## A function that takes a directory of data files and a 
## threshold for complete cases and calculates the 
## correlation between sulfate and nitrate for monitor locations 
## where the number of completely observed cases (on all variables)
## is greater than the threshold. 

## The function returns a vector of correlations 
## for the monitors that meet the threshold requirement.
## If no monitors meet the threshold requirement, then 
## the function returns a numeric vector of length 0. 


corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all variables)
  ## required to compute the correlation between nitrate and
  ## sulfate; the default is 0
  
  ## Returns a numeric vector of correlations 
  
  completes <- complete(directory, 1:332)
  completes <- subset(completes, nobs > threshold )
  
  ## Initialize variables
  correlations <- vector()
  
  ## Loop over the passed id's
  for(i in completes$id ) {
    
    ## Pad the i to create a filename
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    ## Load the data
    data <- read.csv(filepath)
    
    ## Calculate and store the count of complete cases
    completeCases <- data[complete.cases(data),]
    count <- nrow(completeCases)
    
    ## Calculate and store the count of complete cases
    ## if threshhold is reached
    if( count >= threshold ) {
      correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  
  ## Return a numeric vector of correlations
  correlations
}
  
  
