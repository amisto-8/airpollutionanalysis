  

  ## A function that reads a directory full of files and reports the number of completely observed cases in each data file.
  ## The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.
  
  
  complete<- function(directory, id = 1:332) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers 
    ## to be used
    
    ## Returns a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041 ...
    ## ...
    ## 'id' = the monitor ID number and 'nobs' = number of complete cases
    
    
    ## Get a list of all the 332 filenames (1:332)
    filenames <- list.files(path=directory, pattern="*.csv", full.names = TRUE)
    
    ## Initialize a vector to hold values
    nobs <- numeric()
    
    ## Loop over the passed id's
    for(i in id) {
      
      ## Pad the i to create a filename
      filename <- sprintf("%03d.csv", i) 
      filepath <- paste(directory, filename, sep="/")
      
      ## Load the data
      data <- read.csv(filepath)
      
      nobs <- c(nobs, sum(complete.cases(data)))
           
    }
    
    data.frame(id, nobs)
  }
  
  
  
  

