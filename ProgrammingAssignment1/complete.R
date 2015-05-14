complete <- function(directory, id = 1:331){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ### id nobs
  ### 1  117
  ### 2  1041
  ### ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Initialize variables
  completes <- rep(0, length(id))
  resultIndex <- 1

  for (monitorID in id) {
    # Construct the filename using the 'directory' and 'monitorID' vectors
    pathToMonitorFile <- file.path(directory,
                                   paste(sprintf("%03d",
                                         as.numeric(monitorID)),
                                         ".csv",
                                         sep=""))
    
    # read csv data file using the filename constructed earlier
    monitorData <- read.csv(pathToMonitorFile, header = TRUE)
    
    # Count the number of complete cases and append to numeric vector
    completes[resultIndex] <- sum(complete.cases(monitorData))
    
    # Store the result in a data.frame
    result <- data.frame(id = id, nobs = completes)
    
    # Increase the index
    resultIndex<-resultIndex + 1
  }
  
  # Return value is a data frame with TWO (2) columns
  print(result)
}