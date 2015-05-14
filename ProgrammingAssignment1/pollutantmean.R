pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitors ID numbers
  ## to be used.
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result.
  
  # Initialize variables
  meanResult <- c()

  # Loop through the list of monitor-files mentioned in the 'id' vector
  # and read the associated CSV files.
  for (monitorID in id) {
    # Construct the filename using the 'directory' and 'monitorID' vectors
    pathToMonitorFile <- file.path(directory,
                                    paste(sprintf("%03d",
                                    as.numeric(monitorID)),
                                    ".csv",
                                    sep=""))
    
    # read csv data file using the filename constructed earlier
    monitorData <- read.csv(pathToMonitorFile)
    
    # identify missing data
    b <- is.na(monitorData[[pollutant]])
    
    # store pollutant data in vector
    meanResult <- c(meanResult, monitorData[[pollutant]][!b])
  }
  
  mean(meanResult)
}