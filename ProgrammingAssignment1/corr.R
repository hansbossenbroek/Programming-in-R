corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ## Return a numeric vector of correlations

  # Initialize variables
  cr <- c() 
  
  # Get the files in the directory
  files <- list.files( directory )
  
  # Now loop through all the files
  for(f in 1:length(files)){
    data <- read.csv( paste(directory, "/", files[f], sep="") )
    
    # Only take the compete cases into account
    data <- data[complete.cases(data),]

    if ( nrow(data) > threshold ) {
      cr <- c(cr, cor(data$sulfate, data$nitrate) )
    }
  }
  
  return( cr )

}