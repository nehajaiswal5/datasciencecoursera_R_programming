## Part 1 problem statement


## Solution
######################################################################
setwd("/Users/nkumari/Git/datasciencecoursera_R_programming/week2/")
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  directory_path_delimitor <- ("/")
  files_list <- list.files(directory)
  file_paths_list <- paste(directory, files_list, sep=directory_path_delimitor)
  
  if (pollutant == "nitrate" || pollutant == "sulfate") {
    ## initialize the pollutant vector
    pollutant_vector <- c()
    ## iterate through the ids
    for(i in id){
      filename<-file_paths_list[i]
      ## read each csv files for those ids.
      data_file <- read.csv(filename,header=TRUE, sep=",")
      ## remove all na's from the data file vector for this pollutant
      no_na_pollutant_vector <- c(data_file[!is.na(data_file[, pollutant]),pollutant])
      ## keep appending for files retrieved. 
      pollutant_vector <- c(pollutant_vector, no_na_pollutant_vector)
    }
    ## call the mean function.
    getMeanForPollutant(pollutant_vector)
  }else{
    error_msg <- "This pollutant doesn't exists in the csv files!"
    error_msg
  }
  
}
# Generate mean function
getMeanForPollutant <- function(data){
  return (mean(data,na.rm=TRUE))
}

