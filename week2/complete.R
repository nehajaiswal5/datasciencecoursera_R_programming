## Part 2 problem statement


## Solution
######################################################################
setwd("/Users/nkumari/Git/datasciencecoursera_R_programming/week2/")

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  directory_path_delimitor <- ("/")
  files_list <- list.files(directory)
  file_paths_list <- paste(directory, files_list, sep=directory_path_delimitor)
  
  complete_cases_data_vector <-  c(length=length(id))
  counter <- 1
  
  for(i in id){
    filename<-file_paths_list[i]
    ## read each csv files for those ids.
    data_file <- read.csv(filename,header=TRUE, sep=",")
    ##  populate the complete_cases_data_vector with sum of complete cases. 
    complete_cases_data_vector[counter] <- sum(complete.cases(data_file))
    ## increase the counter by 1.
    counter <- counter + 1
  }
  
  data_frame_result <- data.frame(id = id, nobs = complete_cases_data_vector)
  data_frame_result
}


# tests
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
