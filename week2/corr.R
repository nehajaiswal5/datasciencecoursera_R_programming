## Part 3 problem statement


## Solution
######################################################################
setwd("/Users/nkumari/Git/datasciencecoursera_R_programming/week2/")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  directory_path_delimitor <- ("/")
  files_list <- list.files(directory)
  file_paths_list <- paste(directory, files_list, sep=directory_path_delimitor)
  
  ## Get the complete table from complete cases function.
  get_complete_data_part2 <- complete("specdata", 1:332)
  ## get all nobs counts
  nobs <- get_complete_data_part2$nobs
  ## get all ids where nobs is greater than threshold.
  ids <- get_complete_data_part2$id[nobs > threshold]
  
  corr_data_vector <-  c(length=length(ids))
  counter <- 1
  
  for(i in ids){
    filename<-file_paths_list[i]
    ## read each csv files for those ids.
    data_file <- read.csv(filename,header=TRUE, sep=",")
    ##  populate the complete_cases_data_vector using corr function. 
    corr_data_vector[counter] <- cor(data_file$sulfate, data_file$nitrate, use="complete.obs")
    ## increase the counter by 1.
    counter <- counter + 1
  }
  
  data_frame_result <- corr_data_vector
  data_frame_result
 
}

## Tests
cr <- corr("specdata",150)
head(cr)
cr <- corr("specdata", 2000)
