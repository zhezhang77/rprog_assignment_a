## Part 3 [6 Marks]
## Write a function that takes a directory of data files and a threshold for 
## complete cases and calculates the correlation between sulfate and nitrate for 
## monitor locations where the number of completely observed cases (on all 
## variables) is greater than the threshold. The function should return a vector 
## of correlations for the monitors that meet the threshold requirement. If no 
## monitors meet the threshold requirement, then the function should return a 
## numeric vector of length 0. A prototype of this function follows:
##
##        corr <- function(directory, threshold = 0) {
##            ## 'directory' is a character vector of length 1 indicating
##            ## the location of the CSV files
##
##            ## 'threshold' is a numeric vector of length 1 indicating the
##            ## number of completely observed observations (on all
##            ## variables) required to compute the correlation between
##            ## nitrate and sulfate; the default is 0
##
##            ## Return a numeric vector of correlations
##            ## NOTE: Do not round the result!
##        }
## 
## For this function you will need to use the 'cor' function in R which 
## calculates the correlation between two vectors. Please read the help page for 
## this function via '?cor' and make sure that you know how to use it.

source("complete.R")

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    completeVec <- complete(directory)
    
    corrVec <- c(id=integer(), corValue=integer())
    
    for(i in 1:nrow(completeVec)) {
        if (completeVec$nobs[i] > threshold) {
            # read each csv
            filename = paste(directory, "\\", formatC(completeVec$id[i], 
                                width = 3, flag="0"), ".csv", sep="")
            data <- read.csv(filename) #read each csv
            
            # find the complete cases
            data <- na.omit(data)
            corrVec = rbind(corrVec, c(completeVec$id[i],
                            cor(data$sulfate, data$nitrate)))
         }
    }
    
    return(corrVec)
}
