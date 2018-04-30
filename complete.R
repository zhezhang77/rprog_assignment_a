## Part 2 [6 Marks]
## Write a function that reads a directory full of files and reports the number
## of completely observed cases in each data file. The function should return a
## data frame where the first column is the name of the file and the second
## column is the number of complete cases. A prototype of this function follows

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return a data frame of the form:
    ## id nobs
    ## 1    117
    ## 2    1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    # Print error message when receiving wrong param
    if(1 > min(id) || 332 < max(id)) {
        print(paste("Error: id is illegal (should be 1:332)"))
        return(NA)
    }

    # create a empty data frame with specific names
    result <- data.frame(id=integer(), nobs=integer())

    # go over each file in id to get the number of complete cases
    for(i in id) {
        # read each csv
        filename = paste(directory, "\\", formatC(i, width = 3, flag="0"), ".csv", sep="")
        data <- read.csv(filename) #read each csv

        # find the complete cases
        goodData <- complete.cases(data)

        # append the data into the result data frame
        result <- rbind(result, data.frame(id=c(i), nobs=c(sum(goodData))))
    }

    return(result)
}
