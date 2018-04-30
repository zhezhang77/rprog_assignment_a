## Part 1 [6 Marks]
## Write a function named 'pollutantmean' that calculates the mean of a
## pollutant (sulfate or nitrate) across a specified list of monitors. The
## function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and
## 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
## particulate matter data from the directory specified in the 'directory'
## argument and returns the mean of the pollutant across all of the monitors,
## ignoring any missing values coded as NA. A prototype of the function is as
## follows:

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'polutant' is a character vector of length 1 indicating
    ## the name of the pollutant for ehich we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used.

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!

    # Print error message when receiving wrong param
    if(pollutant != 'sulfate' && pollutant != 'nitrate') {
        print(paste("Error:", pollutant, "is unknown. Please use either \'sulfate\' or \'nitrate\'"))
        return(NA)
    }

    if(1 > min(id) || 332 < max(id)) {
        print(paste("Error: id is illegal (should be 1:332)"))
        return(NA)
    }

    sumPollutant <- 0 # sum of pollutant
    numPollutant <- 0 # num of values

    for(i in id) {
        # read each csv
        filename = paste(directory, "\\", formatC(i, width = 3, flag="0"), ".csv", sep="")
        data <- read.csv(filename) #read each csv

        # extract valid value to a vector
        naList <- is.na(data[,`pollutant`])
        singleCol = data[!naList,`pollutant`]

        # add up the sum and number of each file
        sumPollutant <- sumPollutant + sum(singleCol)
        numPollutant <- numPollutant + length(singleCol)
    }

    # return the mean of all files included in 'id'
    print(paste("sum =", sumPollutant, "num =", numPollutant, "mean =", sumPollutant/numPollutant))
    return(sumPollutant/numPollutant)
}


