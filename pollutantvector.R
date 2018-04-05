## Part 4 [6 Marks]
## Write a function named 'pollutantvector' that returns a vector of those 
## pollutants (sulfate or nitrate) whose values are greater than 'p', across a 
## specified list of monitors. The function 'pollutantvector' takes four 
## arguments: 'directory', 'pollutant', 'id' and 'p'. Given a vector monitor ID 
## numbers, 'pollutantvector' reads that monitors' particulate matter data from 
## the directory specified in the 'directory' argument and returns the ones more 
## than a certain value ('p') across all of the monitors, ignoring any missing 
## values coded as NA.