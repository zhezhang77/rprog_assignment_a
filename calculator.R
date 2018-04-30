## Part 5 [6 Marks]
## Write a function that prompts a user to choose an operation between six 
## available operations: 1) Add, 2) Subtract, 3) Multiply, 4) Divide, 5) Factors 
## and 6) Prime number. The first four operations will ask user to provide two 
## numbers and add, subtract, multiply and divide them accordingly. The fifth 
## operation calculates the factors of a number and sixth operation checks if a 
## number is prime. 
## Please save your code to a file named calculator.R. The output should look 
## something like this when the user runs the function:
## 
##     [1] "******Simple R Calculator - Select operation: ******"
##     [1] "1.Add"
##     [1] "2.Subtract"
##     [1] "3.Multiply"
##     [1] "4.Divide"
##     [1] "5.Factors"
##     [1] "6.Prime"
##
##     Enter choice [1/2/3/4/5/6]: 4 #prompting the user to select an operation
##     Enter first number: 20 #prompting the user to enter the first number
##     Enter second number: 4 #prompting the user to enter the second number
##     [1] "20 / 4 = 5"
##
##     [1] "******Simple R Calculator - Select operation: ******"
##     [1] "1.Add"
##     [1] "2.Subtract"
##     [1] "3.Multiply"
##     [1] "4.Divide"
##     [1] "5.Factors"
##     [1] "6.Prime"
##
##     Enter choice [1/2/3/4/5/6]: 5    #prompting the user to select an operation
##     Enter the number: 120            #prompting the user to enter the input
##     
##     [1] "The factors of 120 are:"
##     [1] 1
##     [1] 2
##     [1] 3
##     [1] 4
##     [1] 5
##     [1] 6
##     [1] 8
##     [1] 10
##     [1] 12
##     [1] 15
##     [1] 20
##     [1] 24
##     [1] 30
##     [1] 40
##     [1] 60
##     [1] 120
## 
## For this function you will need to use the 'readline' function in R to take 
## input from the user (terminal).
## Please read the help page for this function via '?readline' and make sure 
## that you know how to use it.

## Prime check function
prime <- function(x) {
  intX <- as.integer(x)
  
  if (intX < 0 || is.na(intX)) {
    ## Output warning message when receive error input
    print(paste(x, "need to be a positive integer!"))
    return(NA)
  }else if (intX == 1 || intX == 2) {
    ## Define 1 and 2 are prime
    result <- TRUE
  } else { 
    result <- TRUE
    
    ## For performance, only sqrt(x) numbers are needed for checking
    sqrtX = round(sqrt(intX))
    if (all(intX%%(2:sqrtX) != 0)){
      result <- TRUE
    } else {
      result <- FALSE
    }
  }
  
  ## Output the result
  if (result == TRUE) {
    print(paste(x, "is prime."))
  } else {
    print(paste(x, "is NOT prime."))
  }
  
  return(result)
}

## Factors calc function 
factors <- function(x) {
  intX <- as.integer(x)
  if (intX < 0 || is.na(intX)) {
    print(paste(x, "need to be a positive integer!"))
    return(NA)
  } else {
    ## For performance, only sqrt(x) numbers are needed for checking
    sqrtX = round(sqrt(intX))

    ## vectors to store factors
    list1 <- c()
    list2 <- c()

    ## Calculate factros from 1 to sqrtX and store the factor pair in to seperate vectors    
    for (i in 1:sqrtX) {
      if (intX%%i == 0){
        list1[length(list1)+1] <- i
        list2[length(list2)+1] <- intX/i
      }
    }
    
    ## Print the first half factors
    print(paste("The factors of", intX, "are:"))
    for(i in 1:length(list1)) {
      print(list1[[i]])
    }
    
    ## Print the second half factors
    ## Remove redundent factor if intX == sqrtX * sqrtX
    lenList2 = ifelse(list1[length(list1)] == list2[length(list2)], length(list2)-1, length(list2))
    if (lenList2 > 0) {
      for(i in lenList2:1) {
        print(list2[[i]])
      }
    }
  }
}

calculator <- function() {
  ## Step 1
  print("******Simple R Calculator - Select operation: ******")
  print("1.Add")
  print("2.Subtract")
  print("3.Multiply")
  print("4.Divide")
  print("5.Factors")
  print("6.Prime")
  operatorIdx <- as.numeric(readline("Enter choice [1/2/3/4/5/6]:"))
  
  operatorList <- c("+",
                    "-",
                    "*",
                    "/",
                    "factors",
                    "prime")
  
  
  if (0 < operatorIdx && operatorIdx < 5) { ## 2 params function
    
    firstNumber = as.numeric(readline("Enter first number:"))
    secondNumber = as.numeric(readline("Enter second number:"))
    
    if (is.na(firstNumber) || is.na(secondNumber)) {
      print("Param error")
    } else {
      ## generate expression text
      funcText = paste(firstNumber, operatorList[operatorIdx], secondNumber)
      print(paste(funcText, "=", eval(parse(text=funcText))))
    }
    
  } else if (4 < operatorIdx && operatorIdx < 7){ ## 1 param function
    
    singleNumber = readline("Enter the number:")
    
    funcText = paste(operatorList[operatorIdx], "(", singleNumber, ")", sep="")
    
    eval(parse(text=funcText))
  
  } else { ## Unknown choice
    print("Error: Unknown choice!")
    return(NA)
  }
}
