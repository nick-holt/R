# Week 2 Programming Assignment - R Programming (Coursera)

setwd("C:/Users/nholt2/Desktop/R (Coursera)/2fdata/")
library(dplyr)

# 1. Plot the 30-day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

## 2. Finding the best hospital in a state:
## Write a function called best that takes two arguments: the 2-character abbreviated
## name of a state and an outcome name. The function reads the outcome-of-care-measures.csv
## file and returns a character vector with the name of the hospital that has the best (i.e. lowest)
## 20-day mortality for the specified outcome in that state. The hospital name is the name provided 
## in the Hospital.Name variable. The outcomes can be one lf "heart attack", "heart failure",
## or "pneumonia". Hospitals that do not have data on a particular outcome should be excluded
## from the set of hospitals when deciding rankings.

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        ranklist <- as.data.frame(matrix(ncol = 3, nrow = 1))
        output <- vector()
        states <- unique(data$State)
        statecheck <- state %in% states
                ## assign correct variable to ranklist based on outcome input
                if(statecheck == "TRUE" & outcome == "heart attack") {
                        ranklist <- cbind(data[,11], data$Hospital.Name, data$State)
                }
                else if(statecheck == "TRUE" & outcome == "heart failure") {
                        ranklist <- cbind(data[,17], data$Hospital.Name, data$State)
                }
                else if(statecheck == "TRUE" & outcome == "pneumonia") {
                        ranklist <- cbind(data[,23], data$Hospital.Name, data$State)
                }
                ## Handle errors
                else if(statecheck == "FALSE") {
                        return("ERROR in best(state, outcome) : invalid state")
                }
                else {
                        return("ERROR in best(state, outcome) : invalid outcome")
                }
        colnames(ranklist) <- c("Deaths", "Hospital.Name", "State")
        ranklist <- as.data.frame(ranklist)
        ranklist$Deaths <- suppressWarnings(as.numeric(levels(ranklist$Deaths))[ranklist$Deaths])
        ranklist <- subset(ranklist, ranklist[,3] == state)
        ranklist <- subset(ranklist, ranklist[,1] != "Not Available")
        ## Rank hospitals and use Hospital.Name as tie breaker
        order.scores<-order(ranklist$Deaths,ranklist$Hospital.Name)
        ranklist$ranks <- NA
        ranklist$ranks[order.scores] <- 1:nrow(ranklist)
        ranklist <- subset(ranklist, ranklist$ranks == 1)
        bestHospital <- as.vector(ranklist[,2])
        ## Return hospital name in that state with the lowest 30-day death rate
        return(bestHospital)
}

## 3. Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

## The num argument can take values "best", "worst", or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        targetrank <- 1
        ranklist <- as.data.frame(matrix(ncol = 3, nrow = 1))
        output <- vector()
        states <- unique(data$State)
        statecheck <- state %in% states
        ## assign correct variable to ranklist based on outcome input
        if(statecheck == "TRUE" & outcome == "heart attack") {
                ranklist <- cbind(data[,11], data$Hospital.Name, data$State)
        }
        else if(statecheck == "TRUE" & outcome == "heart failure") {
                ranklist <- cbind(data[,17], data$Hospital.Name, data$State)
        }
        else if(statecheck == "TRUE" & outcome == "pneumonia") {
                ranklist <- cbind(data[,23], data$Hospital.Name, data$State)
        }
        ## Handle errors
        else if(statecheck == "FALSE") {
                return("ERROR in best(state, outcome) : invalid state")
        }
        else {
                return("ERROR in best(state, outcome) : invalid outcome")
        }
        colnames(ranklist) <- c("Deaths", "Hospital.Name", "State")
        ranklist <- as.data.frame(ranklist)
        ranklist$Deaths <- suppressWarnings(as.numeric(levels(ranklist$Deaths))[ranklist$Deaths])
        ranklist <- subset(ranklist, ranklist[,3] == state)
        ranklist <- subset(ranklist, ranklist[,1] != "Not Available")
        ## Rank hospitals and use Hospital.Name as tie breaker
        order.scores<-order(ranklist$Deaths,ranklist$Hospital.Name)
        ranklist$ranks <- NA
        ranklist$ranks[order.scores] <- 1:nrow(ranklist)
        ## Take num argument and alter the targetrank variable accordingly
                if(num != "worst" & num != "best" & num <= max(ranklist$ranks)) {
                        targetrank <- num
                }
                else if(num == "worst") {
                        targetrank <- max(ranklist$ranks)
                }
                else if(num == "best") {
                        targetrank <- 1
                }
                else if(num > nrow(ranklist)) {
                        targetrank <- NA
                }
                else {
                        return("NUM ARG ERROR")
                }
        ## Set value of the correct hospital name
        if(is.na(targetrank)=="FALSE"){
                ranklist <- subset(ranklist, ranklist$ranks == targetrank)
                bestHospital <- as.vector(ranklist[,2])
        } 
        else {
                bestHospital <- targetrank
        }
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        return(bestHospital)
}

## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
## (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. The function should return a value
## for every state (some may be NA). The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        listHospital <- as.data.frame(matrix(ncol = 2, nrow = 0))
        colnames(listHospital) <- c("Hospital.Name", "State")
        targetrank <- 1
        ranklist <- as.data.frame(matrix(ncol = 3, nrow = 1))
        output <- vector()
        states <- unique(data$State)
        ## assign correct variable to ranklist based on outcome input
        if(outcome == "heart attack") {
                ranklist <- cbind(data[,11], data$Hospital.Name, data$State)
        }
        else if(outcome == "heart failure") {
                ranklist <- cbind(data[,17], data$Hospital.Name, data$State)
        }
        else if(outcome == "pneumonia") {
                ranklist <- cbind(data[,23], data$Hospital.Name, data$State)
        }
        ## Handle errors
        else {
                return("ERROR in best(outcome, num) : invalid outcome")
        }
        for(i in states) {
                statelist <- ranklist
                colnames(statelist) <- c("Deaths", "Hospital.Name", "State")
                statelist <- as.data.frame(statelist)
                statelist$Deaths <- suppressWarnings(as.numeric(levels(statelist$Deaths))[statelist$Deaths])
                statelist <- subset(statelist, statelist[,3] == i)
                statelist <- subset(statelist, statelist[,1] != "Not Available")
                ## Rank hospitals and use Hospital.Name as tie breaker
                order.scores<-order(statelist$Deaths,statelist$Hospital.Name)
                statelist$ranks <- NA
                statelist$ranks[order.scores] <- 1:nrow(statelist)
                ## Take num argument and alter the targetrank variable accordingly
                if(num != "worst" & num != "best" & num <= max(statelist$ranks)) {
                        targetrank <- num
                }
                else if(num == "worst") {
                        targetrank <- max(statelist$ranks)
                }
                else if(num == "best") {
                        targetrank <- 1
                }
                else {
                        targetrank <- NA
                }
                ## Create data frame for correct row of data so that row can be added to master list
                bestHospital <- as.data.frame(matrix(ncol = 2, nrow = 0))
                colnames(bestHospital) <- c("Hospital.Name", "State")
                ## Set value of the correct hospital name
                if(is.na(targetrank)=="FALSE"){
                        statelist <- subset(statelist, statelist$ranks == targetrank)
                        bestHospital <- statelist[,2:3]
                } 
                else {
                        bestHospital[1,1] <- NA
                        bestHospital[1,2] <- i
                }
                listHospital <- rbind(listHospital, bestHospital)
        }
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        colnames(listHospital) <- c("Hospital", "State")
        listHospital <- arrange(listHospital, by = State)
        return(listHospital)
}



