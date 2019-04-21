##This function determines the state with the best outcome for a specific disease condition
best <- function(state, outcome){
  data<- read.csv("outcome-of-care-measures.csv")
  states <- levels(data[,7])[data[,7]]
  state_flag <- FALSE
  for (i in 1:length(states)){
    if (state == states [i]){
      state_flag <- TRUE
    }
  }
  if (!state_flag){
    stop("invalid state")
  }
  if (!((outcome == "heart attack")|(outcome == "heart failure")
        | (outcome == "pneumonia"))){
            stop ("invalid outcome")
  }
col <- if (outcome == "heart attack") {
  11
} else if (outcome == "heart failure") {
  17
}else {
  23
}
  data[,col] <- suppressWarnings(as.numeric(levels(data[,col])[data[,col]]))
  data[,2] <- as.character(data[,2])
  statedata <- data[grep(state, data$State),]
  orderdata <- statedata[order(statedata[,col], statedata[,2], na.last = NA),]
  orderdata[1,2]
}

##This functions gives tha name of a state with the specific rank in the identified disease condition
rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv")
  states<- levels(data[,7])[data[,7]]
  state_flag <- FALSE
  for (i in 1:length(states)){
    if (state == states[i]){
      state_flag <- TRUE
    }
  }
  if (!state_flag){
    stop("invalid state")
  }
  if(!((outcome == "heart attack")|(outcome == "heart failure")
       | (outcome == "pneumonia"))){
        stop ("invalid outcome")
  }
  col <- if (outcome == "heart attack"){
    11
  } else if (outcome == "heart failure") {
    17
  }else {
    23
  }
  data[,col]<- suppressWarnings(as.numeric(levels(data[,col])[data[,col]]))
  data[,2] <- as.character(data[,2])
  statedata <- data[grep(state, data$State),]
  orderdata <- statedata[order(statedata[,col], statedata[,2], na.last = NA),]
  if(num == "best") {
      orderdata[1,2]
  } else if (num == "worst"){
      orderdata[nrow(orderdata),2]
  } else {
      orderdata[num,2]
  }
}

##This function provides the names of the hospital that are the best performing in terms of the identified 
##disease condition and returms a data frame
##Part 4: rankall.R:
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  ## Check that outcome is valid
  if (!((outcome == "heart attack") | (outcome == "heart failure")
        | (outcome == "pneumonia"))) {
    stop ("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  col <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else {
    23
  }
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  # Generate an empty vector that will be filled later, row by row, to
  # generate the final output.
  output <- vector()
  states <- levels(data[, 7])
  for(i in 1:length(states)) {
    statedata <- data[grep(states[i], data$State), ]
    orderdata <- statedata[order(statedata[, col], statedata[, 2],
                                 na.last = NA), ]
    hospital <- if(num == "best") {
      orderdata[1, 2]
    } else if(num == "worst") {
      orderdata[nrow(orderdata), 2]
    } else{
      orderdata[num, 2]
    }
    output <- append(output, c(hospital, states[i]))
  }
  ## Return a data frame with the hospital names and the (abbreviated)
  ## state name
  output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
  colnames(output) <- c("hospital", "state")
  rownames(output) <- states
  output
}