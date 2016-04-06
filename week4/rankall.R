rankall <- function(outcome, num = "best") { 
  ## Read outcome data
  outcome_df <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(is.na(match(outcome, valid_outcomes))) {
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  col_index <- NULL
  new_df <- NULL
  if(outcome == "heart attack") {
    col_index <- 13
    new_df <- subset(outcome_df, outcome_df[[col_index]]!="Not Available", select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    new_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(new_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ))
  } else if(outcome == "heart failure") {
    col_index <- 19
    new_df <- subset(outcome_df, outcome_df[[col_index]]!="Not Available", select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    new_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(new_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ))
  } else if(outcome == "pneumonia") {
    col_index <- 25
    new_df <- subset(outcome_df, outcome_df[[col_index]]!="Not Available", select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    new_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(new_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ))
  }
  states <- levels(outcome_df[, 7])
  result <- data.frame()
  state_df <- NULL
  for(i in 1:length(states)) {
    if(outcome == "heart attack") {
      state_df <- subset(new_df, new_df$State==states[i], select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    } else if(outcome == "heart failure") {
      state_df <- subset(new_df, new_df$State==states[i], select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    } else if(outcome == "pneumonia") {
      state_df <- subset(new_df, new_df$State==states[i], select=c(Hospital.Name, State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    }
    state_df <- state_df[with(state_df, order(state_df[,3], state_df[,1])), ]
    if(num == "best") {
      state_df <- state_df[1,]
    } else if(num == "worst") {
      row <- nrow(state_df)
      state_df <- state_df[row,]
    } else {
      state_df <- state_df[num,]
      if(num > nrow(state_df)) {
        state_df$State <- states[i]
      }
    }
    result <- rbind(result, state_df)
  }
 
  result[, 1:2]
}
