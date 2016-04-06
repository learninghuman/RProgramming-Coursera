best <- function(state, outcome) {
  outcome_df <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  unique_states <- unique(outcome_df$State)
  if(is.na(match(state, unique_states))) {
    stop("invalid state")
  }
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(is.na(match(outcome, valid_outcomes))) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  col_index <- NULL
  new_df <- NULL
  if(outcome == "heart attack") {
    col_index <- 13
    new_df <- subset(outcome_df, outcome_df$State==state & outcome_df[[col_index]]!="Not Available", select=c(Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    new_df$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(new_df$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ))
  } else if(outcome == "heart failure") {
    col_index <- 19
    new_df <- subset(outcome_df, outcome_df$State==state & outcome_df[[col_index]]!="Not Available", select=c(Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    new_df$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(new_df$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ))
  } else if(outcome == "pneumonia") {
    col_index <- 25
    new_df <- subset(outcome_df, outcome_df$State==state & outcome_df[[col_index]]!="Not Available", select=c(Hospital.Name, Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    new_df$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(new_df$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ))
  }
  new_df <- new_df[with(new_df, order(new_df[,2],  Hospital.Name)), ]
  new_df[1,][1]
}