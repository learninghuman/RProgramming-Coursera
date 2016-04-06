complete <- function(directory, id=1:332) {
  for(i in 1:length(id)){
    fileName <-  paste(str_pad(id[i], 3, pad = "0"), ".csv", sep="")
    df <- read.csv(file.path(directory,fileName))
    new_df <- subset(df, !is.na(df$sulfate) & !is.na(df$nitrate))
    values <- c(values, nrow(new_df))
  }
  data.frame(id=id,nobs=values)
}