corr <- function(directory, threshold = 0) {
  fileNames <- dir(directory, pattern =".csv")
  result <- c()
  for(i in 1:length(fileNames)){
    df <- read.csv(file.path(directory,fileNames[i]))
    new_df <- subset(df, !is.na(df$sulfate) & !is.na(df$nitrate))
    if(nrow(new_df) >= threshold) {
      c <- cor(data.frame(new_df$sulfate, new_df$nitrate))
      result <- c(result, round(c[2],5))
    }
  }
  result
}