pollutantmean <- function(directory, pollutant, id=1:332) {
  fileNames <- dir(directory, pattern =".csv")
  count <- 0
  total <- 0
  for(i in 1:length(id)){
    fileName <-  paste(str_pad(id[i], 3, pad = "0"), ".csv", sep="")
    df <- read.csv(file.path(directory,fileName))
    colVect <- df[[pollutant]]
    total <- total + sum(colVect, na.rm=TRUE)
    count <- count + length(colVect[!is.na(df[[pollutant]])])
  }
  total / count
  
}