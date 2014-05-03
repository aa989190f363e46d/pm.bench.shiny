pollutantmean <- function(directory, pollutant, id = 1:332) {
    library(plyr)
    id <- formatC(id, width = 3, flag = "0")
    filenames <- paste(id, ".csv", sep = "")
    fullnames <- file.path(directory, filenames)
    ldf <- ldply(fullnames, read.csv)
    m <- mean(ldf[[pollutant]], na.rm = T)
    m
}
