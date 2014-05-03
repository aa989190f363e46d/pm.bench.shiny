pollutantmean <- function(directory, pollutant, id = 1:332) {
    library("plyr")
    files <- paste(directory, "/", formatC(id, width = 3, flag = "0"), 
        ".csv", sep = "")
    data <- ldply(files, function(fn) read.csv(fn))
    mean(data[, pollutant], na.rm = TRUE)
}
