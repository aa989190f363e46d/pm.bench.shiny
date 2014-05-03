pollutantmean <- function(directory, pollutant, id = 1:332) {
    csvfile <- function(id) {
        paste(ifelse(nchar(id) == 1, "00", ifelse(nchar(id) == 
            2, "0", "")), id, ".csv", sep = "")
    }
    values <- numeric()
    for (i in id) {
        datafile <- read.csv(paste(directory, "/", csvfile(i), 
            sep = ""))
        values <- c(values, datafile[, pollutant])
    }
    mean(values, na.rm = TRUE)
}
