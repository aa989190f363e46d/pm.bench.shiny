pollutantmean <- function(directory, pollutant, id = 1:332) {
    data <- data.frame()
    for (i in seq_along(id)) {
        if (id[i] < 10) {
            fileName <- paste("00", id[i], ".csv", sep = "")
        }
        else if (id[i] < 100) {
            fileName <- paste("0", id[i], ".csv", sep = "")
        }
        else {
            fileName <- paste(id[i], ".csv", sep = "")
        }
        fileDir <- paste(directory, "/", fileName, sep = "")
        filedf <- read.csv(fileDir)
        data <- rbind(data, read.csv(fileDir))
    }
    mean(data[, pollutant], na.rm = TRUE)
}
