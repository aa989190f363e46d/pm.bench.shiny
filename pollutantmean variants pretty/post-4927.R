pollutantmean <- function(directory, pollutant, id = 1:332) {
    FileNames <- list.files(path = directory)
    PolData <- data.frame(Date = as.Date(character()), sulfate = numeric(), 
        nitrate = numeric(), ID = integer())
    for (i in id) {
        PolData <- rbind(PolData, read.csv(file = paste(directory, 
            "/", FileNames[i], sep = ""), header = TRUE))
    }
    mean(x = PolData[, pollutant], na.rm = TRUE)
}
