pollutantmean <- function(directory, pollutant, id = 1:332) {
    filePaths <- file.path(directory, paste(sprintf("%03d", id), 
        ".csv", sep = ""))
    obs <- numeric()
    for (filePath in filePaths) {
        tmpData <- read.csv(filePath)
        tmpData <- subset(tmpData[[pollutant]], !is.na(tmpData[[pollutant]]))
        obs <- c(obs, as.vector(tmpData))
    }
    mean(obs)
}
