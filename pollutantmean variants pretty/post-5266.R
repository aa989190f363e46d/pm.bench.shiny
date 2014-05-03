pollutantmean <- function(directory, pollutant, id = 1:332) {
    currentPath <- getwd()
    folderPath <- paste(currentPath, "/", directory, sep = "")
    selectedData <- data.frame()
    files <- c(list.files(folderPath))
    for (i in files[id]) {
        fileContents <- read.csv(paste(folderPath, "/", i, sep = ""))
        selectedData <- rbind(selectedData, fileContents)
    }
    myMean <- mean(selectedData[, pollutant], na.rm = TRUE)
    return(myMean)
}
