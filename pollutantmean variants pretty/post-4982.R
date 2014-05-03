pollutantmean <- function(directory, pollutant, id = 1:332) {
    for (file_id in id) {
        filename <- paste(directory, "/", formatC(file_id, width = 3, 
            flag = "0"), ".csv", sep = "")
        if (!exists("outData")) {
            outData <- read.csv(filename)
        }
        else {
            tempData <- read.csv(filename)
            outData <- rbind(outData, tempData)
        }
    }
    mean(outData[[pollutant]], na.rm = TRUE)
}
