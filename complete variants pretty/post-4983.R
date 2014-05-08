complete <- function(directory, id = 1:332) {
    outData <- data.frame(id = integer(0), nobs = integer(0))
    for (file_id in id) {
        filename <- paste(directory, "/", formatC(file_id, width = 3, 
            flag = "0"), ".CSV", sep = "")
        tempData <- read.csv(filename)
        tempData2 <- na.omit(tempData)
        obsCount <- nrow(tempData2)
        outDataRow <- data.frame(id = file_id, nobs = obsCount)
        outData <- rbind(outData, outDataRow)
    }
    outData
}
