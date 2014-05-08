complete <- function(directory, id = 1:332) {
    currentPath <- getwd()
    folderPath <- paste(currentPath, "/", directory, sep = "")
    selectedData <- data.frame()
    files <- c(list.files(folderPath))
    for (i in files[id]) {
        fileContents <- read.csv(paste(folderPath, "/", i, sep = ""))
        thisRow <- cbind(as.numeric(strsplit(i, ".csv")), nrow(fileContents[complete.cases(fileContents), 
            ]))
        selectedData <- rbind(selectedData, thisRow)
    }
    names(selectedData) <- c("id", "nobs")
    return(selectedData)
}
