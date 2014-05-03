pollutantmean <- function(directory, pollutant, id = 1:332) {
    filelist <- list.files(directory, full.names = TRUE)
    filestoread <- filelist[c(id)]
    dataset <- NULL
    for (i in 1:length(filestoread)) {
        myData <- read.csv(filestoread[i])
        dataset <- rbind.data.frame(dataset, myData)
    }
    colnames(dataset) <- c("Date", "sulfate", "nitrate", "ID")
    polmean <- mean(dataset[, pollutant], na.rm = TRUE)
    polmean
}
