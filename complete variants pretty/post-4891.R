complete <- function(directory, id = 1:332) {
    storeDirectory <- paste0(getwd(), "/", directory, "/")
    if (dir.create(directory, showWarnings = FALSE) != 0) {
        dir.create(directory, showWarnings = FALSE)
    }
    if (length(list.files(directory)) == 0) {
        download.file("https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip", 
            destfile = paste0(storeDirectory, "specdata.zip"), 
            method = "internal")
        unzip(zipfile = paste0(storeDirectory, "specdata.zip"))
    }
    fileNames <- list.files(storeDirectory)
    zipLoc <- grep(pattern = ".zip", x = fileNames, fixed = TRUE)
    if (length(zipLoc) > 0) {
        fileNames <- fileNames[-zipLoc]
    }
    fileNameVector <- paste0(storeDirectory, fileNames)
    fileDataList <- lapply(1:length(fileNameVector), function(index) read.csv(fileNameVector[index]))
    numComp <- lapply(1:length(fileNameVector), function(index) {
        sum(complete.cases(fileDataList[[index]]))
    })
    numCompVec <- unlist(numComp)
    completeObsFrame <- data.frame(cbind(1:length(fileNameVector), 
        numCompVec))
    colnames(completeObsFrame) <- c("id", "nobs")
    return(completeObsFrame[id, ])
}
