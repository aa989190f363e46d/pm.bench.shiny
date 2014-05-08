complete <- function(directory, id = 1:332) {
    vectorID <- numeric()
    vectorNobs <- numeric()
    for (i in id) {
        filePath <- file.path(directory, paste(sprintf("%03d", 
            i), ".csv", sep = ""))
        tmpData <- read.csv(filePath)
        vectorID <- append(vectorID, i)
        vectorNobs <- append(vectorNobs, sum(complete.cases(tmpData)))
    }
    data.frame(id = vectorID, nobs = vectorNobs)
}
