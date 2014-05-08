complete <- function(directory, id = 1:332) {
    allData <- data.frame()
    for (i in id) {
        myData <- read.csv(paste(directory, sprintf("%03d.csv", 
            i), sep = "/"), header = TRUE)
        completeCases <- myData[complete.cases(myData), ]
        nobs <- nrow(completeCases)
        allData <- rbind(allData, data.frame(id = i, nobs = nobs))
    }
    return(allData)
}
