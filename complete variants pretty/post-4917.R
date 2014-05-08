complete <- function(directory, id = 1:332) {
    FileNames <- list.files(path = directory)
    Obs <- matrix(nrow = 0, ncol = 2)
    for (i in id) {
        MonData <- read.csv(file = paste(directory, "/", FileNames[i], 
            sep = ""), header = TRUE)
        MonData <- MonData[complete.cases(MonData), ]
        Obs <- rbind(Obs, c(i, nrow(MonData)))
    }
    NumberOfObs <- data.frame(id = (Obs[, 1]), nobs = (Obs[, 
        2]))
    NumberOfObs <- NumberOfObs[complete.cases(NumberOfObs), ]
    NumberOfObs
}
