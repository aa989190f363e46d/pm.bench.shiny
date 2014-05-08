complete <- function(directory, id = 1:332) {
    comcollect <- data.frame()
    comcount <- 0
    collate <- data.frame()
    if (directory == "specdata") 
        setwd("E:/Coursera/Working Directory/specdata")
    filename <- list.files()
    count <- as.numeric(id)
    for (i in count) {
        file <- read.csv(filename[i])
        good <- complete.cases(file)
        comcollect <- file[good, ]
        comcount <- nrow(comcollect)
        df <- data.frame(id = i, nobs = comcount)
        collate <- rbind(collate, df)
    }
    print(collate)
}
