complete <- function(directory, id) {
    setwd(paste("my local directory which is the parent dir of specdata", 
        directory, sep = ""))
    files <- dir()
    data <- data.frame()
    for (i in id) {
        file.i <- read.csv(files[i])
        obs <- sum(complete.cases(file.i))
        data <- rbind(data, cbind(i, obs))
    }
    data
}
