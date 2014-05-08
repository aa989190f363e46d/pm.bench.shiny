complete <- function(directory, id = 1:332) {
    nobs <- numeric(0)
    for (i in id) {
        filename <- file.path(directory, paste(formatC(i, width = 3, 
            flag = "0"), ".csv", sep = ""))
        data <- read.csv(filename)
        nobs <- c(nobs, nrow(na.omit(data)))
    }
    data.frame(id, nobs)
}
