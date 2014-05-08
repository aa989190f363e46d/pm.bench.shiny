complete <- function(directory, id = 1:332) {
    nobs <- vector()
    for (i in id) {
        filename <- paste(directory, "/", sprintf("%03d", i), 
            ".csv", sep = "")
        my_data <- read.csv(filename)
        x <- sum(complete.cases(my_data))
        nobs <- append(nobs, x)
    }
    final <- data.frame(id, nobs)
    return(final)
}
