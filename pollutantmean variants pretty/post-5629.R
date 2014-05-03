pollutantmean <- function(directory, pollutant, id = 1:332) {
    final <- NULL
    for (i in 1:length(id)) {
        temp <- paste(directory, "/", sep = "")
        temp1 <- sprintf("%03d", id[i])
        filename <- paste(temp, temp1, ".csv", sep = "")
        file <- read.csv(filename)
        mn <- file[, pollutant]
        final <- append(final, mn)
    }
    final1 <- mean(final, na.rm = T)
    return(final1)
}
