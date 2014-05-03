pollutantmean <- function(directory, pollutant, id = 1:332) {
    total <- vector()
    for (i in id) {
        filename <- paste(directory, "/", sprintf("%03d", i), 
            ".csv", sep = "")
        my_data <- read.csv(filename)
        x <- my_data[, pollutant]
        y <- x[!is.na(x)]
        total <- append(total, y)
    }
    return(mean(total))
}
