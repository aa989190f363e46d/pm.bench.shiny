pollutantmean <- function(directory, pollutant, id = 1:332) {
    nums <- c()
    for (i in id) {
        filename <- if (i < 10) {
            paste("00", as.character(i), sep = "")
        }
        else if (i < 100) {
            paste("0", as.character(i), sep = "")
        }
        else {
            as.character(i)
        }
        filepath <- paste(c(directory, "/", filename, ".csv"), 
            collapse = "")
        dataframe <- read.csv(filepath)
        newnums <- na.omit(dataframe[, pollutant])
        nums <- c(nums, newnums)
    }
    mn <- mean(nums)
    round(mn, digits = 3)
}
