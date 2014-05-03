pollutantmean <- function(directory, pollutant, id = 1:332) {
    for (i in id) {
        csvfile <- sprintf("%03d.csv", i)
        filepath <- file.path(directory, csvfile)
        if (!exists("masterfoo")) {
            masterfoo <- read.csv(filepath, header = TRUE, sep = ",")
        }
        else {
            foo <- read.csv(filepath, header = TRUE, sep = ",")
            masterfoo <- rbind(masterfoo, foo)
            rm(foo)
        }
    }
    pollutantfoo <- masterfoo[, pollutant]
    cleanedpollutantfoo <- pollutantfoo[!(is.na(pollutantfoo))]
    meanvalue <- round(mean(cleanedpollutantfoo), digits = 3)
    meanvalue
}
