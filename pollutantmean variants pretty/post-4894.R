pollutantmean <- function(directory, pollutant, id = 1:332) {
    x <- data.frame()
    for (fname in list.files(path = directory, full.names = TRUE)[id]) {
        x <- rbind(x, read.csv(fname))
    }
    mean(x[, pollutant], na.rm = TRUE)
}
