pollutantmean <- function(directory, pollutant, id = 1:332) {
    list <- lapply(paste(directory, "/", sprintf("%03d", id), 
        ".csv", sep = ""), read.csv)
    df <- data.frame(Reduce(rbind, list))
    mean(df[, pollutant], na.rm = TRUE)
}
