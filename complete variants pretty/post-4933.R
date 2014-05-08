complete <- function(directory, id = 1:332) {
    df <- data.frame(id = integer(), nobs = integer(), stringsAsFactors = FALSE)
    for (monitor in id) {
        dataset <- read.csv(paste("./", directory, "/", sprintf("%03d", 
            monitor), ".csv", sep = ""), TRUE)
        df <- rbind(df, data.frame(id = monitor, nobs = sum(complete.cases(dataset))))
    }
    df
}
