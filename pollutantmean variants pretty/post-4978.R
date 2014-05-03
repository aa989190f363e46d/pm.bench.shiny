pollutantmean <- function(directory, pollutant, id = 1:332) {
    df <- data.frame()
    for (i in id) {
        file <- paste(".//", directory, "/", sprintf("%03d", 
            as.numeric(i)), ".csv", sep = "")
        temp <- read.csv(file, header = TRUE)
        df <- append(df, temp[pollutant])
    }
    df <- unlist(df)
    mean(df, na.rm = TRUE)
}
