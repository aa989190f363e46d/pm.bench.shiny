pollutantmean <- function(directory, pollutant, id = 1:332) {
    path <- paste(getwd(), directory, sep = "/")
    files <- list.files(path, pattern = ".csv", full.names = TRUE)
    index <- files[c(id)]
    df <- NULL
    for (i in 1:length(index)) {
        data <- read.csv(index[i])
        df <- rbind.data.frame(df, data)
    }
    if (pollutant == "nitrate") {
        print(mean(df$nitrate, na.rm = TRUE))
    }
    if (pollutant == "sulfate") {
        mean(df$sulfate, na.rm = TRUE)
    }
}
