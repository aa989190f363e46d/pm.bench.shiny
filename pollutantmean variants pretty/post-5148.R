pollutantmean <- function(directory, pollutant, id = 1:332) {
    raw_data <- vector(mode = "numeric", length = 0)
    for (i in 1:length(id)) {
        csv_data <- read.csv(paste(directory, "/", formatC(as.integer(id[i]), 
            2, flag = 0), ".csv", sep = ""))
        raw_data <- append(raw_data, csv_data[[pollutant]])
    }
    poll_mean <- mean(raw_data, na.rm = TRUE)
    return(poll_mean)
}
