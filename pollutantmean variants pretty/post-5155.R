pollutantmean <- function(directory, pollutant, id = 1:332) {
    AF <- list.files(directory, pattern = "*.csv")
    total <- NULL
    for (i in id) {
        readfile <- paste(directory, AF[i], sep = "/")
        x <- read.csv(readfile)
        total <- rbind.data.frame(total, x)
    }
    if (pollutant == "sulfate") {
        means <- mean(total[, "sulfate"], na.rm = TRUE)
    }
    else {
        means <- mean(total[, "nitrate"], na.rm = TRUE)
    }
    means_roundoff <- round(means, digits = 3)
    print(means_roundoff)
}
