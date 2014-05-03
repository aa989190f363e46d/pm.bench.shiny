pollutantmean <- function(directory, pollutant, id = 1:332) {
    dir <- c(paste(getwd(), directory, sep = "/"))
    setwd(dir)
    x <- list.files(pattern = ".csv")
    y <- lapply(x[id], read.table, header = TRUE, sep = ",")
    z <- do.call(rbind, y)
    meanpollut <- mean(z[, paste(pollutant)], na.rm = TRUE)
    return(meanpollut)
}
