pollutantmean <- function(directory, pollutant, id = 1:332) {
    files <- list.files(directory, full.names = T)
    vdata <- NULL
    for (i in id) {
        currentMonitor <- read.csv(files[i])
        vdata <- append(vdata, na.omit(currentMonitor[, pollutant]))
    }
    return(round(mean(vdata), 3))
}
