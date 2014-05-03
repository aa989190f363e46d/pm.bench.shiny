pollutantmean <- function(directory, pollutant, id = 1:332) {
    dir <- paste0(getwd(), "/", directory)
    files <- list.files(dir)
    data <- numeric()
    for (i in id) {
        currentMonitor <- read.csv(paste0(dir, "/", files[as.numeric(i)]))
        if (pollutant == "nitrate") 
            K <- 3
        else K <- 2
        data <- append(data, na.omit(currentMonitor[, K]))
    }
    return(mean(data, na.rm = TRUE))
}
