pollutantmean <- function(directory, pollutant, id = 1:332) {
    observations = c()
    for (monitor in id) {
        dataset <- read.csv(paste("./", directory, "/", sprintf("%03d", 
            monitor), ".csv", sep = ""), TRUE)
        observations <- append(observations, dataset[, pollutant])
    }
    mean(observations, na.rm = TRUE)
}
