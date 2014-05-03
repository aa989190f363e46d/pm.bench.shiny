pollutantmean <- function(directory, pollutant, id = 1:332) {
    pollutants <- vector()
    for (i in id) {
        sensor <- read.csv(sprintf("%s/%03d.csv", directory, 
            i))
        pollutants <- append(pollutants, sensor[[pollutant]])
    }
    mean(pollutants, na.rm = TRUE)
}
