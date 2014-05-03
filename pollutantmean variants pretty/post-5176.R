pollutantmean <- function(directory, pollutant, id = 1:332) {
    m <- matrix(NA, 1, 1)
    colnames(m) <- pollutant
    for (i in id) {
        x <- read.csv(paste(getwd(), "/", directory, "/", sprintf("%03d", 
            i), ".csv", sep = ""))
        if (pollutant == "nitrate") {
            m <- c(m, x$nitrate)
        }
        else if (pollutant == "sulfate") {
            m <- c(m, x$sulfate)
        }
    }
    mean(m, na.rm = T)
}
