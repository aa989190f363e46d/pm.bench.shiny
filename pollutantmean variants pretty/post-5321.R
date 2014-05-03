pollutantmean <- function(directory, pollutant, id = 1:332) {
    filenames <- paste0(directory, "/", formatC(id, width = 3, 
        flag = "0"), ".csv")
    a <- numeric()
    for (i in 1:length(filenames)) {
        x <- read.csv(filenames[[i]])
        a <- c(a, x[[pollutant]])
    }
    round(mean(a, na.rm = TRUE), digits = 3)
}
