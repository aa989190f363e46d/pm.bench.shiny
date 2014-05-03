pollutantmean <- function(directory, pollutant, id = 1:332) {
    polreadings <- vector("numeric")
    filelist <- dir(directory)
    for (n in id) {
        fileloc <- paste(directory, "/", filelist[n], sep = "")
        aqreadings <- read.csv(fileloc)
        polreadings <- c(polreadings, aqreadings[[pollutant]])
    }
    mean(polreadings, na.rm = TRUE)
}
