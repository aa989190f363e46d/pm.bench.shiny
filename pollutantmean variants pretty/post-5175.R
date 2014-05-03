pollutantmean <- function(directory, pollutant, id = 1:332) {
    mydir <- paste(getwd(), directory, sep = "/")
    setwd(mydir)
    filelist <- list.files()
    filenames <- filelist[id]
    mybig <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
    if (pollutant == "sulfate") {
        myMeans <- mean(mybig[, 2], na.rm = TRUE)
    }
    else if (pollutant == "nitrate") {
        myMeans <- mean(mybig[, 3], na.rm = TRUE)
    }
    else {
        print("error")
    }
    myMeans
}
