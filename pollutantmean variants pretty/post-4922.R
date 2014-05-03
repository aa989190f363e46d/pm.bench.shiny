pollutantmean <- function(directory, pollutant, id = 1:332) {
    mydir <- paste("./", directory, "/", sep = "")
    makeFileNames <- function(id) {
        idvec2char <- substring(paste("00", id, sep = ""), -3)
        filename <- paste(mydir, substr(idvec2char, nchar(idvec2char) - 
            2, nchar(idvec2char)), ".csv", sep = "")
        return(filename)
    }
    filenames <- makeFileNames(id)
    mydataframe <- do.call("rbind", lapply(filenames, read.csv, 
        header = TRUE))
    mycol <- which(colnames(mydataframe) == pollutant)
    myvar <- mydataframe[, mycol]
    myNonMissingData <- myvar[!is.na(myvar)]
    myMean <- mean(myNonMissingData)
    return(myMean)
}
