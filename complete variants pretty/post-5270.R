complete <- function(directory, id = 1:332) {
    numrows <- length(id)
    mtrx <- matrix(NA, nrow = numrows, ncol = 2)
    colnames(mtrx) <- c("id", "nobs")
    for (i in 1:numrows) {
        filenum <- id[i]
        filename <- if (filenum < 10) {
            paste("00", as.character(filenum), sep = "")
        }
        else if (filenum < 100) {
            paste("0", as.character(filenum), sep = "")
        }
        else {
            as.character(filenum)
        }
        filepath <- paste(c(directory, "/", filename, ".csv"), 
            collapse = "")
        dataframe <- read.csv(filepath)
        numcases <- sum(complete.cases(dataframe))
        mtrx[i, ] <- c(filenum, numcases)
    }
    as.data.frame(mtrx)
}
