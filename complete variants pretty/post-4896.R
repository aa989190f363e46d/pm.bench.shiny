complete <- function(directory, id = 1:332) {
    x <- data.frame()
    flist <- list.files(path = directory, full.names = TRUE)
    for (fname in flist[id]) {
        df <- read.csv(fname)
        dfgood <- !logical(length = nrow(df))
        for (j in 1:ncol(df)) {
            dfgood <- dfgood & !is.na(df[, j])
        }
        x <- rbind(x, c(match(fname, flist), sum(dfgood)))
    }
    names(x) <- c("id", "nobs")
    x
}
