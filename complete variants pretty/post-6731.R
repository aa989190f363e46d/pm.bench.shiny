complete <- function(directory, id = 1:332) {
    wd <- getwd()
    setwd(directory)
    files <- dir()
    returnDf <- data.frame()
    for (i in id) {
        data <- data.frame(read.csv(files[i]))
        x <- complete.cases(data["sulfate"])
        y <- complete.cases(data["nitrate"])
        z <- x & y
        completeCases <- length(z[z == TRUE])
        returnDf <- rbind(returnDf, c(i, completeCases))
    }
    setwd(wd)
    colnames(returnDf) <- c("id", "nobs")
    returnDf
}
