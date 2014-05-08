complete <- function(directory, id = 1:332) {
    mydir <- paste(getwd(), directory, sep = "/")
    setwd(mydir)
    filelist <- list.files()
    filenames <- filelist[id]
    nobs <- numeric()
    for (i in filenames) {
        singlefile <- read.csv(i)
        p1 <- singlefile[, 2]
        p2 <- singlefile[, 3]
        ok <- complete.cases(p1, p2)
        nobs <- c(nobs, sum(ok))
    }
    data.frame(id, nobs)
}
