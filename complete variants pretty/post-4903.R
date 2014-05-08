complete <- function(directory, id = 1:332) {
    path <- paste(getwd(), directory, sep = "/")
    files <- list.files(path, pattern = ".csv", full.names = TRUE)
    index <- files[c(id)]
    nobs <- NULL
    for (i in 1:length(index)) {
        mydata <- read.csv(index[i])
        x <- sum(complete.cases(mydata))
        nobs <- c(nobs, x)
    }
    df <- data.frame(id, nobs)
    colnames(df) <- c("id", "nobs")
    df
}
