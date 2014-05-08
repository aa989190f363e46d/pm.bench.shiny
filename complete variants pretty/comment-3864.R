complete2 <- function(directory, id = 1:332) {
    files <- list.files(directory, full.names = TRUE)
    df = NULL
    for (i in id) {
        temp <- read.csv(files[i], header = TRUE)
        comp <- sum(complete.cases(temp))
        df <- rbind(df, data.frame(id = i, nobs = comp))
    }
    df
}
