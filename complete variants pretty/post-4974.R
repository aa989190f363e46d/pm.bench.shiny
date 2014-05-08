complete <- function(directory, id = 1:332) {
    df = data.frame()
    names <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    for (i in id) {
        temp <- sum(complete.cases(read.csv(names[i], header = TRUE)))
        df <- rbind(df, data.frame(id = i, nobs = temp))
    }
    df
}
