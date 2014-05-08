complete <- function(directory, id = 1:332) {
    df <- data.frame(id = id, nobs = rep(-1, length(id)))
    c <- 1
    for (i in id) {
        d <- read.csv(paste(directory, "\\", formatC(i, width = 3, 
            format = "d", flag = "0"), ".csv", sep = ""))
        df$nobs[c] <- sum(complete.cases(d))
        c <- c + 1
    }
    return(df)
}
