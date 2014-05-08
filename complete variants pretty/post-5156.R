complete <- function(directory, id = 1:332) {
    AF = list.files(directory, pattern = "*.csv")
    total <- NULL
    for (i in id) {
        readfile <- paste(directory, AF[i], sep = "/")
        x <- read.csv(readfile)
        no_nas <- na.omit(x)
        total_obs <- nrow(no_nas)
        single <- (data.frame(id = i, nobs = total_obs))
        total <- rbind(total, single)
    }
    total
}
