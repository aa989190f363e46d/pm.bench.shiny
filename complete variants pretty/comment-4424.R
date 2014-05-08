complete <- function(directory, id = 1:332) {
    files <- list.files(pattern = "*.csv")
    nobs <- integer()
    for (i in id) {
        fi <- read.csv(files[i])
        nobs <- c(nobs, length(which(complete.cases(fi))))
    }
    return(data.frame(id, nobs))
}
