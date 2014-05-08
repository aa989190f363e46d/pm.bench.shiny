complete <- function(directory, id = 1:332) {
    list <- dir(directory, full.names = TRUE)
    f.data <- function(n) {
        for (file in n) {
            if (!exists("p.data")) {
                p.data <- read.csv(file, header = TRUE)
            }
            else if (exists("p.data")) {
                temp.data <- read.csv(file, header = TRUE)
                p.data <- rbind(p.data, temp.data)
                rm(temp.data)
            }
        }
        p.data
    }
    pollutant.data <- f.data(list)
    good <- function(i) {
        subset.i <- subset(pollutant.data, ID == i)
        sum(complete.cases(subset.i))
    }
    nobs.data <- data.frame()
    for (i in id) {
        i.data <- data.frame(id = i, nobs = good(i))
        nobs.data <- rbind(nobs.data, i.data)
        rm(i.data)
    }
    nobs.data
}
