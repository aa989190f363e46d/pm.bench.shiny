corr <- function(directory, threshold = 0) {
    id <- list.files(directory)
    cv <- c()
    for (i in id) {
        data <- read.csv(paste(directory, "/", i, sep = ""))
        comp <- complete.cases(data)
        sub <- subset(data, comp)
        if (nrow(sub) > threshold) {
            cv <- c(cv, cor(sub$sulfate, sub$nitrate))
        }
    }
    cv
}
