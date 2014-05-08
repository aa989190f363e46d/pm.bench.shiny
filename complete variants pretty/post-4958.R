complete <- function(directory, id = 1:332) {
    resultdf <- data.frame(id = integer(0), nobs = numeric(0))
    for (curid in id) {
        fn <- paste(directory, "/", formatC(curid, width = 3, 
            flag = "0"), ".csv", sep = "")
        data <- read.csv(fn)
        resultdf = rbind(resultdf, data.frame(id = curid, nobs = sum(complete.cases(data))))
    }
    resultdf
}
