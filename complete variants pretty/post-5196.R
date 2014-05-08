complete <- function(directory, id = 1:332) {
    nobs <- vector()
    for (i in id) {
        sensor <- read.csv(sprintf("%s/%03d.csv", directory, 
            i))
        nobs <- append(nobs, sum(!is.na(sensor$sulfate) & !is.na(sensor$nitrate)))
    }
    data.frame(id, nobs)
}
