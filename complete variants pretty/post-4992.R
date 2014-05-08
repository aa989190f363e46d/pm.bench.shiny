complete <- function(directory, id = 1:332) {
    data1 <- list.files(directory)
    neededfiles <- paste("specdata/", data1[id], sep = "")
    result.mat <- mat.or.vec(length(id), 2)
    temp <- NULL
    for (i in 1:length(neededfiles)) {
        data2 <- read.csv(neededfiles[i])
        temp1 <- rbind.data.frame(temp, data2)
        m <- complete.cases(temp1)
        m1 <- sum(m)
        result.mat[i, 1] <- id[i]
        result.mat[i, 2] <- m1
    }
    mydataframe <- data.frame(result.mat)
    names(mydataframe) <- c("id", "nobs")
    return(mydataframe)
}
