complete <- function(directory, id = 1:332) {
    files <- list.files(directory)
    df <- NULL
    for (i in id) {
        myFiles <- paste(directory, files[i], sep = "/")
        for (file in myFiles) {
            temp <- read.csv(paste(getwd(), "/", file, sep = ""), 
                header = TRUE)
            comp <- sum(complete.cases(temp))
            df <- rbind(df, data.frame(id = i, nobs = comp))
        }
    }
}
