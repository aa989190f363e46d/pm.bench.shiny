complete <- function(directory, id = 1:332) {
    idaux <- id + 1000
    idaux2 <- substr(paste(as.character(idaux), ".csv", sep = ""), 
        2, 8)
    nobs <- vector()
    for (i in 1:length(id)) {
        df.aux <- read.csv(paste(getwd(), "/", directory[1], 
            "/", idaux2[i], sep = ""))
        good <- complete.cases(df.aux)
        nobs[i] <- nrow(df.aux[good, ][, ])
    }
    df.salida <- data.frame(id, nobs)
    print(df.salida)
}
