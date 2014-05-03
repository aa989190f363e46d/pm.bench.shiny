pollutantmean <- function(directory, pollutant, id = 1:332) {
    idaux <- id + 1000
    idaux2 <- substr(paste(as.character(idaux), ".csv", sep = ""), 
        2, 8)
    dataF <- read.csv(paste(getwd(), "/", directory[1], "/", 
        idaux2[1], sep = ""))
    if (length(id) > 1) {
        for (i in 2:length(id)) {
            df.aux <- read.csv(paste(getwd(), "/", directory[1], 
                "/", idaux2[i], sep = ""))
            dataF <- rbind(dataF, df.aux)
        }
    }
    m <- mean(dataF[[pollutant]], na.rm = TRUE)
    print(m)
}
