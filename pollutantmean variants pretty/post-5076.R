pollutantmean <- function(directory, pollutant, id) {
    datafile <- paste0(getwd(), "/", directory, "/")
    datasource <- list.files(datafile)
    dataloc <- paste0(datafile, datasource)
    full <- read.csv(paste0(datafile, "999.csv"))
    for (i in dataloc[id]) {
        data <- read.csv(i)
        full <- rbind(full, data)
    }
    write.csv(full, file = "temp.csv")
    print(mean(full[[pollutant]], na.rm = T))
    return(mean(full[[pollutant]], na.rm = T))
}
