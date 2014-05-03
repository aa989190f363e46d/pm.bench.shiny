pollutantmean <- function(directory, pollutant, id = 1:332) {
    setwd(paste(getwd(), directory, sep = "/"))
    temp <- paste(formatC(id, width = 3, flag = "0"), ".csv", 
        sep = "")
    sourceframe <- do.call("rbind", lapply(temp, read.csv, header = TRUE))
    ifelse(pollutant == "sulfate", round(mean(sourceframe[, 2], 
        na.rm = TRUE), 3), round(mean(sourceframe[, 3], na.rm = TRUE), 
        3))
}
