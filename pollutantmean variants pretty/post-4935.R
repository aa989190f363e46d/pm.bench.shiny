pollutantmean <- function(directory, pollutant, id = 1:332) {
    filenames <- paste(getwd(), "/", directory, "/", formatC(id, 
        width = 3, flag = "0"), ".csv", sep = "")
    full_data <- do.call("rbind", lapply(filenames, FUN = function(files) {
        read.table(files, header = TRUE, sep = ",")
    }))
    bad <- is.na(full_data[, pollutant])
    sum_data <- full_data[, pollutant][!bad]
    mean(sum_data)
}
