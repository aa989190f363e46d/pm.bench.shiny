pollutantmean <- function(directory, pollutant, id = 1:332) {
    file.summs <- apply(sapply(X = paste("specdata", sprintf("%03i.csv", 
        id), sep = "/"), FUN = function(x) {
        tbl <- na.omit(read.table(x, header = T, sep = ",", na.strings = "NA", 
            stringsAsFactors = FALSE)[[pollutant]])
        c(length(tbl), sum(tbl))
    }), 1, sum)
    file.summs[2]/file.summs[1]
}
