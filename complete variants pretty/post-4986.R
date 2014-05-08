complete <- function(directory, id = 1:332) {
    counts <- cbind(id, sapply(X = paste("specdata", sprintf("%03i.csv", 
        id), sep = "/"), FUN = function(x) nrow(na.omit(read.table(x, 
        header = T, sep = ",", na.strings = "NA", stringsAsFactors = FALSE)))))
    colnames(counts) <- c("id", "nobs")
    as.data.frame(counts)
}
