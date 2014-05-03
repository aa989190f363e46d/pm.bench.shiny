pollutantmean <- function(directory, pollutant, ID = 1:332) {
    directory <- "specdata"
    specdata_files <- paste("specdata/", list.files(directory), 
        sep = "")
    specdata_all <- do.call(rbind, lapply(specdata_files, read.csv))
    if (pollutant == "sulfate") {
        specdata_sub <- subset(specdata_all, select = c(ID, sulfate))
    }
    else {
        specdata_sub <- subset(specdata_all, select = c(ID, nitrate))
    }
    good <- complete.cases(specdata_sub)
    specdata_good <- specdata_sub[good, ]
    specdata_sum <- c()
    specdata_len <- c()
    temp_sum <- numeric()
    temp_len <- numeric()
    for (i in ID) {
        temp <- subset(specdata_good, ID == i)
        temp_sum[i] <- sum(temp[, 2])
        temp_len[i] <- nrow(temp)
        specdata_sum[i] <- temp_sum[i]
        specdata_len[i] <- temp_len[i]
    }
    bad_sum <- is.na(specdata_sum)
    bad_len <- is.na(specdata_len)
    sum(specdata_sum[!bad_sum])/sum(specdata_len[!bad_len])
}
