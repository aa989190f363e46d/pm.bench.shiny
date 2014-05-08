complete <- function(directory, id = 1:332) {
    fileformat <- paste0(directory, "/%03d", ".csv")
    files <- c(sprintf(fileformat, id))
    pmcomplete <- do.call("rbind", lapply(files, function(xfile) {
        this.file <- read.csv(xfile, na.strings = c("NA"), stringsAsFactors = FALSE)
        cbind(id = this.file$ID[1], nobs = sum(complete.cases(this.file)))
    }))
    return(data.frame(pmcomplete))
}
