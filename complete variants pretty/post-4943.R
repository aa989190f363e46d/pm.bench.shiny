complete <- function(directory, id = 1:332) {
    filenames <- paste("C:/Users/JSTEPHEN/Documents/Coursera/02 R Programming/rprog-data-specdata/", 
        directory, "/", formatC(id, width = 3, flag = "0"), ".csv", 
        sep = "")
    full_data <- do.call("rbind", lapply(filenames, FUN = function(files) {
        read.table(files, header = TRUE, sep = ",")
    }))
    mydata <- full_data[complete.cases(full_data), ]
    idx <- split(1:nrow(mydata), mydata$ID)
    DF <- data.frame(ID = sapply(idx, function(i) mydata$ID[i[1]]), 
        nobs = sapply(idx, function(i) NROW(mydata[, "ID"][i])))
    DF
}
