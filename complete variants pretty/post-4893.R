complete <- function(directory, id = 1:332) {
    dw <- c("C:/Users/Diego/Desktop/Coursera_Data_Science/Programming Assessment/Week2/")
    setwd(paste(dw, directory, sep = ""))
    file <- list.files(pattern = ".csv")
    table <- data.frame()
    h <- id
    for (i in 1:length(h)) {
        x <- read.csv(file[h[i]])
        y <- c(h[i], length(which(!is.na(x[, "nitrate"] * x[, 
            "sulfate"]))))
        table <- as.data.frame(rbind(table, y))
    }
    colnames(table) <- c("id", "nobs")
    table
}
