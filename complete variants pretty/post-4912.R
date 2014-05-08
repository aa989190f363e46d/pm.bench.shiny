complete <- function(directory, id = 1:332) {
    dir <- c(paste(("C:/Users/tariyaratne/Documents/R/"), "specdata", 
        sep = ""))
    setwd(dir)
    x <- list.files(pattern = ".csv")
    y <- lapply(x, read.table, header = TRUE, sep = ",")
    z <- do.call(rbind, y)
    data <- z[complete.cases(z), ]
    d.fr <- NULL
    for (i in id) {
        d.fr <- rbind(d.fr, data.frame(id = i, nobs = nrow(subset(data, 
            ID == i))))
    }
    print(d.fr)
}
