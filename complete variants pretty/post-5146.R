complete <- function(directory, id = 1:332) {
    all <- data.frame(id = id, nobs = NA)
    for (i in 1:length(id)) {
        csv <- read.csv(paste(directory, "/", formatC(as.integer(id[i]), 
            2, flag = 0), ".csv", sep = ""))
        all$nobs[i] <- dim(csv[!is.na(csv$sulfate) & !is.na(csv$nitrate), 
            ])[1]
    }
    return(all)
}
