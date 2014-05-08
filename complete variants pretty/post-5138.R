complete <- function(directory, id = 1:332) {
    data <- NULL
    nobs <- NULL
    combined <- NULL
    for (i in 1:length(id)) {
        data <- read.csv(paste0(getwd(), "/", directory, "/", 
            sprintf("%03d", id[i]), ".csv"))
        nobs <- append(nobs, length(data[complete.cases(data), 
            ][, 2]))
    }
    combined <- data.frame(id = id, nobs = nobs)
    return(combined)
}
