complete <- function(directory, id = 1:332) {
    wd = getwd()
    setwd(directory)
    monitors <- dir()
    monitor_data <- lapply(monitors[id], read.csv)
    setwd(wd)
    nobs <- lapply(monitor_data, function(x) {
        sum(complete.cases(x))
    })
    df <- data.frame(cbind(id, nobs))
    colnames(df) <- c("id", "nobs")
    df
}
