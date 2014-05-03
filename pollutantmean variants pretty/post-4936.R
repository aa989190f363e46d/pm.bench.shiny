pollutantmean <- function(directory, pollutant, id = 1:332) {
    search <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    relevant <- search[id]
    data <- data.frame()
    for (i in 1:length(relevant)) {
        all.data <- do.call(rbind, lapply(relevant, read.csv))
    }
    good.data <- data.frame(all.data[complete.cases(all.data), 
        ])
    answer <- mean(good.data[, pollutant], na.rm = TRUE)
    print(answer)
}
