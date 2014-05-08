complete <- function(directory, id = 1:332) {
    search <- list.files(directory, pattern = "*.csv", full.names = TRUE)
    relevant <- search[id]
    total.data <- data.frame()
    for (i in 1:length(id)) {
        raw.data <- read.csv(relevant[i])
        comp.data <- sum(complete.cases(raw.data))
        total.data <- rbind(total.data, data.frame(id = i, nobs = comp.data))
    }
    print(total.data)
}
