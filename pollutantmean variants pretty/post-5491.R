pollutantmean <- function(directory, pollutant, id = 1:332) {
    list <- list.files(directory, full.names = TRUE)
    f.data <- function(n) {
        for (file in n) {
            if (!exists("p.data")) {
                p.data <- read.csv(file, header = TRUE)
            }
            else if (exists("p.data")) {
                temp.data <- read.csv(file, header = TRUE)
                p.data <- rbind(p.data, temp.data)
                rm(temp.data)
            }
        }
        p.data
    }
    pollutant.data <- f.data(list)
    f.subset <- function(m, p, x) {
        if (length(x) > 1) {
            for (i in x) {
                if (!exists("p.subset")) {
                  p.subset <- subset(m, select = c(p), ID == 
                    i)
                }
                else if (exists("p.subset")) {
                  temp.subset <- subset(m, select = c(p), ID == 
                    i)
                  p.subset <- rbind(p.subset, temp.subset)
                  rm(temp.subset)
                }
            }
        }
        if (length(x) == 1) {
            p.subset <- subset(m, select = c(p), ID == id)
        }
        p.subset
    }
    pollutant.subset <- f.subset(pollutant.data, pollutant, id)
    mean(pollutant.subset[, 1], na.rm = TRUE)
}
