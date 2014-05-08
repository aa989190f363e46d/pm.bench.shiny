complete <- function(directory, id = 1:332) {
    dir <- paste(getwd(), "/", directory, sep = "")
    setwd(dir)
    files <- list.files(path = getwd())
    complete.dat <- do.call("rbind", lapply(files, read.csv, 
        header = TRUE))
    setwd("..")
    pol.subset <- complete.dat[complete.dat$ID %in% id, ]
    not.null <- complete.cases(pol.subset)
    not.null <- cbind(pol.subset$ID, not.null)
    results <- tapply(not.null[, 2], not.null[, 1], sum)
    df = as.data.frame.table(results)
    colnames(df) <- c("ids", "nobs")
    if (id[1] < id[length(id)]) {
        df = df
    }
    else {
        df = df[order(df$ids, decreasing = T), ]
    }
    df
}
