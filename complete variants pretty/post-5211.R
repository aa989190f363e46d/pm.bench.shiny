complete <- function(directory, id = 1:332) {
    path <- paste("./", directory, sep = "")
    files <- dir(path, full.names = TRUE)
    files_to_open <- files[id]
    d <- data.frame()
    for (i in 1:length(id)) {
        main_data <- read.csv(files_to_open[i])
        MyNobs <- sum(complete.cases(main_data))
        d <- rbind(d, data.frame(id = id[i], nobs = MyNobs))
    }
    print(d)
}
