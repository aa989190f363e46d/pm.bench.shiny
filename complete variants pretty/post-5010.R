complete <- function(directory, id = 1:332) {
    files <- list.files(directory)
    .dt <- data.frame()
    for (i in id) {
        .temp <- na.omit(fread(file.path(directory, files[i])))
        .dt <- rbind(.dt, data.frame(id = i, nobs = nrow(.temp)))
        rm(.temp)
    }
    .dt
}
