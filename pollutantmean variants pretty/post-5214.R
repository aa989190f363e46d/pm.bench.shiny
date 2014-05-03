pollutantmean <- function(directory, pollutant, id = 1:332) {
    path <- paste("./", directory, sep = "")
    files <- dir(path, full.names = TRUE)
    files_to_open <- files[id]
    main_data <- NA
    for (i in 1:length(id)) {
        main_data <- rbind(read.csv(files_to_open[i]), main_data)
    }
    mean(main_data[[pollutant]], na.rm = TRUE)
}
