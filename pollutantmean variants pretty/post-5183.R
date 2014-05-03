pollutantmean <- function(directory, pollutant, id = 1:332) {
    the_path <- paste(getwd(), directory, "", sep = "/")
    all_data <- ""
    for (i in id) {
        the_file_name <- paste(the_path, the_file_id(i), ".csv", 
            sep = "")
        some_data <- read.csv(the_file_name, header = TRUE)
        if (length(all_data) > 1) {
            all_data <- rbind(some_data, all_data, deparse.level = 1)
        }
        else {
            all_data <- some_data
        }
        some_data <- ""
    }
    num <- grep(pollutant, colnames(all_data))
    print(mean(all_data[[num]], trim = 0, na.rm = TRUE))
}
