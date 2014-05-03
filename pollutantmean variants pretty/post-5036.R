pollutantmean <- function(directory, pollutant, id = 1:332) {
    require(data.table)
    .dt <- rbindlist(lapply(list.files(directory)[id], function(file) fread(file.path(directory, 
        file))))
    mean(.dt[[pollutant]], na.rm = T)
}
