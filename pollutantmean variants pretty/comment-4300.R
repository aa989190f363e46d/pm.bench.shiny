pollutantmean <- function(directory, pollutant, id = 1:332) {
 total <- 0
 count <- 0
 for (fname in list.files(path = directory, full.names = TRUE)[id]) {
    if(file.exists(fname) & file.info(fname)$size != 0){
       data <- read.csv(fname)
       if (pollutant %in% names(data)) {
           x <- data[pollutant][!is.na(data[pollutant])]
           total <- total + sum(x)
           count = count + length(x)        
       }
    }
 }
      
 if (length(x) > 0) total/count
 
}