pollutantmean <- function(directory, pollutant, id = 1:332) {
    require(stringr)
    paddedIdList <- lapply(1:length(id), function(index) {
        str_pad(id[index], width = 3, side = "left", pad = "0")
    })
    paddedIdVec <- unlist(paddedIdList)
    fileNameList <- lapply(1:length(paddedIdList), function(index) {
        paste0(getwd(), "/", directory, "/", paddedIdVec[index], 
            ".csv")
    })
    fileNameVec <- unlist(fileNameList)
    pollutantFrameList <- lapply(1:length(fileNameList), function(index) {
        read.csv(fileNameVec[index])
    })
    if (pollutant == "sulfate") {
        sulfateDataList <- lapply(1:length(pollutantFrameList), 
            function(index) {
                pollutantFrameList[[index]]["sulfate"]
            })
        sulfateVec <- unlist(sulfateDataList)
        sulfateMean <- mean(sulfateVec, na.rm = TRUE)
        return(sulfateMean)
    }
    if (pollutant == "nitrate") {
        nitrateDataList <- lapply(1:length(pollutantFrameList), 
            function(index) {
                pollutantFrameList[[index]]["nitrate"]
            })
        nitrateVec <- unlist(nitrateDataList)
        nitrateMean <- mean(nitrateVec, na.rm = TRUE)
        return(nitrateMean)
    }
}
