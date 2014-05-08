complete <- function(directory, id = 1:332) {
    if (!class(directory) == "character") {
        print("Not a Character")
    }
    setwd("C:/_rdev")
    the_path <- paste(getwd(), directory, "", sep = "/")
    id_vect <- c()
    cases_vect <- c()
    for (i in id) {
        the_file_name <- paste(the_path, the_file_id(i), ".csv", 
            sep = "")
        some_data <- read.csv(the_file_name, header = TRUE)
        the_complete_obs <- sum(complete.cases(some_data))
        some_data <- ""
        id_vect <- append(id_vect, i)
        cases_vect <- append(cases_vect, the_complete_obs)
    }
    df = data.frame(id_vect, cases_vect)
    colnames(df) <- c("id", "nobs")
    print(df)
}
