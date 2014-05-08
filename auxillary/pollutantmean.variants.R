# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4889
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
dir <- paste0(getwd(), "/", directory)
files <- list.files(dir)
data <- numeric()
for(i in id) {
currentMonitor <- read.csv(paste0(dir,"/",files[as.numeric(i)]))
if (pollutant == "nitrate") K<-3 else K<-2
data <- append(data,na.omit(currentMonitor[,K]))
}
return(mean(data,na.rm=TRUE))
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#comment-3422
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
files <- list.files(directory, full.names=T) 
vdata <- NULL
for(i in id) {
currentMonitor <- read.csv(files[i])
vdata <- append(vdata,na.omit(currentMonitor[,pollutant])) 
}
return(round(mean(vdata),3)) 
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4894
####
x <- data.frame()
for (fname in list.files(path = directory, full.names = TRUE)[id])
{
x <- rbind(x, read.csv(fname))
}
mean(x[,pollutant],na.rm=TRUE)

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#comment-3390
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
vLen <- 0
vSum <- 0
fileNames <- list.files(path =directory, full.names = TRUE)[id]
for (fileName in fileNames) { 
dataFile <- read.csv(fileName)
x <- dataFile[pollutant][which(!is.na(dataFile[pollutant])),]
vLen <- vLen + length(x)
vSum <- vSum + sum(x)
}
return(sprintf("%05.3f", vSum/vLen))
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4899
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
path <- paste(getwd(), directory, sep="/")
files <- list.files(path, pattern = ".csv", full.names = TRUE)
index <- files[c(id)]
df <- NULL
for (i in 1:length(index)) {
data <- read.csv(index[i])
df <- rbind.data.frame(df,data)
}
if (pollutant == "nitrate") {
print(mean(df$nitrate, na.rm=TRUE)) 
}
if (pollutant == "sulfate") {
mean(df$sulfate, na.rm=TRUE)
}
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4911
####
pollutantmean <- function(directory, pollutant, id=1:332) {
dir <- c(paste(("C:/Users/tariyaratne/Documents/R/"), directory , sep=""))
setwd(dir)
x <- list.files(pattern=".csv")
y <- lapply(x[id], read.table, header = TRUE, sep=",")
z <- do.call(rbind, y)
meanpollut <- mean(z[ ,paste(pollutant)], na.rm= TRUE) # important not to use complete.cases which will remove observations for the other pollutatnt
return(meanpollut)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4922
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
mydir <- paste("./",directory,"/",sep="")
makeFileNames <- function(id){
idvec2char <- substring(paste("00",id,sep=""),-3)
filename <- paste(mydir,substr(idvec2char,nchar(idvec2char)-2,nchar(idvec2char)),".csv",sep="")
return(filename)
}
filenames <- makeFileNames(id)
## I found do.call here: http://www.stat.berkeley.edu/~s133/Docall.html
mydataframe<- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
## I found which() here: 
## http://stackoverflow.com/questions/4427234/get-column-index-from-label-in-a-data-frame
mycol <- which(colnames(mydataframe)==pollutant)
myvar <- mydataframe[,mycol]
myNonMissingData <- myvar[!is.na(myvar)]
myMean <- mean(myNonMissingData)
return(myMean)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4927
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
#Gets and stores the filenames in the specified directory.
FileNames <- list.files(path=directory)
# Creates the empty data frame with column label headers for mearging specified data file tables.
PolData <- data.frame(Date=as.Date(character()), sulfate=numeric(), nitrate=numeric(), ID=integer())
# Mearges the specified data tables in the for loop.
for(i in id){ 
PolData <- rbind(PolData, read.csv(file=paste(directory, "/", FileNames[i], sep=""),header=TRUE))
}
#Returns the mean of the specified pollutant and removes any NA from the calculations.
mean(x = PolData[,pollutant], na.rm = TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4931
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
observations = c()
for (monitor in id) {
dataset <- read.csv(paste("./", directory, "/", sprintf("%03d", monitor), ".csv", sep=""), TRUE)
observations <- append(observations, dataset[,pollutant])
}
mean(observations, na.rm=TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4935
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
## File Paths to the Data
filenames <- paste("C:/Users/JSTEPHEN/Documents/Coursera/02 R Programming/rprog-data-specdata/" , 
directory , 
"/" , 
formatC( id , width = 3, flag = "0" ) , 
".csv" , 
sep = "")
## Combine data
full_data <- do.call("rbind", lapply(filenames, 
FUN=function(files){read.table(files, header = TRUE, sep = ",")}))
## Summarize the data
bad <- is.na(full_data[ , pollutant])
sum_data <- full_data[ , pollutant][!bad]
## Average
mean(sum_data)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4936
####
pollutantmean <- function(directory, pollutant, id = 1:332){
search <- list.files(directory, pattern="*.csv",full.names=TRUE)
relevant <- search[id]
#print(relevant)
data <- data.frame()
for (i in 1:length(relevant)){
#do.call performs rbind upon the lapply fuction consisting of relevant
# (which is full path for CSV file)
#and puts it into the read.csv file. 
all.data <- do.call(rbind, lapply(relevant,read.csv))
}
good.data <- data.frame(all.data[complete.cases(all.data),])
#print(good.data[1:10,])
answer <- mean(good.data[,pollutant],na.rm=TRUE)
print(answer)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4950
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
filePaths <- file.path(directory, paste(sprintf("%03d", id), ".csv", sep=""))
obs <- numeric()
for (filePath in filePaths){
tmpData <- read.csv(filePath)
tmpData <- subset(tmpData[[pollutant]], !is.na(tmpData[[pollutant]]))
obs <- c(obs, as.vector(tmpData))
}
mean(obs)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4957
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
files <- paste(directory,"/",formatC(id,width=3,flag="0"),".csv",sep="")
data <- ldply(files, function(fn) read.csv(fn))
mean(data[,pollutant],na.rm=TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4961
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
csvfile <- function(id){ 
paste(ifelse(nchar(id) == 1, "00", ifelse(nchar(id) == 2, "0", "")), id, ".csv", sep="") 
}
values <- numeric() 
for (i in id){ 
datafile <- read.csv( paste(directory,"/", csvfile(i), sep=""))
values <- c(values,datafile[,pollutant]) 
} 
mean(values, na.rm=TRUE) 
} 

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4975
####
# PART 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
file.summs <- apply(sapply(X = paste('specdata'
,sprintf('%03i.csv',id)
,sep='/')
,FUN = function(x) {tbl <- na.omit(read.table(x ,header=T
,sep=','
,na.strings='NA'
,stringsAsFactors=FALSE)[[pollutant]])
c(length(tbl),sum(tbl))})
,1
,sum)
file.summs[2]/file.summs[1]
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4978
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
df <- data.frame()
for (i in id){
file <- paste(".//",directory, "/", sprintf("%03d", as.numeric(i)), ".csv", sep = "")
temp <- read.csv(file, header = TRUE)
df <- append(df, temp[pollutant])
}
df <- unlist(df)
mean(df, na.rm = TRUE)
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
df <- data.frame()
names <- list.files(directory, pattern = "*.csv", full.names = TRUE)
for(i in id){
df <- rbind(df, read.csv(names[i], header = TRUE)) 
} 
df <- df[,pollutant]
mean(df, na.rm = TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5012
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
data <- data.frame()
for(i in seq_along(id)){
#adding leading zeros on the file name 
if(id[i] &lt; 10 ){
fileName <- paste("00",id[i],".csv",sep = "")
}else if(id[i] &lt; 100){
fileName <- paste("0",id[i],".csv",sep = "")
}else{
fileName <- paste(id[i],".csv",sep = "")
}
#creating the directory path given the correct file name
fileDir <- paste(directory,"/",fileName,sep = "")
#reading the file and appending
filedf <- read.csv(fileDir)
data <- rbind(data,read.csv(fileDir))
}
#caltute the corresponding mean
mean(data[,pollutant],na.rm = TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5036
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
require(data.table)
.dt <- rbindlist(
lapply(
list.files(directory)[id], 
function(file) 
fread(file.path(directory, file))
)
)
mean(.dt[[pollutant]], na.rm=T)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5047
####
pollutantmean <- function(directory, pollutant, id = 1:332) { 
## load the files according to the id
library(plyr)
id <- formatC( id, width=3, flag="0")
filenames <- paste(id, ".csv", sep="")
fullnames <- file.path (directory, filenames)
ldf <- ldply(fullnames, read.csv)
m <- mean(ldf[[pollutant]], na.rm = T)
m
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5071
####
# function to calculate mean of csv files 
pollutantmean <- function(directory, pollutant,id=1:332){
# looping using the id that is provided by the user
for(i in id){
csvfile <- sprintf("%03d.csv", i)
filepath <- file.path(directory, csvfile)
#checking for the master frame. If master frame exists, it concat to that otherwise creates new one
if(!exists("masterfoo")){
masterfoo <- read.csv(filepath, header=TRUE, sep=",") 
} else {
# reading file using read.csv
foo <- read.csv(filepath, header=TRUE, sep=",")
#concatinating each file's data into master data frame
masterfoo <- rbind(masterfoo,foo)
# removing foo after binding to the master foo
rm(foo)
}
}
## holding only a column that is provided as a parameter by the user
pollutantfoo <- masterfoo[,pollutant]
## removing all NA values from the column
cleanedpollutantfoo <- pollutantfoo[!(is.na(pollutantfoo))]
# calculating mean using mean function
meanvalue <- round(mean(cleanedpollutantfoo), digits=3)
# returning mean
meanvalue 
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5076
####
pollutantmean <- function(directory, pollutant, id) {
## find current working directory and the required follder to loop
datafile <- paste0(getwd(),'/',directory,'/')
## list all the files
datasource <- list.files(datafile)
## create a full directory of all the files in the folder
dataloc <- paste0(datafile,datasource)
## initiate / cheat for the rbind function
full <- read.csv(paste0(datafile,'999.csv'))
##loop through files and read files
for(i in dataloc[id])
{
data <- read.csv(i)
## create a copy of the data required
full <- rbind(full, data)
}
## write the data used into a temporary file
write.csv(full, file='temp.csv')
## print out mean and removed the NA values
print(mean(full[[pollutant]], na.rm=T )) 
##return mean
return(mean(full[[pollutant]], na.rm=T )) 
}
## execute function with values
pollutantmean("specdata", "nitrate", 70:72)

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5085
####
pollutantmean <- function(directory, pollutant, id=1:332){
total <- vector()
for (i in id){
filename <- paste(directory, "/", sprintf("%03d", i), ".csv", sep = "")
my_data <- read.csv(filename)
x <- my_data[ , pollutant]
y <- x[!is.na(x)]
total <- append(total, y)}
return(mean(total))
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5088
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
polreadings <- vector("numeric")
filelist <- dir(directory)
for(n in id) {
fileloc <- paste(directory, "/", filelist[n], sep = "")
aqreadings <- read.csv(fileloc)
polreadings <- c(polreadings, aqreadings[[pollutant]])
}
mean(polreadings, na.rm = TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5148
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
#here we will store our raw values
raw_data <- vector(mode = 'numeric', length = 0)
# reading data and adding it to the raw data vector
# may use sprintf('%03d.csv', id[i]) instead of formatC
for (i in 1:length(id)){
csv_data <- read.csv(paste(directory, '/', 
formatC(as.integer(id[i]),2,flag=0),
'.csv', sep=''))
raw_data <- append(raw_data, csv_data[[pollutant]])
}
# mean
poll_mean <- mean (raw_data, na.rm = TRUE)
return(poll_mean)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5155
####
pollutantmean <- function (directory, pollutant, id = 1:332) { 
AF <- list.files(directory, pattern="*.csv")
total <- NULL 
for (i in id) { 
readfile <- paste(directory, AF[i], sep = "/")
x <- read.csv(readfile)
total <- rbind.data.frame(total, x)
}
if (pollutant == "sulfate") {
means <- mean (total[,"sulfate"], na.rm = TRUE ) 
} 
else {
means <- mean (total[,"nitrate"], na.rm = TRUE ) 
}
means_roundoff <- round(means, digits = 3)
print (means_roundoff) 
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5175
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
mydir <- paste(getwd(),directory,sep="/")
setwd(mydir) 
filelist <- list.files()
filenames <- filelist[id]
mybig <- do.call("rbind", lapply(filenames, read.csv, header = TRUE)) 
#append all files into a single data frame
if(pollutant=="sulfate"){ 
myMeans <- mean(mybig[,2],na.rm=TRUE) #2nd column for sulfate
} else if(pollutant=="nitrate"){
myMeans <- mean(mybig[,3],na.rm=TRUE) #3rd column for nitrate
} else {
print("error")
}
myMeans
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5176
####
pollutantmean <- function(directory, pollutant, id=1:332) {
m <- matrix(NA,1,1)
colnames(m) <- pollutant
for (i in id) { 
x <- read.csv(paste(getwd(),"/",directory,"/",sprintf("%03d",i),".csv",sep="")) 
if(pollutant=="nitrate") { 
m <- c(m,x$nitrate)
} 
else if(pollutant=="sulfate") {
m <- c(m,x$sulfate)
}
}
mean(m,na.rm=T)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5183
####
pollutantmean <- function(directory, pollutant, id=1:332) {
# should we verify incoming arguments before starting? 
# directory where sample data can be found
setwd("C:/_rdev")
the_path <- paste(getwd(),directory,"",sep="/")
# create empty object for rbind() merge
all_data <- "" 
# iterate the id list
for(i in id){ 
# use function to create the filename with 00 prefix & csv suffix
the_file_name <- paste(the_path,the_file_id(i),".csv",sep="") 
# put this iterations file into a data object
some_data <- read.csv(the_file_name,header=TRUE) 
# begin merging the data by observation (vertical, add rows)
if(length(all_data) &gt; 1){
all_data <- rbind(some_data,all_data,deparse.level=1) 
}else{
all_data <- some_data
}
some_data <- ""
}
# get column number to calculate
num <- grep(pollutant, colnames(all_data)) 
# collect pollutant mean excluding NAN NA
print(mean(all_data[[num]],trim=0,na.rm=TRUE))
}
the_file_id <- function(the_num){ 
if(the_num&lt;10){ 
# 1 digit numbers need "00"
the_rtn = paste("00",the_num,sep="") 
}else if(the_num&lt;=99){
# 2 digit numbers need "0"
the_rtn = paste("0",the_num,sep="") 
}else {
the_rtn = the_num
} 
} 

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5194
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
pollutants <- vector();
for(i in id) {
sensor <- read.csv(sprintf("%s/%03d.csv", directory, i))
pollutants <- append(pollutants, sensor[[pollutant]])
}
mean(pollutants, na.rm = TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5214
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
#setup & initialize
path <- paste("./",directory,sep="") #build the path to data files
files <- dir(path,full.names=TRUE) #store all available files
#print(files)
#select relevant files and read them
files_to_open <- files[id] #get a subset of the files
#print(files_to_open)
main_data<-NA
for (i in 1:length(id)){
#print(i) 
#print(files_to_open[i])
main_data <- rbind(read.csv(files_to_open[i]),main_data)
}
#main_data #for debug
mean(main_data[[pollutant]],na.rm=TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5321
####
filenames <- paste0(directory, "/",formatC(id, width=3, flag="0"), ".csv")
a <- numeric()
for( i in 1:length( filenames) ){
x <- read.csv(filenames[[i]])
a <- c(a, x[[pollutant]])
}
round(mean(a, na.rm=TRUE), digits = 3)

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5441
####
pollutantmean<- function(directory, pollutant, ID=1:332) {
directory<- 'specdata'
specdata_files<- paste("specdata/", list.files(directory), sep="")
specdata_all<- do.call(rbind, lapply(specdata_files, read.csv))
if(pollutant=="sulfate") {
specdata_sub<- subset(specdata_all, select=c(ID, sulfate)) 
} else { 
specdata_sub<- subset(specdata_all, select=c(ID, nitrate))
}
good<- complete.cases(specdata_sub)
specdata_good<- specdata_sub[good,]
specdata_sum<- c()
specdata_len<- c()
temp_sum<- numeric()
temp_len<- numeric()
for(i in ID) {
temp<- subset(specdata_good, ID==i)
temp_sum[i]<- sum(temp[,2])
temp_len[i]<- nrow(temp)
specdata_sum[i]<- temp_sum[i]
specdata_len[i]<- temp_len[i] 
} 
bad_sum<- is.na(specdata_sum)
bad_len<- is.na(specdata_len)
sum(specdata_sum[!bad_sum])/sum(specdata_len[!bad_len])
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5491
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
list <- list.files(directory, full.names = TRUE)
f.data <- function(n) {
for(file in n) {
if(!exists("p.data")){
p.data <- read.csv(file, header = TRUE)
}
else if(exists("p.data")) {
temp.data <- read.csv(file, header = TRUE)
p.data <- rbind(p.data, temp.data)
rm(temp.data)
}
}
p.data
}
pollutant.data <- f.data(list)
f.subset <- function(m, p, x) {
if(length(x) &gt; 1) {
for(i in x) {
if(!exists("p.subset")) {
p.subset <- subset(m, select = c(p), ID == i)
}
else if(exists("p.subset")) {
temp.subset <- subset(m, select = c(p), ID == i)
p.subset <- rbind(p.subset, temp.subset)
rm(temp.subset)
}
}
}
if(length(x) == 1) {
p.subset <- subset(m, select = c(p), ID == id)
}
p.subset
}
pollutant.subset <- f.subset(pollutant.data, pollutant, id)
mean(pollutant.subset[,1], na.rm = TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5629
####
pollutantmean <- function(directory , pollutant , id = 1:332){
final <- NULL
for(i in 1:length(id)){
temp <- paste(directory , "/" , sep = "") ##specdata/
temp1 <- sprintf("%03d" , id[i]) ##changes 1 to 001
filename <- paste(temp , temp1 , ".csv" , sep = "") ##specdata/001.csv
file <- read.csv(filename) ##this is the file, read this file
mn <- file[,pollutant]
final <- append(final , mn) ##joins the values vector of all the files
} ##for loop ending
final1 <- mean(final ,na.rm = T)
return(final1)
} ##function ending

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5314
####
# function to calculate mean of csv files 
pollutantmean <- function(directory, pollutant,id=1:332){
# looping using the id that is provided by the user
for(i in id){
csvfile <- sprintf("%03d.csv", i)
filepath <- file.path(directory, csvfile)
#checking for the master frame. If master frame exists, it concat to that otherwise creates new one
if(!exists("masterfoo")){
masterfoo <- read.csv(filepath, header=TRUE, sep=",") 
} else {
# reading file using read.csv
foo <- read.csv(filepath, header=TRUE, sep=",")
#concatinating each file's data into master data frame
masterfoo <- rbind(masterfoo,foo)
# removing foo after binding to the master foo
rm(foo)
}
}
## holding only a column that is provided as a parameter by the user
pollutantfoo <- masterfoo[,pollutant]
## removing all NA values from the column
cleanedpollutantfoo <- pollutantfoo[!(is.na(pollutantfoo))]
# calculating mean using mean function
meanvalue <- round(mean(cleanedpollutantfoo), digits=3)
# returning mean
meanvalue 
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5266
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
## set working directory to passed directory - hold current wd to set back at end
currentPath <- getwd()
folderPath <- paste(currentPath,"/",directory,sep='')
#create empty dataframe for our data
selectedData <- data.frame()
##get all filenames within this directory
files <- c(list.files(folderPath))
#read in the data
for (i in files[id]){
fileContents <- read.csv(paste(folderPath,"/",i,sep=''))
selectedData <- rbind(selectedData,fileContents)
}
#calculate the mean of the selected pollutant ignoring NA values
myMean <- mean(selectedData[,pollutant], na.rm=TRUE)
return(myMean)
}
pollutantmean <- function(directory,pollutant,id =1:332)
{
# get the list of all files
filelist <-list.files(directory,full.names=TRUE)
# get the list of required files 
filestoread <- filelist[c(id)]
dataset <- NULL
# loop through the filelist and assign the data to the data frame
for (i in 1:length(filestoread)) {
myData <- read.csv(filestoread[i])
dataset <-rbind.data.frame(dataset,myData)
}
# assign the colnames
colnames(dataset) <- c("Date","sulfate","nitrate","ID")
#compute the mean
polmean <- mean(dataset[,pollutant], na.rm = TRUE)
polmean
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5093
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
setwd( paste("/tmp", directory, sep = "/") ) 
temp <- paste(formatC(id, width=3, flag="0"), ".csv", sep="") # Get the list of files to be read
sourceframe <- do.call("rbind", lapply(temp, read.csv, header = TRUE)) # Create the data frame
ifelse(pollutant == "sulfate", round(mean(sourceframe[,2], na.rm=TRUE),3), round(mean(sourceframe[,3], na.rm=TRUE),3))
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5105
####
pollutantmean <- function (directory,pollutant, id=1:332){
list <- lapply (paste(directory,"/",sprintf("%03d", id),".csv",sep=""),read.csv)
df<-data.frame(Reduce(rbind,list))
mean(df[,pollutant],na.rm=TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5131
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
idaux <- id +1000
idaux2 <- substr(paste(as.character(idaux),".csv", sep=""),2,8)
# ahora los leo
dataF <- read.csv(paste(getwd(),"/",directory[1],"/",idaux2[1], sep=""))
if (length(id)>1) {
for (i in 2:length(id)) {
df.aux <- read.csv(paste(getwd(),"/",directory[1],"/",idaux2[i], sep=""))
dataF <- rbind(dataF,df.aux)
}
}
#calculo la media de la variable que paso por parámetro
m <- mean(dataF[[pollutant]], na.rm=TRUE)
print(m)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5092
####
pollutantmean <- function(directory,pollutant,id =1:332)
{
# get the list of all files
filelist <-list.files(directory,full.names=TRUE)
# get the list of required files 
filestoread <- filelist[c(id)]
dataset <- NULL
# loop through the filelist and assign the data to the data frame
for (i in 1:length(filestoread)) {
myData <- read.csv(filestoread[i])
dataset <-rbind.data.frame(dataset,myData)
}
# assign the colnames
colnames(dataset) <- c("Date","sulfate","nitrate","ID")
#compute the mean
polmean <- mean(dataset[,pollutant], na.rm = TRUE)
polmean
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5073
####
pollutantmean <- function (directory, pollutant, id = 1:332) {
# get all numbers to be averaged
nums <- c()
for (i in id) {
# convert the file number to character and prepend zeroes if necessary
filename <- if (i < 10) {
paste("00", as.character(i), sep = "")
} else if (i < 100) {
paste("0", as.character(i), sep = "")
} else {
as.character(i)
}
# assume specdata is a subdirectory of the present working directory
filepath <- paste(c(directory, "/", filename, ".csv"), collapse = "")
# append new data to nums
dataframe <- read.csv(filepath)
newnums <- na.omit(dataframe[, pollutant])
nums <- c(nums, newnums)
}
# get the mean
mn <- mean(nums)
# round the mean to three digits
round(mn, digits = 3)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-5045
####
corr <- function(directory, threshold = 0) {
id <- list.files(directory)
cv <- c() 
for (i in id) {
data <- read.csv(paste(directory, '/', i, sep=""))
comp <- complete.cases(data)
sub <- subset(data,comp)
if (nrow(sub) > threshold) {
cv <- c(cv, cor(sub$sulfate, sub$nitrate))
}
}
cv
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4982
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
for (file_id in id) {
filename<-paste(directory,"/",formatC(file_id, width=3, flag="0"),".CSV",sep="")
if(!exists("outData")) {
outData <- read.csv(filename)
}
else {
tempData <-read.csv(filename)
outData <- rbind(outData,tempData)
}
} 
mean(outData[[pollutant]],na.rm=TRUE)
}

####
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1173#post-4890
####
pollutantmean <- function(directory, pollutant, id = 1:332) {
require(stringr)
#paddedIdList is a list of padded id values used to create
#the correct file names 
#for each monitor data file
paddedIdList <- lapply(1: length(id),function(index) { str_pad(id[index], width=3, side="left", pad="0")})
paddedIdVec <- unlist(paddedIdList) #stores a vector of padded ids
#A list of names for each monitor data file
fileNameList <- lapply(1:length(paddedIdList),function(index) { paste0(getwd(),"/",directory,"/",paddedIdVec[index],".csv") })
fileNameVec <- unlist(fileNameList)
#polluntantFrameList creates a 
#list of data frames that store 
#sulfate and nitrate measurements 
#for each monitor
pollutantFrameList <- lapply(1:length(fileNameList), function(index) { read.csv(fileNameVec[index])})
if(pollutant == "sulfate")
{
sulfateDataList <- lapply(1:length(pollutantFrameList),function(index){ pollutantFrameList[[index]]["sulfate"]})
sulfateVec <- unlist(sulfateDataList)
sulfateMean <- mean(sulfateVec,na.rm = TRUE)
return(sulfateMean)
}
if(pollutant == "nitrate")
{
nitrateDataList <- lapply(1:length(pollutantFrameList),function(index){ pollutantFrameList[[index]]["nitrate"]})
nitrateVec <- unlist(nitrateDataList)
nitrateMean <- mean(nitrateVec,na.rm = TRUE)
return(nitrateMean)
}
}