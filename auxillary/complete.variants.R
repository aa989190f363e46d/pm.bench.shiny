# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4891
complete <- function(directory, id = 1:332) {
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
## Return a data frame of the form:
## id nobs
## 1 117
## 2 1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
#Create the character vector of the directory you want to use
storeDirectory <- paste0(getwd(),"/",directory,"/")
#Creates specdata directory
if(dir.create(directory,showWarnings = FALSE) != 0)
{
dir.create(directory,showWarnings = FALSE)
}
#Downloads compressed specdata files if 
#specdata directory is empty
if(length(list.files(directory)) == 0)
{
download.file("https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip",destfile = paste0(storeDirectory,"specdata.zip"),method = "internal") #downloads file
unzip(zipfile = paste0(storeDirectory,"specdata.zip")) #unzips monitor files
}
fileNames <- list.files(storeDirectory) #store the names of the CSV files
zipLoc <- grep(pattern = ".zip",x = fileNames, fixed = TRUE) #location of specdata.zip
if(length(zipLoc) > 0) #Checks if the zipped file is in the directory
{
fileNames <- fileNames[-zipLoc] # Remove the zipfile from the file name list
}
fileNameVector <- paste0(storeDirectory,fileNames) #pastes the directory name to each CSV file
fileDataList <- lapply(1:length(fileNameVector), function(index) read.csv(fileNameVector[index])) #creates a dataframe for each monitor
numComp <- lapply(1:length(fileNameVector), function(index) {sum(complete.cases( fileDataList[[index]]))} ) #number of complete.cases
numCompVec <- unlist(numComp) #stores the number of complete cases for each monitor in the data set in a vector
#completeObsFrame - a dataframe of monitor ids which are stored in the first column
# and the number of complete cases for each monitor stored in the second column
completeObsFrame <- data.frame(cbind(1:length(fileNameVector),numCompVec))
colnames(completeObsFrame) <- c("id","nobs")
return(completeObsFrame[id,]) #returns the relevant rows in the complete cases data frame based on id number
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#comment-3741
complete <- function(directory, id) {
setwd(paste("my local directory which is the parent dir of specdata", 
directory, sep = ""))
files <- dir()
data <- data.frame()
for (i in id){
file.i <- read.csv(files[i])
obs <- sum(complete.cases(file.i))
data <- rbind(data, cbind(i, obs)) 
}
data
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4893
complete <- function(directory, id=1:332){
dw<-c("C:/Users/Diego/Desktop/Coursera_Data_Science/Programming Assessment/Week2/") ##Set the directory
setwd(paste(dw,directory,sep="")) ##Set the folder
file<-list.files(pattern=".csv") ## List all the files
table<-data.frame()##Initializing a null vector
h<-id
for (i in 1:length(h)){
x<-read.csv(file[h[i]]) ##read files indexed
y<-c(h[i],length(which(!is.na(x[,"nitrate"]*x[,"sulfate"])))) ##identify number and assign to a vector y
table<-as.data.frame(rbind(table,y)) ##create a table
}
colnames(table)<-c("id","nobs") ##data frame column names
table
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4896
complete <- function(directory, id = 1:332) {
x <- data.frame()
flist <- list.files(path = directory, full.names = TRUE)
for (fname in flist[id])
{
df <- read.csv(fname)
dfgood <- !logical(length = nrow(df)) ## start all TRUE
for (j in 1:ncol(df))
{
dfgood <- dfgood & !is.na(df[,j])
}
x <- rbind(x, c(match(fname, flist), sum(dfgood)))
}
names(x) <- c("id", "nobs")
x
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4903
complete <- function(directory, id = 1:332) {
path <- paste(getwd(), directory, sep="/")
files <- list.files(path, pattern = ".csv", full.names = TRUE)
index <- files[c(id)]
nobs <- NULL
for (i in 1:length(index)) {
mydata <- read.csv(index[i]) 
x <- sum(complete.cases(mydata))
nobs <-c(nobs,x) 
}
df <-data.frame(id, nobs)
colnames(df) <-c("id", "nobs")
df
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4912
complete <- function(directory, id = 1:332) {
dir <- c(paste(("C:/Users/tariyaratne/Documents/R/"), "specdata", sep=""))
setwd(dir)
x <- list.files(pattern=".csv")
y <- lapply(x, read.table, header = TRUE, sep=",")
z <- do.call(rbind, y)
data <- z[complete.cases(z), ]
d.fr <- NULL
for (i in id) {
d.fr <- rbind(d.fr, data.frame(id=i, nobs= nrow(subset(data, ID==i))))
}
print(d.fr)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4915
complete <- function(directory, id = 1:332) {
matt <- matrix(, nrow = length(id), ncol = 2)
for (i in 1:length(id)) {
csv <- paste(direc,"/",sprintf("%03d",id[i]),".csv",sep="")
temp <- read.csv(csv)
matt[i,1] <- id[i]
matt[i,2] <- nrow(temp[complete.cases(temp),])
}
df <- data.frame(matt)
colnames(df) <- c("id", "nobs")
df
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4917
complete <- function(directory, id = 1:332) {
#Gets and stores the filenames in the specified directory.
FileNames <- list.files(path=directory)
#Sets up the observation summary matrix for compiling the observations
# from each monitor.
Obs <- matrix(nrow=0,ncol=2)
# Loop will count and summarize the number of complete cases for each monitor id.
for(i in id){
#Reads individual monitor data file
MonData <- read.csv(file=paste(directory, "/", FileNames[i], sep=""),header=TRUE)
#Retains the complete cases and purges the NA rows from monitor data.
MonData <- MonData[complete.cases(MonData),]
#Compiles a list of observation counts for each monitor / iteration of loop.
Obs <- rbind(Obs, c(i, nrow(MonData)))
}
#Creates a dataframe from the observation matrix compliation with headings.
NumberOfObs <- data.frame(id=(Obs[,1]), nobs=(Obs[,2]))
#Must remove the initial "NA" from initially setting up the Obs matrix
NumberOfObs <- NumberOfObs[complete.cases(NumberOfObs),]
#Reports the completed dataframe.
NumberOfObs
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4933
complete <- function(directory, id = 1:332)
{
df <- data.frame(id=integer(), nobs=integer(), stringsAsFactors=FALSE)
for (monitor in id) {
dataset <- read.csv(paste("./", directory, "/", sprintf("%03d", monitor), ".csv", sep=""), TRUE)
df <- rbind(df, data.frame(id=monitor, nobs=sum(complete.cases(dataset))))
}
df
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4934
complete <- function(directory, id = 1:332) {
files <- list.files(directory)
df <- NULL 
for (i in id) {
myFiles <- paste(directory,files[i],sep="/")
for (file in myFiles) {
temp <- read.csv(paste(getwd(),"/", file, sep=""), header=TRUE)
comp <- sum(complete.cases(temp))
df <- rbind(df, data.frame(id=i, nobs=comp))
}}}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#comment-3864
complete2 <- function(directory, id = 1:332) {
files <- list.files(directory, full.names = TRUE)
df = NULL
for(i in id) {
temp <- read.csv(files[i], header = TRUE)
comp <- sum(complete.cases(temp))
df <- rbind(df, data.frame(id = i, nobs = comp))
}
df
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4938
complete <- function(directory, id = 1:332) {
#In order to get read.csv to function properly, you need to supply it with a full path to the file. 
#This is where the "full.names=TRUE" thing comes in.
search <- list.files(directory, pattern="*.csv",full.names=TRUE)
relevant <- search[id]
total.data <- data.frame()
for (i in 1:length(id)){
raw.data <- read.csv(relevant[i])
#Apparently a sum works just as well as 'table' does.
comp.data <- sum(complete.cases(raw.data))
#property of rbind to add new rows to existing dataframe, and tag=value to define what the data is
total.data <- rbind(total.data, data.frame(id=i, nobs=comp.data))
}
print(total.data)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4943
complete <- function(directory, id = 1:332) {
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
mydata <- full_data[complete.cases(full_data), ]
## Summarize the data
idx <- split(1:nrow(mydata), mydata$ID)
DF <- data.frame(ID=sapply(idx, function(i) mydata$ID[i[1]]),
nobs=sapply(idx, function(i) NROW(mydata[ , "ID"][i])) )
DF
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4953
complete <- function(directory, id = 1:332) {
vectorID <- numeric()
vectorNobs <- numeric()
for (i in id){
filePath <- file.path(directory, paste(sprintf("%03d", i), ".csv", sep=""))
tmpData <- read.csv(filePath)
vectorID <- append(vectorID, i)
vectorNobs <- append(vectorNobs, sum(complete.cases(tmpData)))
}
data.frame(id=vectorID, nobs=vectorNobs)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4958
complete <- function(directory, id = 1:332) {
resultdf <- data.frame(id=integer(0), nobs=numeric(0))
for(curid in id) {
fn <- paste(directory,"/",formatC(curid,width=3,flag="0"),".csv",sep="")
data <- read.csv(fn)
resultdf = rbind(resultdf,data.frame(id=curid, nobs=sum(complete.cases(data))))
}
resultdf
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4974
complete <- function(directory, id = 1:332) {
## Creates a blank data frame to store the two columns
## creates a list of the full file names and stores it to names
## The for loop Increments though the id vector
## reads in the data, 
## complete.cases() makes a logical vecto(True = complete case), You can NOT use na.omit() w/sum()
## sum() on a logical vector returns the total number of TRUE values in the vector
## this is stored to temp
## rbind() adds the new and correctly labled data.frame to the end of df
## Where, i = the current 'id' and temp = the sum of completed values
## Returns the data.frame of interest
df = data.frame()
names <- list.files(directory, pattern = "*.csv", full.names = TRUE)
for(i in id){
temp <- sum(complete.cases(read.csv(names[i], header = TRUE)))
df <- rbind(df, data.frame('id' = i, 'nobs' = temp))
} 
df
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4983
complete <- function(directory, id = 1:332) {
outData <- data.frame(id=integer(0),nobs=integer(0))
for (file_id in id) {
filename<-paste(directory,"/",formatC(file_id, width=3, flag="0"),".CSV",sep="")
tempData <-read.csv(filename)
tempData2 <- na.omit(tempData)
obsCount <- nrow(tempData2)
outDataRow <- data.frame(id=file_id,nobs=obsCount)
outData <- rbind(outData,outDataRow)
}
outData
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4986
# PART 2 
complete <- function(directory, id = 1:332) { 
counts <- cbind( id 
,sapply(X = paste( 'specdata' 
,sprintf('%03i.csv',id) 
,sep='/') 
,FUN = function(x) nrow(na.omit(read.table(x 
,header=T 
,sep=',' 
,na.strings='NA' 
,stringsAsFactors=FALSE))))) 
colnames(counts) <- c('id','nobs') 
as.data.frame(counts) 
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-4992
complete <- function(directory,id=1:332){
data1 <- list.files(directory)
neededfiles <- paste("specdata/",data1[id], sep="")
result.mat <- mat.or.vec(length(id),2)
temp <- NULL
for (i in 1:length(neededfiles)){
data2<-read.csv(neededfiles[i])
temp1<-rbind.data.frame(temp,data2)
m<-complete.cases(temp1)
m1<-sum(m)
result.mat[i,1] <- id[i]
result.mat[i,2] <- m1
}
mydataframe<-data.frame(result.mat)
names(mydataframe)<-c("id","nobs")
return(mydataframe)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5010
complete <- function(directory, id = 1:332) {
files <- list.files(directory)
.dt <- data.frame()
for (i in id){
.temp <- na.omit(fread(file.path(directory, files[i])))
.dt <- rbind(.dt, data.frame(id=i, nobs=nrow(.temp)))
rm(.temp)
}
.dt
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5044
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
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5089
complete <- function(directory, id=1:332){
nobs <- vector()
for (i in id){
filename <- paste(directory, "/", sprintf("%03d", i), ".csv", sep = "")
my_data <- read.csv(filename)
x <- sum(complete.cases(my_data))
nobs <- append(nobs, x)}
final <- data.frame(id, nobs)
return(final)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5132
complete <- function(directory, id = 1:332) {
idaux <- id +1000
idaux2 <- substr(paste(as.character(idaux),".csv", sep=""),2,8)
nobs <- vector()
for (i in 1:length(id)) {
df.aux <- read.csv(paste(getwd(),"/",directory[1],"/",idaux2[i], sep=""))
good <- complete.cases(df.aux)
nobs[i] <- nrow(df.aux[good,][,])
}
df.salida <- data.frame(id,nobs)
print(df.salida)
} 
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5138
complete <- function(directory, id = 1:332) {
data <- NULL
nobs <- NULL
combined <- NULL
for(i in 1:length(id)) {
## individually read each file into a data.frame called 'data'
data <- read.csv(paste0(getwd(), "/", directory, "/", sprintf("%03d", id[i]), ".csv")) 
nobs <- append(nobs, length(data[complete.cases(data),][,2]))
}
combined <- data.frame(id = id, nobs = nobs) 
return(combined)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5146
complete <- function(directory, id = 1:332) {
#empty data frame to store output data
all <- data.frame( id = id, nobs = NA)
for (i in 1:length(id)){
#load CSV files
csv <- read.csv(paste(directory, '/', 
formatC(as.integer(id[i]),2,flag=0),
'.csv', sep=''))
#measure number of complete cases and store in all$nobs
all$nobs[i] <- dim(csv[!is.na(csv$sulfate) & !is.na(csv$nitrate),])[1]
}
return(all)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5156
complete <- function (directory, id = 1:332) { 
AF = list.files(directory, pattern="*.csv")
total <- NULL 
for (i in id) { 
readfile <- paste(directory, AF[i], sep = "/")
x <- read.csv(readfile)
no_nas <- na.omit(x)
total_obs <- nrow(no_nas)
single <- (data.frame(id = i, nobs = total_obs))
total <- rbind (total, single) 
}
total
} 
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5174
complete <- function(directory, id = 1:332) {
mydir <- paste(getwd(),directory,sep="/")
setwd(mydir) 
filelist <- list.files()
filenames <- filelist[id]
nobs <- numeric()
# for(i in seq_along(filenames)) { 
for(i in filenames){
singlefile <- read.csv(i)
p1 <- singlefile[,2]
p2 <- singlefile[,3]
ok <- complete.cases(p1, p2)
nobs <- c(nobs,sum(ok)) 
}
data.frame(id,nobs)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5180
complete <- function(directory, id = 1:332) { 
if(!class(directory)== "character"){
print("Not a Character")
}
# directory where sample data can be found
setwd("C:/_rdev")
the_path <- paste(getwd(),directory,"",sep="/")
# reporting vectors
id_vect <- c()
cases_vect <- c()
# iterate the id list
for(i in id){ 
# use function to create the filename with 00 prefix & csv suffix
the_file_name <- paste(the_path,the_file_id(i),".csv",sep="")
# put this iterations file into a data object
some_data <- read.csv(the_file_name,header=TRUE)
the_complete_obs <- sum(complete.cases(some_data))
some_data <- ""
id_vect <- append(id_vect,i)
cases_vect <- append(cases_vect,the_complete_obs)
}
df = data.frame(id_vect,cases_vect)
colnames(df) <- c("id","nobs")
print(df)
}
the_file_id <- function(the_num){ 
if(the_num<10){
# 1 digit numbers need "00"
the_rtn = paste("00",the_num,sep="")
}else if(the_num<=99){
# 2 digit numbers need "0"
the_rtn = paste("0",the_num,sep="")
}else {
the_rtn = the_num
} 
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5190
complete<-function(directory,id=1:332) 
{
comcollect<-data.frame()##To collate number of complete cases
comcount<-0 ## To count number of complete cases
collate<-data.frame() ## To consolidate final data frame of all findings
if(directory=="specdata") #Check Argument Passed to Function
setwd("E:/Coursera/Working Directory/specdata")##Set Working Folder
filename<-list.files() ##Collect all filenames in directory to a vector
count<-as.numeric(id) ## Set counter for for loop from id argument
for(i in count){ #For Loop Kicks Off
file<-read.csv(filename[i]) 
good<-complete.cases(file)
comcollect<-file[good,]
comcount<-nrow(comcollect)
##print(comcount)
df<-data.frame("id"=i,"nobs"=comcount)##Create Dataframe with finding 
collate<-rbind(collate,df) #Collate Dataframe from all loop iterations
}
print (collate) #Printing Complete Cases
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5196
complete <- function(directory, id = 1:332) {
nobs <- vector()
for (i in id) {
sensor <- read.csv(sprintf("%s/%03d.csv", directory, i))
nobs <- append(nobs, sum(!is.na(sensor$sulfate) & !is.na(sensor$nitrate)))
}
data.frame(id, nobs)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5211
complete <- function(directory, id = 1:332) {
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'id' is an integer vector indicating the monitor ID numbers
## to be used
## Return a data frame of the form:
## id nobs
## 1 117
## 2 1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
#setup & initialize
path <- paste("./",directory,sep="") #build the path to data files
files <- dir(path,full.names=TRUE) #store all available files
#select relevant files and read them
files_to_open <- files[id] #get a subset of the files
#loop and compute complete sets
d<-data.frame()
for (i in 1:length(id)){
main_data <- read.csv(files_to_open[i])
MyNobs <- sum(complete.cases(main_data)) #Complete -&gt;T(1) ** NA -&gt;F(0)
d<-rbind(d,data.frame(id=id[i],nobs=MyNobs))
}
#print output
print(d)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5268
complete <- function(directory, id = 1:332) {
## set working directory to passed directory - hold current wd to set back at end
currentPath <- getwd()
folderPath <- paste(currentPath,"/",directory,sep='')
#create empty dataframe for our data
selectedData <- data.frame()
##get all filenames within the directory
files <- c(list.files(folderPath))
#read in the data - selecting only files that are passed in via the id parameter
for (i in files[id]){
#read in this file
fileContents <- read.csv(paste(folderPath,"/",i,sep=''))
#create single row 2 col vector for file name and count of complete cases
thisRow <- cbind(as.numeric(strsplit(i,".csv")),nrow(fileContents[complete.cases(fileContents),]))
#append to the dataframe
selectedData <- rbind(selectedData,thisRow)
}
#assign column names
names(selectedData) <- c("id","nobs")
#return data
return (selectedData)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5270
complete <- function(directory, id = 1:332) {
# get all id/nobs pairs and store in a matrix
numrows <- length(id)
mtrx <- matrix(NA, nrow = numrows, ncol = 2)
colnames(mtrx) <- c("id", "nobs")
for (i in 1:numrows) {
filenum <- id[i]
# convert the file number to character and prepend zeroes if necessary
filename <- if (filenum < 10) {
paste("00", as.character(filenum), sep = "")
} else if (filenum < 100) {
paste("0", as.character(filenum), sep = "")
} else {
as.character(filenum)
}
# assume specdata is a subdirectory of the present working directory
filepath <- paste(c(directory, "/", filename, ".csv"), collapse = "")
# put new data in matrix
dataframe <- read.csv(filepath)
numcases <- sum(complete.cases(dataframe))
mtrx[i, ] <- c(filenum, numcases)
}
as.data.frame(mtrx)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5322
filenames <- paste0(directory, "/",formatC(id, width=3, flag="0"), ".csv")
ids <- numeric()
nobs <- numeric()
for( i in 1:length( filenames) ){
x <- read.csv(filenames[[i]])
ids <- c(ids, i)
nobs <- c(nobs, sum(complete.cases(x)))
}
df <- data.frame(ids, nobs)
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5492
complete <- function(directory, id = 1:332) {
list <- dir(directory, full.names = TRUE)
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
good <- function(i) {
subset.i <- subset(pollutant.data, ID == i)
sum(complete.cases(subset.i))
}
nobs.data <- data.frame()
for(i in id) {
i.data <- data.frame(id = i, nobs = good(i))
nobs.data <- rbind(nobs.data, i.data)
rm(i.data)
}
nobs.data
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5817
complete <- function(directory, id= 1:332) {
wd = getwd() # Store current directory
setwd(directory) # Move to data directory
monitors <- dir() # Gather all monitors
monitor_data <- lapply(monitors[id], read.csv) # Read monitors given in "id"
setwd(wd) # Restore original directory
# List of counts of complete cases for each monitor; length(nobs) == length(id)
nobs <- lapply(monitor_data, function(x) {sum(complete.cases(x))})
# Build a data frame by column stacking the id and nobs lists
df <- data.frame(cbind(id, nobs))
colnames(df) <- c("id","nobs") # Give the dataframe some names
df
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5857
complete <- function(directory, id = 1:332) {
## 'directory' is a character vector of length 1 indicating the location of the CSV files
## 'id' is an integer vector indicating the monitor ID numbers to be used
fileformat <- paste0(directory, "/%03d", ".csv")
#make file name strings from supplid ids
files <- c(sprintf(fileformat, id))
#combine number of complete cases from specified files using rbind the list of files
pmcomplete <- do.call("rbind", lapply(files, function(xfile) {
this.file <- read.csv(xfile, na.strings=c("NA"), stringsAsFactors=FALSE)
cbind(id=this.file$ID[1], nobs = sum(complete.cases(this.file)))
}))
return(data.frame(pmcomplete)) #return
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-5937
complete <- function(directory, id = 1:332) {
nobs<-numeric(0)
for(i in id){
filename<-file.path(directory,paste(formatC(i, width = 3,flag = "0"),".csv",sep=""))
data<-read.csv(filename)
nobs<-c(nobs,nrow(na.omit(data)))
}
data.frame(id, nobs)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-6395
complete <- function(directory, id = 1:332) {
# set up the directory 
dir <- paste(getwd(), "/", directory, sep="")
setwd(dir)
# merge all 332 csv files into a one data set (complete.dat)
files <- list.files(path = getwd()) 
complete.dat <- do.call("rbind", lapply(files, read.csv, header = TRUE))
# move up a folder to automatically go back to the main directory
# when running the function multiple times
setwd("..")
# subset the complete data set according to the id specified in the third
# argument of the function
pol.subset <- complete.dat[complete.dat$ID %in% id, ]
# get the complete cases for the IDs specified in the function
not.null <- complete.cases(pol.subset) # rows with no missing values
not.null <- cbind(pol.subset$ID, not.null) # rows with no missing values with corresponding IDs
results <- tapply(not.null[,2], not.null[,1], sum) # results in "array" format
# create data frame to display the results
df = as.data.frame.table(results)
colnames(df) <- c("ids", "nobs")
# check if "id" in the function is called in ascending or descending order
if (id[1] < id[length(id)]) {
df = df
} else {
df = df[order(df$ids, decreasing = T), ]
}
df
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-6731
complete <- function(directory, id = 1:332) {
wd <- getwd()
setwd(directory) # change directory to specdata 
files <- dir() # get all CSV files
returnDf <- data.frame() # create a data frame that holds complete cases
for (i in id) {
data <- data.frame(read.csv(files[i])) # read a CSV file
x <- complete.cases(data["sulfate"]) # find complete cases for sulfate
y <- complete.cases(data["nitrate"]) # find complete cases for nitrate
#do a logical AND of x and y vectors. 
#TRUE is returned only for records in which sulfate and nitrate don't have NA values
z <- x & y 
completeCases <- length(z[z == TRUE]) #get the number of TRUEs from the z vector
returnDf <- rbind(returnDf, c(i, completeCases)) #add file number and no. of complete cases to data frame
}
setwd(wd)
colnames(returnDf) <- c("id", "nobs") #set columns for the data frame
returnDf
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#comment-4424
complete <- function(directory, id = 1:332) {
files <- list.files(pattern="*.csv")
nobs <- integer()
for (i in id) {
fi <- read.csv(files[i])
nobs <- c(nobs, length(which(complete.cases(fi))) )
} #end of for loop
return(data.frame(id,nobs))# join the vectors id and nobs 
# into a data frame
} # end of function complete
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-6852
complete <- function(directory, id = 1:332) {
allData <- data.frame()
for (i in id){
myData <- read.csv(paste(directory,sprintf("%03d.csv", i), sep ="/"), header = TRUE)
completeCases <- myData[complete.cases(myData),]
nobs <- nrow(completeCases) 
allData <- rbind(allData,data.frame(id=i,nobs=nobs))
} 
return(allData)
}
# https://class.coursera.org/rprog-002/forum/thread?thread_id=1174#post-6908
complete<-function(directory,id=1:332) {
df<-data.frame(id=id,nobs=rep(-1,length(id)))
c<-1
for (i in id) {
d<-read.csv(paste(directory,'\\',formatC(i,width=3,format='d',flag='0'),'.csv',sep=''))
df$nobs[c]<-sum(complete.cases(d))
c<-c+1
}
return(df)
}