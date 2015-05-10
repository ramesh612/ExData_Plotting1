##
## Author: Ramesh Balasubramanian
## Course: Exploratory Data Analysis
## Date: 05/10/2015
##
##
## installs libName if it is not already installed.
## if library exists, but fails to load - halts the program
## 
load_library <- function (libName) {
  if(!require(libName, character.only = TRUE)){
    install.packages(libName)
    if(!require(libName, character.only = TRUE)){
      stop("could not install ", libName)
    }
  }
}

##
## downloads zipped data file into a temporary file
## unzips the file, greps the file for the required data (first 2 days in Feb 2007)
## creates a DateTime column formed by combining Date and Time columns
## returns that data to caller. All temporary objects/files created are cleaned up.
##
read_data <- function (url) {
  temp <- tempfile()
  download.file(url, temp, method = 'curl', quiet = TRUE)
  fileName <- unzip(temp, list = TRUE)$Name
  file.remove(temp)
  fh <- file(fileName, "r")
  lines <- grep("(^Date)|(^[1|2]/2/2007)", readLines(fh), value=TRUE)
  temp<- tempfile()
  cat(lines, sep="\n", file=temp)
  close(fh)
  
  data <- fread(temp, sep=";", header=TRUE, na.strings="?")
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  data$DateTime <- as.POSIXct(paste(as.Date(data$Date), data$Time))
  
  file.remove(temp)
  return(data)
}

## if required install and then load "data.table"
load_library("data.table") 

## url of the zip file that contains data for analysis
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

## read the required subset of data into an object
data <- read_data(url)

## plot Sub_metering_1 against DateTime and add more lines in the same plot, add
## a legend and save the file as a png
png("plot3.png") # width and height default to 480
plot(data$Sub_metering_1~data$DateTime, 
     xlab="", type="l", ylab="Energy sub metering")
lines(data$Sub_metering_2~data$DateTime, col = "red")
lines(data$Sub_metering_3~data$DateTime, col = "blue") 

legend("topright", cex=1, col=c("black", "red", "blue"), lwd = 1, y.intersp = 1,
       xjust = 1, text.width = strwidth("Sub_metering_1"), legend = c("Sub_metering_1",
                                                                      "Sub_metering_2",
                                                                      "Sub_metering_3"))

dev.off()

## cleanup.
rm(data)
rm(url)
