# == Checking for datafile availability ==
myfile <- "exdata_data_household_power_consumption.zip"
if (!file.exists(myfile)){
    #file is not present in working directory. Let's download it.
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                  myfile,
                  "curl", #I'm in a Linux box. Must be removed for Win pc's.
                  TRUE,
                  "wb")
    if (!file.exists(myfile)) stop("download failed.")    
}
# == Loading data ==
# At this point the data file should be present at the wd. Let's start reading 
# data from it. To avoid uncompressing data, I'll read it directlty from inside 
# the Zip.
mydatafile <- "household_power_consumption.txt"
myconn <- unz(myfile, mydatafile)
dataset <- read.table(myconn, TRUE, ";", na.strings="?", stringsAsFactors=FALSE)

# all the data should be inside the dataset.
# let's filter the data we'll need and discard the old dataset.
filtered_data <- dataset[(dataset$Date=="1/2/2007" | dataset$Date=="2/2/2007"), ]
rm(dataset)

# Convert date and time to date/time
filtered_data$Date <- as.Date(filtered_data$Date, "%d/%m/%Y")
filtered_data$Date <- as.POSIXlt(paste(filtered_data$Date,filtered_data$Time) ,format="%Y-%m-%d %H:%M:%S")
filtered_data <- filtered_data[,-2]

# == Start ploting ==
hist(filtered_data$Global_active_power,
     main = "Global Active Power", 
     col="red",
     xlab="Global Active Power (kilowatts)")
dev.copy(png, 
         filename = "plot1.png", 
         width = 480, 
         height = 480)
dev.off()
