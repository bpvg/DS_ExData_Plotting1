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
# I got weird results when using dev.copy(), so after setting all up on screen I
# changed the output method to redirect to png. It looks it is related with 
# rescaling
png (filename = "plot4.png", width = 480, height = 480)

#Change Plot to 2x2 format
par(mfrow=c(2,2))

# = PLOT1 =
plot(filtered_data$Date,  # X
     filtered_data$Global_active_power,  # Y
     type = "l", #line
     main = "", 
     col  = "black",
     xlab = "",
     ylab = "Global Active Power")

# = PLOT2 =
plot(filtered_data$Date,  # X
     filtered_data$Voltage,  # Y
     type = "l", #line
     main = "", 
     col  = "black",
     xlab = "datetime",
     ylab = "Voltage")

# = PLOT3 =
# I'll start adding a first plot for sub1, and then 2 different sub's via points
plot(filtered_data$Date,  # X
     filtered_data$Sub_metering_1,
     type = "l", #none. 
     main = "", 
     col  = "black",
     xlab = "",
     ylab = "Energy Sub Metering")
# Sub2
points(filtered_data$Date,
       filtered_data$Sub_metering_2,
       type = "l",
       col  = "red"
       )
# Sub3
points(filtered_data$Date,
       filtered_data$Sub_metering_3,
       type = "l",
       col  = "blue"
)
# Now I'll add the legend.
legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col    = c("black",          "red",            "blue"),
       pch    = c("-",              "-",              "-"),
       lwd    = c(2,                2,                2)
       )

# = PLOT4 =
plot(filtered_data$Date,  # X
     filtered_data$Global_reactive_power,  # Y
     type = "l", #line
     main = "", 
     col  = "black",
     xlab = "datetime",
     ylab = "Global_reactive_power")

# Close dev and clean data
dev.off()

