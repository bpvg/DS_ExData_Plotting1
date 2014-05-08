# === ExData -> Project Ploting 1 ===
#
# Estimated memory consumption for the data to be loaded:
# 2,750,000 rows * (7 colums * 8 bytes num + 18 bytes text) = 146Mb aprox.
# That's fine for my laptop.
#
# [xxx@localhost ~]$ free -tm
#              total       used       free     shared    buffers     cached
# Mem:          3824       1651       2172          0         82        663
# -/+ buffers/cache:        906       2918
# Swap:         3992          1       3990
# Total:        7817       1653       6163


# Main Function to execute full scrip
RunPlot.4 <- function(){
    
    # == Initialize 'constants' ==
    myfile <- "exdata_data_household_power_consumption.zip"    
    mydatafile <- "household_power_consumption.txt"
    
    
    # == Checking for datafile availability ==
    if (!file.exists(myfile)){
        #file is not present in the working directory. Let's download it!
        method <-"auto"  # Shall be fine for M$ Windows
        if (.Platform$OS.type=="unix") method <- "curl"  # Use for unix-like systems
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                      myfile,
                      method, 
                      TRUE,
                      "wb")
        if (!file.exists(myfile)) stop("download failed.")    
    }
    
    # == Loading data ==
    # At this point the data file should be present at the wd if we need to read
    # it. I'll store the final data.frame in the Global Environment so before doing
    # anything else I'll check if it exists from a previous run on any of the
    # assignment's scrips.
    
    # Variable Checking to decide if we need to load the data again.
    if (!exists("GLB_filtered_data", envir=.GlobalEnv)) {
        # To avoid uncompressing data, I'll read it directlty from inside the Zip.
        myconn <- unz(myfile, mydatafile)
        dataset <- read.table(myconn, TRUE, ";", na.strings="?", stringsAsFactors=FALSE)
        
        # All the data should now be inside the dataset.
        # Let's filter the days we'll need, store the data and discard the old dataset.
        assign("GLB_filtered_data", 
               dataset[(dataset$Date=="1/2/2007" | dataset$Date=="2/2/2007"), ],
               envir=.GlobalEnv)
        rm(dataset) # No longer needed.
        
        # Convert date and time columns to date+time
        GLB_filtered_data$Date <- as.POSIXlt(paste(GLB_filtered_data$Date,
                                                   GLB_filtered_data$Time),
                                             format="%d/%m/%Y %H:%M:%S")
        GLB_filtered_data <- GLB_filtered_data[,-2] #remove time
    }
    
    
    # == Start ploting ==
    # I got weird results when using dev.copy(), so after setting all up on screen I
    # changed the output method to redirect to png.
    png (filename = "plot4.png", width = 480, height = 480, bg="transparent")

    #Change Plot to 2x2 format
    par(mfrow=c(2,2))
    
    # = PLOT1 =
    plot(GLB_filtered_data$Date,  # X
         GLB_filtered_data$Global_active_power,  # Y
         type = "l", #line
         main = "", 
         col  = "black",
         xlab = "",
         ylab = "Global Active Power")
    
    # = PLOT2 =
    plot(GLB_filtered_data$Date,  # X
         GLB_filtered_data$Voltage,  # Y
         type = "l", #line
         main = "", 
         col  = "black",
         xlab = "datetime",
         ylab = "Voltage")
    
    # = PLOT3 =
    # I'll start adding a first plot for sub1, and then 2 different sub's via points
    plot(GLB_filtered_data$Date,  # X
         GLB_filtered_data$Sub_metering_1,
         type = "l", #none. 
         main = "", 
         col  = "black",
         xlab = "",
         ylab = "Energy Sub Metering")
    # Sub2
    points(GLB_filtered_data$Date,
           GLB_filtered_data$Sub_metering_2,
           type = "l",
           col  = "red"
    )
    # Sub3
    points(GLB_filtered_data$Date,
           GLB_filtered_data$Sub_metering_3,
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
    plot(GLB_filtered_data$Date,  # X
         GLB_filtered_data$Global_reactive_power,  # Y
         type = "l", #line
         main = "", 
         col  = "black",
         xlab = "datetime",
         ylab = "Global_reactive_power")
    
    # Close dev
    dev.off()
}
