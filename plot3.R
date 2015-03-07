# Loads raw data into memory
loadData <- function() {

	# Source URL
	url <- c("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")

	# Paths
	path.dataDir <- c("./data/")
	path.rawData <- paste0(path.dataDir, "household_power_consumption.txt")
	path.destFile <- paste0(path.dataDir, "hpc.zip")

	# Download and unzip file if it doesn't already exist
	if (!file.exists(path.rawData)) {

		# create the data directory if it doesn't already exist
		dir.create(path.dataDir)

		# Download the file if necessary
		if (!file.exists(path.destFile)) {
			download.file(url, destfile = path.destFile, method = "curl")
		}

		# unzip file
		unzip(path.destFile, exdir = path.dataDir)
	}

	# set colclasses
	colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

	# read file into memory
	hpc <- read.table(path.rawData, header = TRUE, sep = ";", colClasses = colClasses, na.strings = "?")

	if (is.null(hpc)) stop("Error reading file")

	# return the raw data
	hpc
}

# Processes data from raw input to plottable data
cleanData <- function(rawData) {

	# Subset for correct time frame
	rawData <- rawData[(rawData$Date %in% c("2/2/2007","1/2/2007")), ]

	# Convert to date format
	rawData$Date <- as.Date(rawData$Date, format = "%d/%m/%Y")
	datetime <- paste(rawData$Date, rawData$Time)
	rawData$datetime <- strptime(datetime, format = "%Y-%m-%d %H:%M:%S")

	# return cleaned version of raw data
	rawData
}

# Plots data on screen
plotData <- function(processedData) {

	# Prepare plot
	yLabel <- "Energy sub metering"
	xLabel <- ""
	par(mfrow = c(1,1))

	# Make plot
	plot(processedData$datetime, processedData$Sub_metering_1, type = "n", ylab = yLabel, xlab = xLabel)
	lines(processedData$datetime, processedData$Sub_metering_1)
	lines(processedData$datetime, processedData$Sub_metering_2, col = "red")
	lines(processedData$datetime, processedData$Sub_metering_3, col = "blue")

	# Legend
	legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), pch = "_")

	# copy to png device and close the device
	dev.copy(png, "plot3.png", width = 480, height = 480, units = "px")
	dev.off()
}

# Full program
plot3 <- function() {
	raw <- loadData()

	processed <- cleanData(raw)

	plotData(processed)
}
