# Scripts to load and plot data

# Loads raw data into memory
loadData <- function() {

	# Source
	sourceFile <- c("./tmpdata/household_power_consumption.txt")

	# set colclasses
	colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

	# read file into memory
	hpc <- read.table(sourceFile, header = TRUE, sep = ";", colClasses = colClasses, na.strings = "?")

	if (is.null(hpc)) stop("Error reading file")

	# return the raw data
	hpc
}

# Processes data from raw input to plottable data
cleanData <- function(rawData) {

	# Subset for correct time frame
	rawData <- rawData[(rawData$Date %in% c("2/2/2007","1/2/2007")), ]

	# return cleaned version of raw data
	rawData
}

# Plots data on screen
plotData <- function(processedData) {

	# Prepare plot
	title <- "Global Active Power"
	xLabel <- "Global Active Power (kilowatts)"
	par(mfrow = c(1,1))

	# Make plot
	hist(processedData$Global_active_power, col = "red", main = title, xlab = xLabel)

	# copy to png device and close the device
	dev.copy(png, "plot1.png", width = 480, height = 480, units = "px")
	dev.off()
}

# Full program
plot1 <- function() {
	raw <- loadData()

	processed <- cleanData(raw)

	plotData(processed)
}