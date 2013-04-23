library(ggplot2)
library(raster)
library(date)
library(pracma) # for 'trapz'

filtered_data_path <- 'D:/Workspace/TEMP/EVI_stack_filtered.dat'
filt_data <- brick(filtered_data_path)[]

image_dates <- read.csv('image_dates_EVI.csv')

image_dates$date <- as.Date(mdy.date(1, 1, image_dates$YEAR)+image_dates$JULIAN_DAY - 1)

mean_EVI <- apply(filt_data, 2, mean, na.rm=TRUE)

trapz(as.numeric(image_dates$date)/365, mean_EVI)

qplot(image_dates$date, mean_EVI, geom='line')
