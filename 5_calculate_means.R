library(foreign)
library(ggplot2)
library(raster)
library(date)
library(pracma) # for 'trapz'

filtered_data_path <- 'D:/Workspace/TEMP/EVI_stack_filtered.dat'
filt_data <- brick(filtered_data_path)[]
original_data_path <- 'D:/Workspace/TEMP/EVI_stack.dat'
orig_data <- brick(original_data_path)[]

image_dates <- read.csv('image_dates_EVI.csv')
image_dates$date <- as.Date(mdy.date(1, 1, image_dates$YEAR)+image_dates$JULIAN_DAY - 1)

filt_mean_EVI <- apply(filt_data, 2, mean, na.rm=TRUE)
orig_mean_EVI <- apply(orig_data, 2, mean, na.rm=TRUE)
stacked <- data.frame(EVI=c(filt_mean_EVI, orig_mean_EVI),
                      Series=factor(rep(c("filtered","original"),
                                         each=nrow(image_dates))),
                      Date=rep(image_dates$date, 2))

#trapz(as.numeric(image_dates$date)/365, mean_EVI)

qplot(Date, EVI, geom='line', colour=Series, 
      linetype=Series, data=stacked)

###############################################################################
# Calculate neighborhood-level EVI timeseries

# First load CVFS nbh data sp dataframe
nbhhist <- read.xport("W:/Nepal/ICPSR_0538_Restricted/da04538-0014_REST.xpt")
ID_cols <- grep('^(NEIGHID|STRATA|NX|NY)$', names(nbhhist))
nbhs <- nbhhist[ID_cols]

# Make a n-NBH by n-MODIS timepts dataframe to store the results

# Loop over MODIS timepoints

# Use 'focal' to extract the points in a buffer around each NBH, taking the 
# mean. Store the results in the results data frame

