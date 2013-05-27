library(foreign)
library(sp)
library(maptools)
library(ggplot2)
library(lubridate)
library(raster)
library(rgdal)
library(plyr)

###############################################################################
# Calculate neighborhood-level EVI timeseries

source('tpa2df.R')
source('tpadf2raster.R')

tpa_file_name <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_Expanded_TS.tpa'
tpa_df <- tpa2df(tpa_file_name, 11)

base_image_file <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'

# First load CVFS nbh data  and convert to sp dataframe
cvfs_nbhs <- readOGR('G:/Data/Nepal/GIS/CVFS_Data', 'cvfsns_with_elevations')
cvfs_nbhs <- cvfs_nbhs[cvfs_nbhs$NID <= 151, ]
cvfs_nbhs$NEIGHID <- sprintf("%03i", cvfs_nbhs$NID)
nbhs <- spTransform(cvfs_nbhs, CRS=CRS(projection(raster(base_image_file))))

# Ensure the data lines up properly:
#plot(raster(base_image_file)[[1]])
#points(nbhs)

CVFS_area_mask <- raster('G:/Data/Nepal/Imagery/MODIS/AOIs/CVFS_Study_Area_mask.img')

# Add fields converting the start, end, and peak times in the dataframe into 
# Julian days, and convert the length field into actual days. Note that %j 
# means 001-366 day of the year.
start_date <- as.Date('2000001', format='%Y%j')
# Multiply by 16 since each period represents 16 days
tpa_df$start_julian <- as.numeric(format(start_date + tpa_df$start * 
                                         (365.25/23), '%j'))
tpa_df$end_julian <- as.numeric(format(start_date + tpa_df$end * 
                                         (365.25/23), '%j'))
tpa_df$peak_time_julian <- as.numeric(format(start_date + tpa_df$peak_time 
                                             * (365.25/23), '%j'))
tpa_df$length_days <- as.numeric(tpa_df$length * (365.25/23))

hist(tpa_df$start_julian)
hist(tpa_df$end_julian)
hist(tpa_df$peak_time_julian)
hist(tpa_df$length_days)

table(tpa_df$length_days > 365)

# Dates is a vector of season start dates (the year of the season start date 
# only).
dates <- year(seq(as.Date('2000/01/01'), as.Date('2010/01/01'), by='year'))
tpa_results <- data.frame(NEIGHID=rep(nbhs$NEIGHID, length(dates)),
                          Year=rep(dates, each=nrow(nbhs)))
tpa_results$NEIGHID <- sprintf("%03i", tpa_results$NEIGHID)

buffer_dists_m <- c(250, 500)
variables <- c('start_julian', 'end_julian', 'length_days', 'base_value', 
               'peak_time_julian', 'peak_value', 'amp')
for (variable in variables) {
    for (buffer_dist_m in buffer_dists_m) {
        tparaster <- tpadf2raster(tpa_df, base_image_file, variable)
        # Mask values outside CVFS study area (so rivers, Barandabar, etc. 
        # are not included in EVI calculation).
        tparaster <- setValues(tparaster, getValues(tparaster) * 
                               getValues(CVFS_area_mask))
        # Note that buffer is in meters
        extract_vals <- data.frame(as.vector(extract(tparaster, nbhs, 
                                                     buffer=buffer_dist_m, 
                                                     fun=mean)))
        names(extract_vals) <- paste(variable, '_', buffer_dist_m, 'm', sep='')
        tpa_results <- cbind(tpa_results, extract_vals)
    }
}
qplot(Year, start_julian_250m, geom='line', colour=NEIGHID, data=tpa_results) + guides(colour=FALSE)

qplot(Year, end_julian_250m, geom='line', colour=NEIGHID, data=tpa_results) + guides(colour=FALSE)

qplot(Year, peak_value_250m, geom='line', colour=NEIGHID, data=tpa_results) + guides(colour=FALSE)

qplot(Year, peak_value_500m, geom='line', colour=NEIGHID, data=tpa_results) + guides(colour=FALSE)

qplot(Year, length_days_500m, geom='line', colour=NEIGHID, data=tpa_results) + guides(colour=FALSE)

qplot(Year, amp_250m, geom='line', colour=NEIGHID, data=tpa_results) + guides(colour=FALSE)

qplot(Year, amp_500m, geom='line', colour=NEIGHID, data=tpa_results) + guides(colour=FALSE)

filter_years <- 2
filter_size <- 1*filter_years
filter_coefs <- rep(1/filter_size, filter_size)
tpa_results <- ddply(tpa_results, .(NEIGHID), transform,
                     mean_amp_250m_2yr=filter(as.matrix(amp_250m), 
                                              filter_coefs, sides=1),
                     mean_amp_500m_2yr=filter(as.matrix(amp_500m), 
                                              filter_coefs, sides=1))

save(tpa_results, file='CVFS_NBHs_MODIS_tpa.Rdata')

seasonal_summary <- ddply(tpa_results, .(Year), summarize,
                     mean_amp_250m=mean(amp_250m, na.rm=TRUE),
                     mean_amp_500m=mean(amp_500m, na.rm=TRUE),
                     mean_amp_250m_2yr=mean(mean_amp_250m_2yr),
                     mean_amp_250m_2yr=mean(mean_amp_250m_2yr))

qplot(Year, mean_amp_250m, data=seasonal_summary, geom='line')

qplot(Year, mean_amp_250m_2yr, data=seasonal_summary, geom='line')
