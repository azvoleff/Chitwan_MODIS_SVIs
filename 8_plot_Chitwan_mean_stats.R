library(foreign)
library(sp)
library(maptools)
library(ggplot2)
library(lubridate)
library(raster)
library(rgdal)
library(plyr)

source('tts2df.R')
source('ttsdf2raster.R')

tts_file_name <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_fit.tts'
tts_df <- tts2df(tts_file_name)

base_image_file <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'

CVFS_area_mask <- raster('R:/Data/Nepal/Imagery/MODIS/AOIs/CVFS_Study_Area_mask_float.img')

# Below is for debug only
#projection(CVFS_area_mask) <- projection(raster(base_image_file))
#extent(CVFS_area_mask) <- extent(raster(base_image_file))
#res(CVFS_area_mask) <- res(raster(base_image_file))
#writeRaster(raster(base_image_file), 'D:/Workspace/TEMP/base_image.envi', 
#            overwrite=TRUE)
#writeRaster(CVFS_area_mask, 'D:/Workspace/TEMP/CVFS_area_mask.envi', 
#            overwrite=TRUE)
#writeOGR(nbhs, 'D:/Workspace/TEMP', 'CVFS_NBHs', driver="ESRI Shapefile")

days <- seq(1, 365, 16)
years <- rep(seq(2000, 2011), each=length(days))
dates <- as.Date(paste(years, days), '%Y %j')
dates <- dates[dates >= as.Date('2000 01', '%Y %j')]
dates <- dates[dates <= as.Date('2011 353', '%Y %j')]

tts_results <- data.frame(Date=dates)
tts_results$Year <- year(tts_results$Date)
tts_results$Month <- month(tts_results$Date)
tts_results$Day <- day(tts_results$Date)

ttsraster <- ttsdf2raster(tts_df, base_image_file)
# Mask values outside CVFS study area (so rivers, Barandabar, etc. 
# are not included in EVI calculation).
ttsraster <- setValues(ttsraster, getValues(ttsraster) * 
                       getValues(CVFS_area_mask))
tts_results <- cbind(tts_results, mean_EVI=cellStats(ttsraster, stat='mean'))

ggplot(tts_results, aes(Date, mean_EVI)) + geom_line()

EVI_annual_stats <- ddply(tts_results, .(Year), summarize,
                  mean=mean(mean_EVI),
                  min=min(mean_EVI),
                  max=max(mean_EVI),
                  sd=sd(mean_EVI))

filter_years <- 2
# Note that there are 22 time points per year (1 every 16 days, with last on 
# day 352 of the year).
filter_size <- 22*filter_years
filter_coefs <- rep(1/filter_size, filter_size)
# Note that sides=1 is used in below filter commands to ensure filter includes 
# past values only (otherwise the filter would by default be centered around 
# lag 0, which would mean future values would be used in the average). We don't 
# want future values in the average as these time series will be used to model 
# human decisions based on past climate.
tts_results$mean_EVI_2yr <- filter(tts_results$mean_EVI, filter_coefs, sides=1)
# Now take monthly mean of the rolling mean values, as the hhreg data is 
# collected only once per month.
EVI_monthly <- ddply(tts_results, .(Year, Month), summarize,
                        mean_EVI=mean(mean_EVI),
                        min_EVI=min(mean_EVI),
                        max_EVI=max(mean_EVI),
                        mean_EVI_2yr=mean(mean_EVI_2yr),
                        min_EVI_2yr=min(mean_EVI_2yr),
                        max_EVI_2yr=max(mean_EVI_2yr))
EVI_monthly$Date <- as.Date(paste(EVI_monthly$Year, 
                                  EVI_monthly$Month, 15),'%Y %m %d')

# Normalize by Jan 2000 - Dec 2001 mean (which is stored in row 24 for 
# 2002/01/15
EVI_monthly$mean_EVI_2yr_norm <- EVI_monthly$mean_EVI_2yr/EVI_monthly$mean_EVI_2yr[24]

save(EVI_monthly, file='EVI_monthly_mean_over_valley.Rdata')

qplot(Date, mean_EVI, geom='line', data=EVI_monthly)

qplot(Date, mean_EVI_2yr, geom='line', data=EVI_monthly)

qplot(Date, mean_EVI_2yr_norm, geom='line', data=EVI_monthly)

ggplot(EVI_monthly, aes(Date, mean_EVI_2yr_norm*100)) + 
       geom_line() + 
       ylab('Percent of 2000-2001 mean EVI') + xlab('Date') +
       xlim(c(as.Date('2002/01/01'), as.Date('2012/01/01'))) +
       geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
                     ymin=75, ymax=200), alpha=.002) +
       geom_text(aes(x=as.Date("2004/06/15"), y=180, label="CVFS Registry Data"))

EVI_daily_stats$mean_2yr_roll <- filter(EVI_daily_stats$mean, filter_coefs)

ggplot(EVI_daily_stats, aes(Date, mean)) + geom_line() + xlab('Time') +
    ylab('EVI mean (16 day)')

ggplot(EVI_daily_stats, aes(Date, mean_2yr_roll)) + geom_line() + xlab('Time') +
    ylab('2-year rolling mean EVI')
