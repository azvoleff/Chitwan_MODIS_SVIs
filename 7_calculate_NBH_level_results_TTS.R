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

source('tts2df.R')
source('ttsdf2raster.R')

# Below just used to set uniform plotting options for dissertation plots
source('C:/Users/azvoleff/Code/R/Chitwan_R_files/Climate/0_utility_functions.R')

tts_file_name <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_Expanded_fit.tts'
tts_df <- tts2df(tts_file_name)

base_image_file <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'

# Load CVFS nbh data  and convert to sp dataframe
cvfs_nbhs <- readOGR('G:/Data/Nepal/GIS/CVFS_Data', 'cvfsns_with_elevations')
cvfs_nbhs <- cvfs_nbhs[cvfs_nbhs$NID <= 151, ]
cvfs_nbhs$NEIGHID <- sprintf("%03i", cvfs_nbhs$NID)
nbhs <- spTransform(cvfs_nbhs, CRS=CRS(projection(raster(base_image_file))))

# Ensure the data lines up properly:
#plot(raster(base_image_file)[[1]])
#points(nbhs)

CVFS_area_mask <- raster('G:/Data/Nepal/Imagery/MODIS/AOIs/CVFS_Study_Area_mask_float.img')

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
years <- rep(seq(2000, 2012), each=length(days))
dates <- as.Date(paste(years, days), '%Y %j')
dates <- dates[dates >= as.Date('2000 49', '%Y %j')]
dates <- dates[dates <= as.Date('2012 33', '%Y %j')]

tts_results <- data.frame(NEIGHID=rep(nbhs$NEIGHID, length(dates)),
                          Date=rep(dates, each=nrow(nbhs)))
tts_results$NEIGHID <- sprintf("%03i", tts_results$NEIGHID)
tts_results$Year <- year(tts_results$Date)
tts_results$Month <- month(tts_results$Date)
tts_results$Day <- day(tts_results$Date)

ttsraster <- ttsdf2raster(tts_df, base_image_file)
# Mask values outside CVFS study area (so rivers, Barandabar, etc. 
# are not included in EVI calculation).
ttsraster <- setValues(ttsraster, getValues(ttsraster) * 
                       getValues(CVFS_area_mask))
buffer_dists_m <- c(250, 500)
for (buffer_dist_m in buffer_dists_m) {
    # Note that buffer is in meters
    extract_vals <- data.frame(as.vector(extract(ttsraster, nbhs, 
                                                 buffer=buffer_dist_m, 
                                                 fun=mean, na.rm=TRUE)))
    names(extract_vals) <- paste('mean_EVI_', buffer_dist_m, 'm', sep='')
    tts_results <- cbind(tts_results, extract_vals)
}
save(tts_results, file='CVFS_NBHs_MODIS_tts_fit.Rdata')

EVI_daily_stats <- ddply(tts_results, .(Date), summarize,
                  mean=mean(mean_EVI_250m),
                  min=min(mean_EVI_250m),
                  max=max(mean_EVI_250m),
                  sd=sd(mean_EVI_250m))

EVI_annual_stats <- ddply(tts_results, .(Year), summarize,
                  mean=mean(mean_EVI_250m),
                  min=min(mean_EVI_250m),
                  max=max(mean_EVI_250m),
                  sd=sd(mean_EVI_250m))

# Note that there are 22 time points per year (1 every 16 days, with last on 
# day 352 of the year).
filter_size_6mth <- 22 * .5
filter_coefs_6mth <- rep(1/filter_size_6mth, filter_size_6mth)
filter_size_1yr <- 22 * 1
filter_coefs_1yr <- rep(1/filter_size_1yr, filter_size_1yr)
filter_size_2yr <- 22 * 2
filter_coefs_2yr <- rep(1/filter_size_2yr, filter_size_2yr)
# Note that sides=1 is used in below filter commands to ensure filter includes 
# past values only (otherwise the filter would by default be centered around 
# lag 0, which would mean future values would be used in the average). We don't 
# want future values in the average as these time series will be used to model 
# human decisions based on past climate.
tts_results <- ddply(tts_results, .(NEIGHID), transform,
                     mean_EVI_250m_6mth=filter(as.matrix(mean_EVI_250m),
                                              filter_coefs_6mth, sides=1),
                     mean_EVI_500m_6mth=filter(as.matrix(mean_EVI_500m),
                                              filter_coefs_6mth, sides=1),
                     mean_EVI_250m_1yr=filter(as.matrix(mean_EVI_250m),
                                              filter_coefs_1yr, sides=1),
                     mean_EVI_500m_1yr=filter(as.matrix(mean_EVI_500m),
                                              filter_coefs_1yr, sides=1),
                     mean_EVI_250m_2yr=filter(as.matrix(mean_EVI_250m),
                                              filter_coefs_2yr, sides=1),
                     mean_EVI_500m_2yr=filter(as.matrix(mean_EVI_500m),
                                              filter_coefs_2yr, sides=1))
# Now take monthly mean of the rolling mean values, as the hhreg data is 
# collected only once per month.
EVI_indicators <- ddply(tts_results, .(NEIGHID, Year, Month), summarize,
                        mean_EVI_250m=mean(mean_EVI_250m),
                        mean_EVI_500m=mean(mean_EVI_500m),
                        min_EVI_250m=min(mean_EVI_250m),
                        min_EVI_500m=min(mean_EVI_500m),
                        max_EVI_250m=max(mean_EVI_250m),
                        max_EVI_500m=max(mean_EVI_500m),
                        mean_EVI_250m_6mth=mean(mean_EVI_250m_6mth),
                        mean_EVI_500m_6mth=mean(mean_EVI_500m_6mth),
                        mean_EVI_250m_1yr=mean(mean_EVI_250m_1yr),
                        mean_EVI_500m_1yr=mean(mean_EVI_500m_1yr),
                        mean_EVI_250m_2yr=mean(mean_EVI_250m_2yr),
                        mean_EVI_500m_2yr=mean(mean_EVI_500m_2yr))
EVI_indicators$Date <- as.Date(paste(EVI_indicators$Year, 
                                     EVI_indicators$Month, 15),'%Y %m %d')
# Normalize by Jan 2000 - Dec 2001 mean (which is stored in row 24 for 
# 2002/01/15
EVI_indicators <- ddply(EVI_indicators, .(NEIGHID), transform,
                        mean_EVI_250m_6mth_norm=mean_EVI_250m_6mth/mean_EVI_250m_6mth[7],
                        mean_EVI_500m_6mth_norm=mean_EVI_500m_6mth/mean_EVI_500m_6mth[7],
                        mean_EVI_250m_1yr_norm=mean_EVI_250m_1yr/mean_EVI_250m_1yr[13],
                        mean_EVI_500m_1yr_norm=mean_EVI_500m_1yr/mean_EVI_500m_1yr[13],
                        mean_EVI_250m_2yr_norm=mean_EVI_250m_2yr/mean_EVI_250m_2yr[24],
                        mean_EVI_500m_2yr_norm=mean_EVI_500m_2yr/mean_EVI_500m_2yr[24])
save(EVI_indicators, file='EVI_indicators.Rdata')

# Find any neighborhoods with zero mean EVIs
#table(EVI_indicators[EVI_indicators$mean_EVI_250m_2yr == 0 & 
#      !is.na(EVI_indicators$mean_EVI_250m_2yr),]$NEIGHID)
#table(EVI_indicators[EVI_indicators$mean_EVI_500m_2yr == 0 & 
#      !is.na(EVI_indicators$mean_EVI_500m_2yr),]$NEIGHID)

qplot(Date, mean_EVI_250m, geom='line', colour=NEIGHID, data=EVI_indicators)

qplot(Date, mean_EVI_250m_6mth, geom='line', colour=NEIGHID, data=EVI_indicators)

qplot(Date, mean_EVI_250m_1yr, geom='line', colour=NEIGHID, data=EVI_indicators)

qplot(Date, mean_EVI_250m_2yr, geom='line', colour=NEIGHID, data=EVI_indicators)

qplot(Date, mean_EVI_250m_6mth_norm, geom='line', colour=NEIGHID, data=EVI_indicators)

qplot(Date, mean_EVI_250m_1yr_norm, geom='line', colour=NEIGHID, data=EVI_indicators)

qplot(Date, mean_EVI_250m_2yr_norm, geom='line', colour=NEIGHID, data=EVI_indicators)

###############################################################################
# EVI indicator plots
#
# Make 6 month plot
mean_EVI_250m_6mth_plot <- ggplot(EVI_indicators, aes(Date, mean_EVI_250m_6mth)) + 
       geom_line(aes(colour=NEIGHID)) + guides(colour=FALSE) +
       ylab('6-month mean EVI') + xlab('Date') +
       xlim(c(as.Date('2002/01/01'), as.Date('2012/01/01'))) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=260, label="CVFS Registry Data"), size=8) +
       #geom_text(aes(x=as.Date("2004/06/15"), y=250, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('mean_EVI_250m_6mth.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(mean_EVI_250m_6mth_plot)
dev.off()

# Make 1 year plot
mean_EVI_250m_1yr_plot <- ggplot(EVI_indicators, aes(Date, mean_EVI_250m_1yr)) + 
       geom_line(aes(colour=NEIGHID)) + guides(colour=FALSE) +
       ylab('12-month mean EVI') + xlab('Date') +
       xlim(c(as.Date('2002/01/01'), as.Date('2012/01/01'))) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2) +
       #geom_text(aes(x=as.Date("2004/06/15"), y=202, label="CVFS Registry Data"), size=8)
       #geom_text(aes(x=as.Date("2004/06/15"), y=195, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('mean_EVI_250m_1yr.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(mean_EVI_250m_1yr_plot)
dev.off()

# Make 2 year plot
mean_EVI_250m_2yr_plot <- ggplot(EVI_indicators, aes(Date, mean_EVI_250m_2yr)) + 
       geom_line(aes(colour=NEIGHID)) + guides(colour=FALSE) +
       ylab('24-month mean EVI') + xlab('Date') +
       xlim(c(as.Date('2002/01/01'), as.Date('2012/01/01'))) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=180, label="CVFS Registry Data"), size=8) +
       #geom_text(aes(x=as.Date("2004/06/15"), y=175, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('mean_EVI_250m_2yr.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(mean_EVI_250m_2yr_plot)
dev.off()

###############################################################################
# Normed plots
# Make 6 month norm plot
mean_EVI_250m_6mth_norm_plot <- ggplot(EVI_indicators, aes(Date, mean_EVI_250m_6mth_norm*100)) + 
       geom_line(aes(colour=NEIGHID)) + guides(colour=FALSE) +
       ylab('Percent of 2000-2001 mean EVI') + xlab('Date') +
       xlim(c(as.Date('2002/01/01'), as.Date('2012/01/01'))) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2) +
       geom_text(aes(x=as.Date("2004/06/15"), y=260, label="CVFS Registry Data"), size=8) +
       geom_text(aes(x=as.Date("2004/06/15"), y=250, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('mean_EVI_250m_6mth_norm.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(mean_EVI_250m_6mth_norm_plot)
dev.off()

# Make 1 year norm plot
mean_EVI_250m_1yr_norm_plot <- ggplot(EVI_indicators, aes(Date, mean_EVI_250m_1yr_norm*100)) + 
       geom_line(aes(colour=NEIGHID)) + guides(colour=FALSE) +
       ylab('Percent of 2000-2001 mean EVI') + xlab('Date') +
       xlim(c(as.Date('2002/01/01'), as.Date('2012/01/01'))) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2) +
       geom_text(aes(x=as.Date("2004/06/15"), y=202, label="CVFS Registry Data"), size=8) +
       geom_text(aes(x=as.Date("2004/06/15"), y=195, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('mean_EVI_250m_1yr_norm.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(mean_EVI_250m_1yr_norm_plot)
dev.off()

# Make 2 year norm plot
mean_EVI_250m_2yr_norm_plot <- ggplot(EVI_indicators, aes(Date, mean_EVI_250m_2yr_norm*100)) + 
       geom_line(aes(colour=NEIGHID)) + guides(colour=FALSE) +
       ylab('Percent of 2000-2001 mean EVI') + xlab('Date') +
       xlim(c(as.Date('2002/01/01'), as.Date('2012/01/01'))) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2) +
       geom_text(aes(x=as.Date("2004/06/15"), y=180, label="CVFS Registry Data"), size=8) +
       geom_text(aes(x=as.Date("2004/06/15"), y=175, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('mean_EVI_250m_2yr_norm.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(mean_EVI_250m_2yr_norm_plot)
dev.off()

EVI_daily_stats$mean_2yr_roll <- filter(EVI_daily_stats$mean, filter_coefs)

ggplot(EVI_daily_stats, aes(Date, mean)) + geom_line() + xlab('Time') +
    ylab('EVI mean (16 day)')

ggplot(EVI_daily_stats, aes(Date, mean_2yr_roll)) + geom_line() + xlab('Time') +
    ylab('2-year rolling mean EVI')
