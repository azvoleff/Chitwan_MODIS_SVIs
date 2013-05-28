library(foreign)
library(sp)
library(maptools)
library(ggplot2)
library(lubridate)
library(raster)
library(rgdal)
library(plyr)
library(reshape2)

source('tts2df.R')
source('ttsdf2raster.R')

# Below just used to set uniform plotting options for dissertation plots
source('C:/Users/azvoleff/Code/R/Chitwan_R_files/Climate/0_utility_functions.R')

tts_file_name <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_fit.tts'
tts_df <- tts2df(tts_file_name)

base_image_file <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'

CVFS_area_mask <- raster('R:/Data/Nepal/Imagery/MODIS/AOIs/CVFS_Study_Area_mask_float.img')
projection(CVFS_area_mask) <- projection(raster(base_image_file))
extent(CVFS_area_mask) <- extent(raster(base_image_file))
res(CVFS_area_mask) <- res(raster(base_image_file))

# Below is for debug only
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
ttsraster <- mask(ttsraster, CVFS_area_mask, maskvalue=0)
tts_results <- cbind(tts_results, mean_EVI=cellStats(ttsraster, stat='mean'))

# Note that there are 23 time points per year (1 every 16 days, with last on 
# day 353 of the year).
filter_size_3mth <- round(23*.25)
filter_size_6mth <- round(23*.5)
filter_size_9mth <- round(23*.75)
filter_size_12mth<- 23*1
filter_size_24mth<- 23*2
filter_coefs_3mth <- rep(1/filter_size_3mth, filter_size_3mth)
filter_coefs_6mth <- rep(1/filter_size_6mth, filter_size_6mth)
filter_coefs_9mth <- rep(1/filter_size_9mth, filter_size_9mth)
filter_coefs_12mth <- rep(1/filter_size_12mth, filter_size_12mth)
filter_coefs_24mth <- rep(1/filter_size_24mth, filter_size_24mth)
# Note that sides=1 is used in below filter commands to ensure filter includes 
# past values only (otherwise the filter would by default be centered around 
# lag 0, which would mean future values would be used in the average). We don't 
# want future values in the average as these time series will be used to model 
# human decisions based on past climate.
tts_results$mean_EVI_3mth <- filter(tts_results$mean_EVI, filter_coefs_3mth, sides=1)
tts_results$mean_EVI_6mth <- filter(tts_results$mean_EVI, filter_coefs_6mth, sides=1)
tts_results$mean_EVI_9mth <- filter(tts_results$mean_EVI, filter_coefs_9mth, sides=1)
tts_results$mean_EVI_12mth <- filter(tts_results$mean_EVI, filter_coefs_12mth, sides=1)
tts_results$mean_EVI_24mth <- filter(tts_results$mean_EVI, filter_coefs_24mth, sides=1)
# Now take monthly mean of the rolling mean values, as the hhreg data is 
# collected only once per month.
EVI_monthly <- ddply(tts_results, .(Year, Month), summarize,
                     mean_EVI=mean(mean_EVI),
                     mean_EVI_3mth=mean(mean_EVI_3mth),
                     mean_EVI_6mth=mean(mean_EVI_6mth),
                     mean_EVI_9mth=mean(mean_EVI_9mth),
                     mean_EVI_12mth=mean(mean_EVI_12mth),
                     mean_EVI_24mth=mean(mean_EVI_24mth))
EVI_monthly$Date <- as.Date(paste(EVI_monthly$Year, 
                                  EVI_monthly$Month, 15),'%Y %m %d')

# Normalize by Jan 2000 - Dec 2001 mean (which is stored in row 24 for 
# 2002/01/15
EVI_monthly$mean_EVI_3mth_norm <- EVI_monthly$mean_EVI_3mth/EVI_monthly$mean_EVI_3mth[4]
EVI_monthly$mean_EVI_6mth_norm <- EVI_monthly$mean_EVI_6mth/EVI_monthly$mean_EVI_6mth[7]
EVI_monthly$mean_EVI_9mth_norm <- EVI_monthly$mean_EVI_9mth/EVI_monthly$mean_EVI_9mth[9]
EVI_monthly$mean_EVI_12mth_norm <- EVI_monthly$mean_EVI_12mth/EVI_monthly$mean_EVI_12mth[13]
EVI_monthly$mean_EVI_24mth_norm <- EVI_monthly$mean_EVI_24mth/EVI_monthly$mean_EVI_24mth[25]

save(EVI_monthly, file='EVI_monthly_mean_over_valley.Rdata')

qplot(Date, mean_EVI, geom='line', data=EVI_monthly)

qplot(Date, mean_EVI_3mth, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_6mth, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_9mth, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_12mth, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_24mth, geom='line', data=EVI_monthly)

qplot(Date, mean_EVI_3mth_norm, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_6mth_norm, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_9mth_norm, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_12mth_norm, geom='line', data=EVI_monthly)
qplot(Date, mean_EVI_24mth_norm, geom='line', data=EVI_monthly)

EVI_monthly_plot <- ggplot(EVI_monthly, aes(Date, mean_EVI_6mth)) + 
       geom_line() + 
       ylab('6 month mean EVI') + xlab('Date') +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=1.1, label="CVFS Registry Data")) +
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('EVI_monthly_6mth.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(EVI_monthly_plot)
dev.off()

EVI_monthly_plot <- ggplot(EVI_monthly, aes(Date, mean_EVI_12mth)) + 
       geom_line() + 
       ylab('12 month mean EVI') + xlab('Date') +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=1.1, label="CVFS Registry Data")) +
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('EVI_monthly_12mth.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(EVI_monthly_plot)
dev.off()

EVI_monthly_plot <- ggplot(EVI_monthly, aes(Date, mean_EVI_24mth)) + 
       geom_line() + 
       ylab('24 month mean EVI') + xlab('Date') +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=1.1, label="CVFS Registry Data")) +
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('EVI_monthly_24mth.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(EVI_monthly_plot)
dev.off()

###############################################################################
# Now plot seasonal mean
###############################################################################
source('tpa2df.R')
source('tpadf2raster.R')

tpa_file_name <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_Expanded_TS.tpa'
tpa_df <- tpa2df(tpa_file_name, 11)
# Add fields converting the start, end, and peak times in the dataframe into 
# Julian days, and convert the length field into actual days. Note that %j 
# means 001-366 day of the year.
start_date <- as.Date('2000 001', format='%Y %j')
# Multiply by 16 since each period represents 16 days
tpa_df$start_date <- start_date + tpa_df$start * (365.25/23)
tpa_df$start_julian <- as.numeric(format(tpa_df$start_date, '%j'))
tpa_df$end_date <- start_date + tpa_df$end * (365.25/23)
tpa_df$end_julian <- as.numeric(format(tpa_df$end_date, '%j'))
tpa_df$peak_date <- start_date + tpa_df$peak_time * (365.25/23)
tpa_df$peak_julian <- as.numeric(format(tpa_df$peak_date, '%j'))
tpa_df$length_days <- as.numeric(tpa_df$length * (365.25/23))

valley_means <- data.frame(Year=seq(as.Date('2000/01/01'), as.Date('2010/01/01'), by='year'))
get_masked_mean_tpa_variable <- function(variable) {
    tparaster <- tpadf2raster(tpa_df, base_image_file, variable)
    # Mask values outside CVFS study area (so rivers, Barandabar, etc.  are not 
    # included in EVI calculation).
    tparaster <- mask(tparaster, CVFS_area_mask, maskvalue=0)
    return(tparaster)
}
# Note that the mean end date is not too useful since the distribution is 
# bimodal (with end dates at the end and beginning of the year since the season 
# stretches through December and into January
valley_means <- cbind(valley_means,
                      amp=cellStats(get_masked_mean_tpa_variable('amp'), 
                                         stat='mean', na.rm=TRUE),
                      start_julian=cellStats(get_masked_mean_tpa_variable('start_julian'), 
                                                  stat='mean', na.rm=TRUE),
                      peak_julian=cellStats(get_masked_mean_tpa_variable('peak_julian'), 
                                            stat='mean', na.rm=TRUE),
                      length_days=cellStats(get_masked_mean_tpa_variable('length_days'), 
                                            stat='mean', na.rm=TRUE))
valley_means$start_date <- as.Date(paste(9999, valley_means$start_julian), '%Y %j')
valley_means$peak_date <- as.Date(paste(9999, valley_means$peak_julian), '%Y %j')
valley_means$start_date_string <- as.character(valley_means$start_date)
valley_means$peak_date_string <- as.character(valley_means$peak_date)

valley_means_amp_plot <- ggplot(valley_means, aes(Year, amp)) + 
       geom_line() + guides(colour=FALSE) + geom_point(size=4) +
       ylab('Mean amplitude') + xlab('Date') +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=260, label="CVFS Registry Data"), size=8) +
       #geom_text(aes(x=as.Date("2004/06/15"), y=250, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('valley_means_amp_plot.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(valley_means_amp_plot)
dev.off()

valley_means_start_plot <- ggplot(valley_means, aes(Year, start_date)) + 
       geom_line() + guides(colour=FALSE) + geom_point(size=4) +
       ylab('Mean start date') + xlab('Year') + scale_y_date() +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=260, label="CVFS Registry Data"), size=8) +
       #geom_text(aes(x=as.Date("2004/06/15"), y=250, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('valley_means_start_plot.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(valley_means_start_plot)
dev.off()

valley_means_peak_plot <- ggplot(valley_means, aes(Year, peak_date)) + 
       geom_line() + guides(colour=FALSE) + geom_point(size=4) +
       ylab('Mean peak date') + xlab('Year') + scale_y_date() +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=260, label="CVFS Registry Data"), size=8) +
       #geom_text(aes(x=as.Date("2004/06/15"), y=250, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('valley_means_peak_plot.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(valley_means_peak_plot)
dev.off()

valley_means_length_plot <- ggplot(valley_means, aes(Year, length_days)) +
       geom_line() + guides(colour=FALSE) + geom_point(size=4) +
       ylab('Mean season length (days)') + xlab('Year') +
       geom_vline(aes(xintercept=as.numeric(as.Date('2002/01/01'))), linetype=2) +
       geom_vline(aes(xintercept=as.numeric(as.Date('2007/01/01'))), linetype=2)
       #geom_text(aes(x=as.Date("2004/06/15"), y=260, label="CVFS Registry Data"), size=8) +
       #geom_text(aes(x=as.Date("2004/06/15"), y=250, label="(2002-2007)"), size=8)
       #geom_rect(aes(xmin=as.Date('2002/01/01'), xmax=as.Date('2007/01/01'), 
       #              ymin=75, ymax=200), alpha=.002) +
png('valley_means_length_plot.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(valley_means_length_plot)
dev.off()

# Note that melt doesn't play nice with date objects, so use their string 
# equivalents, then convert back to dates
valley_means_dates_melt <- melt(valley_means, id.vars='Year', measure.vars=c('start_date_string', 'peak_date_string'))
valley_means_dates_melt$value <- as.Date(valley_means_dates_melt$value)
valley_means_dates_melt$variable <- as.character(valley_means_dates_melt$variable)
valley_means_dates_melt$variable[valley_means_dates_melt$variable == 'start_date_string'] <- 'Start date'
valley_means_dates_melt$variable[valley_means_dates_melt$variable == 'peak_date_string'] <- 'Peak date'
valley_means_dates_melt$variable <- factor(valley_means_dates_melt$variable, levels=c('Start date',
                                                                                      'Peak date'))
valley_means_dates_melt_plot <- ggplot(valley_means_dates_melt, aes(Year, value)) +
       geom_line() + guides(colour=FALSE) + facet_grid(variable~., scales='free') +
       ylab('Date') + xlab('Year') + geom_point(size=4)
png('valley_means_dates_melt.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(valley_means_dates_melt_plot)
dev.off()

###############################################################################
# Plot compared to precip time series derived monsoon onset dates

load('C:/Users/azvoleff/Code/R/Chitwan_R_files/Climate/precip_monsoon_onset_date.Rdata')
onset_date$Year <- as.Date(paste(onset_date$Year, 1, 1), '%Y %m %d')
onset_date$monsoon_start <- as.Date(paste(9999, onset_date$pentad*5), '%Y %j')
onset_date$monsoon_start_string <- as.character(onset_date$monsoon_start)
onset_date$monsoon_start_julian <- as.numeric(format(onset_date$monsoon_start, '%j'))

valley_means_onset <- merge(valley_means, onset_date[onset_date$Station == 'Rampur', ], all.x=TRUE)

# Note that melt doesn't play nice with date objects, so use their string 
# equivalents, then convert back to dates
onset_date_melt <- melt(valley_means_onset, id.vars='Year', measure.vars=c('start_date_string', 'monsoon_start_string'))
onset_date_melt$value <- as.Date(onset_date_melt$value)
onset_date_melt$variable <- as.character(onset_date_melt$variable)
onset_date_melt$variable[onset_date_melt$variable == 'start_date_string'] <- 'EVI season start date'
onset_date_melt$variable[onset_date_melt$variable == 'monsoon_start_string'] <- 'Monsoon onset date'
onset_date_melt$variable <- factor(onset_date_melt$variable, levels=c('EVI season start date',
                                                                      'Monsoon onset date'))
valley_means_onset_vs_monsoon_melt_plot <- ggplot(onset_date_melt, aes(Year, value)) +
       geom_line(aes(colour=variable, linetype=variable), size=1) + 
       guides(colour=guide_legend(title='Legend'), 
              linetype=guide_legend(title='Legend')) +
       theme(legend.position='bottom') +
       ylab('Date') + xlab('Year') + geom_point(size=4)
png('valley_means_onset_vs_monsoon_melt.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(valley_means_onset_vs_monsoon_melt_plot)
dev.off()

# Note that melt doesn't play nice with date objects, so use their string 
# equivalents, then convert back to dates
onset_date_vs_peak_melt <- melt(valley_means_onset, id.vars='Year', measure.vars=c('peak_julian', 'monsoon_start_julian', 'amp'))
#onset_date_vs_peak_melt$value <- as.Date(onset_date_vs_peak_melt$value)
onset_date_vs_peak_melt$variable <- as.character(onset_date_vs_peak_melt$variable)
onset_date_vs_peak_melt$variable[onset_date_vs_peak_melt$variable == 'peak_julian'] <- 'EVI peak'
onset_date_vs_peak_melt$variable[onset_date_vs_peak_melt$variable == 'monsoon_start_julian'] <- 'Monsoon start'
onset_date_vs_peak_melt$variable[onset_date_vs_peak_melt$variable == 'amp'] <- 'Peak EVI amp.'
onset_date_vs_peak_melt$variable <- factor(onset_date_vs_peak_melt$variable, levels=c('Monsoon start',
                                                                                      'EVI peak',
                                                                                      'Peak EVI amp.'))
onset_date_vs_peak_melt_plot <- ggplot(onset_date_vs_peak_melt, aes(Year, value)) +
       geom_line() + guides(colour=FALSE) + facet_grid(variable~., scales='free') +
       xlab('Year') + geom_point(size=4) +
       theme(strip.text.y=element_text(size=27))
png('valley_means_monsoon_onset_vs_peak_melt.png', width=PLOT_WIDTH*PLOT_DPI, 
    height=PLOT_HEIGHT*PLOT_DPI)
print(onset_date_vs_peak_melt_plot)
dev.off()
