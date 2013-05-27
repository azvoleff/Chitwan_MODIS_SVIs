library(ggplot2)
library(lubridate)
require(animation)

source('tpa2df.R')
source('tpadf2raster.R')

tpa_file_name <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_Expanded_TS.tpa'
tpa_df <- tpa2df(tpa_file_name, 11)

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

# Setup animation and plot options
ani.options(convert=shQuote('C:/Program Files (x86)/ImageMagick-6.8.3-Q16/convert.exe'))
ani.options(outdir='D:/Workspace/', ani.width=1600, ani.height=1200)
new_mar = par("mar")
new_mar[1] <- new_mar[1] + .4 # Add to the top margin

# Function used to make each animation frame
plot_TIMESAT_variable <- function(TIMESAT_raster, timestep, zlim, title) {
    # Set margins (add a little to the default bottom margin)
    par("mar"=new_mar)
    img_mean <- round(cellStats(TIMESAT_raster, mean), 2)
    img_max <- round(cellStats(TIMESAT_raster, max), 2)
    img_min <- round(cellStats(TIMESAT_raster, min), 2)
    img_sd <- round(cellStats(TIMESAT_raster, sd), 2)
    plot(TIMESAT_raster, main=paste('Time:', format(timestep, width=4)),
         axes=FALSE, xlab="", ylab="", cex.main=4, cex.sub=3,
         sub=paste("Mean: ", img_mean, ", Max: ", img_max, 
                   ", Min: ", img_min, ", SD:", img_sd, sep=""),
         zlim=zlim,
         legend.width=4, legend.mar=12,
         axis.args=list(cex.axis=3),
         legend.args=list(text=title, side=4, line=6, font=2, cex=3))
}

TIMESAT_variables <- c("start_julian", "end_julian", "length_days", "base_value", "peak_time_julian", 
                       "peak_value", "amp", "L_deriv", "R_deriv", "Linteg", 
                       "Sinteg")
base_image_file <- 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'
output_folder <- 'D:/Workspace/'
output_basename <- 'tparaster'
tparasters <- c()
for (TIMESAT_variable in TIMESAT_variables) {
    tparaster <- tpadf2raster(tpa_df, base_image_file, TIMESAT_variable)
    tparasters <- c(tparasters, tparaster)
    writeRaster(tparaster, file.path(output_folder,
                                     paste(output_basename, '_', 
                                           TIMESAT_variable, sep="")), 
                format='ENVI', overwrite=TRUE)
    animation_file <- paste(output_basename, '_', TIMESAT_variable, 
                            '_animation.gif', sep="")
    zmin <- min(cellStats(tparaster, min))
    zmax <- max(cellStats(tparaster, max))
    zlim <- c(zmin, zmax)
    cropped_tparaster <- crop(tparaster, extent(tparaster, 88, 156, 250, 382))
    saveGIF({for (n in 1:nlayers(tparaster)) 
        plot_TIMESAT_variable(cropped_tparaster[[n]], n, zlim, TIMESAT_variable)}, 
        interval=0.5, movie.name=animation_file)
}
