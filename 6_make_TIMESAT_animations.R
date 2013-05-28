library(ggplot2)
library(gridExtra) # for 'unit'
library(plyr) # for 'join'
library(lubridate)
require(animation)
require(raster)
require(rgdal)

source('tpa2df.R')
source('tpadf2raster.R')

source('C:/Users/azvoleff/Code/R/Chitwan_R_files/Climate/0_utility_functions.R')

tpa_file_name <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/Chitwan_MOD13Q1_EVI_Full_Series_Cropped_Expanded_TS.tpa'
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
ani.options(outdir='D:/Workspace/', ani.width=PLOT_WIDTH*2*PLOT_DPI, ani.height=PLOT_HEIGHT*.5*2*PLOT_DPI)

# Load CVFS area polygon so it can be shown on the image
cvfs_area <- readOGR(dsn="R:/Data/Nepal/GIS/ChitwanDistrict", layer="CVFS_Study_Area")
# Need to transform CVFS polygon to WGS84 since it is in UTM
cvfs_area  <- spTransform(cvfs_area, CRS("+init=epsg:4326"))
cvfs_area@data$id <- rownames(cvfs_area@data)
cvfs_area.points <- fortify(cvfs_area, region="id")
cvfs_area.df <- join(cvfs_area.points, cvfs_area@data, by="id")
plot_TIMESAT_variable <- function(data_raster, zlim, variable, title_string='', size_scale=1) {
    theme_set(theme_bw(base_size=8*size_scale))
    data_raster_df <- data.frame(x=coordinates(data_raster)[, 1],
                                 y=coordinates(data_raster)[, 2],
                                 value=getValues(data_raster))
    ggplot(data_raster_df) +
        geom_raster(aes(x, y, fill=value)) + coord_fixed() + 
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm')) +
        scale_fill_gradient2(midpoint=mean(zlim), low='blue', mid='yellow', high='red',
                             limits=zlim, name=variable) +
        guides(fill=guide_colorbar(barwidth=.8*size_scale, barheight=6*size_scale, ticks=FALSE)) +
        geom_path(data=cvfs_area.df, aes(long, lat), color='black', size=.5*size_scale, alpha=.7) +
        ggtitle(title_string)
}

years <- seq(2000, 2011)
TIMESAT_variables <- c("start_julian", "end_julian", "length_days", "base_value", "peak_time_julian", 
                       "peak_value", "amp", "L_deriv", "R_deriv", "Linteg", 
                       "Sinteg")
base_image_file <- 'R:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped/2000001_MOD13Q1_EVI_scaled_flt16.envi'
output_folder <- 'D:/Workspace/'
output_basename <- 'tparaster'
tparasters <- c()
for (TIMESAT_variable in TIMESAT_variables) {
    tparaster <- tpadf2raster(tpa_df, base_image_file, TIMESAT_variable)
    tparasters <- c(tparasters, tparaster)
    # writeRaster(tparaster, file.path(output_folder,
    #                                  paste(output_basename, '_', 
    #                                        TIMESAT_variable, sep="")), 
    #             format='ENVI', overwrite=TRUE)
    # animation_file <- paste(output_basename, '_', TIMESAT_variable, 
    #                         '_animation.gif', sep="")
    zmin <- min(cellStats(tparaster, min))
    zmax <- max(cellStats(tparaster, max))
    zlim <- c(zmin, zmax)
    cropped_tparaster <- crop(tparaster, extent(tparaster, 85, 159, 247, 385))
    #saveGIF({for (n in 1:nlayers(tparaster)) {p <- plot_TIMESAT_variable(cropped_tparaster[[n]], zlim, TIMESAT_variable, years[n], size_scale=5); print(p)}}, interval=0.5, movie.name=animation_file)
    # Make an example plot of the 3rd raster (2002) just for show:
    example_file <- paste(output_basename, '_', TIMESAT_variable, '_2008_example.png', sep="")
    plot_TIMESAT_variable(cropped_tparaster[[9]], zlim, TIMESAT_variable)
    ggsave(example_file, width=PLOT_WIDTH*2, height=PLOT_HEIGHT*.5*2, dpi=PLOT_DPI)
}
