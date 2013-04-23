###############################################################################
# Script to convert .tpa format output from TIMESAT into an R dataframe for 
# further analysis.
###############################################################################

library(base) # Needed for file.info

###############################################################################
# Input parameters:
tpa_file_name <- "R:/Data/Nepal/Imagery/MODIS/TIMESAT_Test/Chitwan_MOD13Q1_CVFS_Study_Area_TS.tpa"

# Set NUM_SEASONS to the number of seasons in the TIMESAT file
NUM_SEASONS <- 10

# The number of seasonal indicators output by TIMESAT (shouldn't need to change 
# this)
SEASONAL_INDICATORS <- 11

# Number of elements in the tpa file line header (which are normally: row, 
# column, number of seasons). Shouldn't need to change this.
LINE_HEADER_SIZE <- 3

###############################################################################
# Code starts below here
tpa_file_obj <- file(tpa_file_name, "rb")
raw_vector <- readBin(tpa_file_obj, n=file.info(tpa_file_name)$size, raw())
close(tpa_file_obj)

# This function is used to track the offset within the binary vector as readBin 
# does not track position except for file objects
offset <- 1
raw_vec_length <- length(raw_vector)
offset_readBin <- function(raw_vec, what, n=n, size=size, ...) {
    bin_data <- readBin(raw_vec[offset:(n * size + offset)], what, n, size, ...)
    # Be lazy and use a global variable to track the offset
    assign("offset", offset + (size*n), envir = .GlobalEnv)
    return(bin_data)
}

# File header format is: nyears nptperyear rowstart rowstop colstart colstop
file_header <- offset_readBin(raw_vector, integer(), n=6, size=4)
rowstart <- file_header[3]
rowstop <- file_header[4]
colstart <- file_header[5]
colstop <- file_header[6]

num_pixels <- (colstop - colstart) * (rowstop - rowstart)

tpa_data <- matrix(nrow=num_pixels*NUM_SEASONS,
                   ncol=(LINE_HEADER_SIZE + SEASONAL_INDICATORS))

for (pixelnum in 1:num_pixels) {
    line_header <- offset_readBin(raw_vector, integer(), n=3, size=4)
    # Line header format is: rownum colnum numseasons
    if (line_header[3] != NUM_SEASONS) {
        stop(paste('pixel', pixelnum, 'has', line_header[3],
                   'seasons, but NUM_SEASONS is set to', NUM_SEASONS))
    }
    for (seasonnum in 1:NUM_SEASONS) {
        line_data <- offset_readBin(raw_vector, numeric(), 
                                    n=SEASONAL_INDICATORS, size=4)
        tpa_data_row <- (pixelnum-1)*NUM_SEASONS + seasonnum
        tpa_data[tpa_data_row, ] <- c(line_header[1], line_header[2],
                                      seasonnum, line_data)
    }
}

tpa_data <- data.frame(tpa_data)
names(tpa_data) <- c("row", "col", "season", "start","end", "length",
                     "base_value", "peak_time", "peak_value","amp", "L_deriv",
                     "R_deriv", "Linteg", "Sinteg")

tpa_file_name_base <- gsub('[.].*$', '', tpa_file_name)
save(tpa_data, file=paste(tpa_file_name_base, '.Rdata', sep=''))
