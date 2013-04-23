###############################################################################
# Function to convert TIMESAT .tpa binary format to an R dataframe.
###############################################################################

tpa2df <- function(tpa_file_name, num_seasons) {
    require(base) # Needed for file.info

    if (missing(tpa_file_name) || !grepl('[.]tpa$', tolower(tpa_file_name))) {
        stop('must specify a .tpa file')
    } else if (missing(num_seasons)) {
        stop('must specify number of seasons')
    }

    # The number of seasonal indicators output by TIMESAT (shouldn't need to 
    # change this)
    SEASONAL_INDICATORS <- 11

    # Number of elements in the tpa file line header (which are normally: row, 
    # column, number of seasons). Shouldn't need to change this.
    LINE_HEADER_SIZE <- 3

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
        assign("offset", offset + (size*n), inherits=TRUE)
        return(bin_data)
    }

    # File header format is: nyears nptperyear rowstart rowstop colstart colstop
    file_header <- offset_readBin(raw_vector, integer(), n=6, size=4)
    num_years <- file_header[1]
    rowstart <- file_header[3]
    rowstop <- file_header[4]
    colstart <- file_header[5]
    colstop <- file_header[6]

    num_pixels <- (colstop - colstart) * (rowstop - rowstart)

    tpa_data <- matrix(nrow=num_pixels*num_seasons,
                       ncol=(LINE_HEADER_SIZE + SEASONAL_INDICATORS))
    for (pixelnum in 1:num_pixels) {
        line_header <- offset_readBin(raw_vector, integer(), n=3, size=4)
        # Line header format is: rownum colnum numseasons
        if (line_header[3] != num_seasons) {
            stop(paste('pixel', pixelnum, 'has', line_header[3],
                       'seasons, but num_seasons is set to', num_seasons))
        }
        for (seasonnum in 1:num_seasons) {
            line_data <- offset_readBin(raw_vector, numeric(), 
                                        n=SEASONAL_INDICATORS, size=4)
            tpa_data_row <- (pixelnum-1)*num_seasons + seasonnum
            tpa_data[tpa_data_row, ] <- c(line_header[1], line_header[2],
                                          seasonnum, line_data)
        }
    }

    tpa_data <- data.frame(tpa_data)
    names(tpa_data) <- c("row", "col", "season", "start", "end", "length",
                         "base_value", "peak_time", "peak_value", "amp", "L_deriv",
                         "R_deriv", "Linteg", "Sinteg")
    return(tpa_data)
}
