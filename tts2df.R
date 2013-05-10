###############################################################################
# Function to convert TIMESAT .tts binary format to an R dataframe.
###############################################################################

tts2df <- function(tts_file_name) {
    require(base) # Needed for file.info

    if (missing(tts_file_name) || !grepl('[.]tts$', tolower(tts_file_name))) {
        stop('must specify a .tts file')
    }

    # Number of elements in the tts file line header (which are normally: row, 
    # column). Shouldn't need to change this.
    LINE_HEADER_SIZE <- 2

    tts_file_obj <- file(tts_file_name, "rb")
    raw_vector <- readBin(tts_file_obj, n=file.info(tts_file_name)$size, raw())
    close(tts_file_obj)

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
    n_pts_per_year <- file_header[2]
    rowstart <- file_header[3]
    rowstop <- file_header[4]
    colstart <- file_header[5]
    colstop <- file_header[6]

    num_pixels <- (colstop - colstart) * (rowstop - rowstart)

    # Include 2 extra columns to code the row and col IDs
    tts_data <- matrix(nrow=num_pixels, ncol=(2 + n_pts_per_year * num_years))
    for (pixelnum in 1:num_pixels) {
        line_header <- offset_readBin(raw_vector, integer(), n=2, size=4)
        # Line header format is: rownum colnum
        tts_data[pixelnum, ] <- c(line_header[1], line_header[2], 
                                  offset_readBin(raw_vector, numeric(), 
                                                 n=n_pts_per_year*num_years, 
                                                 size=4))
    }
    tts_data <- data.frame(tts_data)

    names(tts_data) <- c("row", "col",
                         paste('t', seq(1, n_pts_per_year*num_years), sep=''))
    return(tts_data)
}
