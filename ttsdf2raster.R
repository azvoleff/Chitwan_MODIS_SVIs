###############################################################################
# Function to convert TIMESAT tts data.frame an R raster. 
###############################################################################

ttsdf2raster <- function(ttsdf, base_image_file) {
    # Parameters:
    # base_image_file should be one of the original MODIS files that was fed 
    # into TIMESAT.
    require(raster)

    if (missing(ttsdf) || !is.data.frame(ttsdf)) {
        stop('must specify a tts data.frame')
    } else if (missing(base_image_file) || !file.exists(base_image_file)) {
        stop('must specify a valid base image raster')
    }

    t_cols <- grep('^t[0-9]{1,4}$', names(ttsdf))

    base_image <- raster(base_image_file)

    out_rasters <- c()
    for (t_col in t_cols) {
        this_time_data <- ttsdf[, t_col]
        data_matrix <- matrix(NA, nrow(base_image), ncol(base_image))
        vector_indices <- (nrow(data_matrix) * ttsdf$col) - 
            (nrow(data_matrix) - ttsdf$row)
        data_matrix[vector_indices] <- this_time_data
        out_raster <- raster(data_matrix, template=base_image)
        out_rasters <- c(out_rasters, out_raster)
    }
    out_rasters <- stack(out_rasters)
    names(out_rasters) <- names(ttsdf[t_cols])

    return(out_rasters)
}
