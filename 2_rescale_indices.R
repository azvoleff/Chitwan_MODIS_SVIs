library(raster)

MODIS_DIR = 'G:/Data/Nepal/Imagery/MODIS/MOD13Q1_Chitwan_Cropped'
SVIs <- c('EVI')
quality_layers <- c('reliability')
#products <- c('MOD13Q1', 'MYD13Q1')
products <- c('MOD13Q1')

products_regex <- paste('(', paste(products, collapse='|'), ')', sep='') 
SVI_regex <- paste('(', paste(SVIs, collapse='|'), ')', sep='') 
SVI_regex <- paste('^[0-9]{7}_', products_regex, '_', SVI_regex, '.img$', sep='')
quality_regex <- paste('(', paste(quality_layers, collapse='|'), ')', sep='') 
quality_regex <- paste('^[0-9]{7}_', products_regex, '_', quality_regex, '.img$', sep='')

file_list <- list.files(MODIS_DIR)

SVI_files <- file.path(MODIS_DIR, file_list[grepl(SVI_regex, file_list)])
for (SVI_file in SVI_files) {
    SVI_raster <- raster(SVI_file)
    scaled_raster <- raster(round(getValues(SVI_raster,
                                            format='matrix') * 10000), 
                            template=SVI_file)
    out_name <- gsub('.img$', '_scaled_flt16.envi', SVI_file)
    # Save in 16 bit signed integer BSQ format with an ENVI header
    writeRaster(scaled_raster, filename=out_name, format='ENVI',
                options='INTERLEAVE=BSQ', datatype='INT2S', overwrite=TRUE)
}

quality_files <- file.path(MODIS_DIR, file_list[grepl(quality_regex, file_list)])
for (quality_file in quality_files) {
    quality_raster <- raster(quality_file)
    scaled_raster <- raster(round(getValues(quality_raster, format='matrix')),
                            template=quality_file)
    out_name <- gsub('.img$', '_flt16.envi', quality_file)
    # Save in 16 bit signed integer BSQ format with an ENVI header
    writeRaster(scaled_raster, filename=out_name, format='ENVI', 
                options='INTERLEAVE=BSQ', datatype='INT2S', overwrite=TRUE)
}
