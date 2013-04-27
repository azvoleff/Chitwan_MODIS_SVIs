MODIS_DIR = 'R:/timesat311/data/Chitwan_MOD13Q1_Cropped'
#file_types <- c('EVI', 'quality', 'reliability')
file_types <- c('EVI', 'reliability')
#products <- c('MOD13Q1', 'MYD13Q1')
products <- c('MOD13Q1')

products_regex <- paste('(', paste(products, collapse='|'), ')', sep='') 
regexes <- paste('^[0-9]{7}_', products_regex, '_', file_types, '.img$', sep='')

MODIS_files <- list.files(MODIS_DIR)
for (n in 1:length(file_types)) {
    these_MODIS_files <- MODIS_files[grepl(regexes[n], MODIS_files)]
    these_MODIS_files <- file.path(MODIS_DIR, these_MODIS_files)

    image_list_file <- file.path(MODIS_DIR, paste('image_list_', file_types[n], 
                                                  '.txt', sep=''))

    write.table(c(length(these_MODIS_files), these_MODIS_files),
                file=image_list_file, quote=FALSE, col.names=FALSE, 
                row.names=FALSE)

    date_strings <- data.frame(DATE_STRING=regmatches(these_MODIS_files, 
                                                      regexpr('[0-9]{7}', 
                                                              these_MODIS_files)))
    date_strings$YEAR <- as.numeric(substr(date_strings$DATE_STRING, 1, 4))
    date_strings$JULIAN_DAY <- as.numeric(substr(date_strings$DATE_STRING, 5, 7))
    date_strings$PRODUCT <- regmatches(these_MODIS_files, regexpr(products_regex, these_MODIS_files))

    image_dates_file <- file.path(MODIS_DIR, paste('image_dates_', 
                                                   file_types[n], '.csv', 
                                                   sep=''))
    write.csv(date_strings, file=image_dates_file, row.names=FALSE)
}

d_dates <- date_strings$JULIAN_DAY[1:(nrow(date_strings) - 1)] - date_strings$JULIAN_DAY[2:nrow(date_strings)]
table(d_dates)
