COMPILE_OPT idl2, hidden

overall_time = SYSTIME(1)

e = ENVI(/HEADLESS)

input_folder = 'D:\Workspace\TEMP\'
output_folder = 'D:\Workspace\TEMP\'

suffixes = ["EVI", "quality", "reliability"]

FOR i=0, N_ELEMENTS(suffixes)-1 DO BEGIN
  suffix_time = SYSTIME(1)
  
  suffix = suffixes[i]
  
  image_list = FILE_SEARCH(input_folder + PATH_SEP() + $
    '*_' + suffix + '.img', count=count, /TEST_READ)
  dates = STREGEX(image_list, '2[0-9]{6}', /EXTRACT)
  
  layer_fids = []
  layer_pos = []
  layer_dims = []
  FOR image_num=0, count-1 DO BEGIN
    image = image_list[image_num]
    ENVI_OPEN_FILE, image, R_FID=layer_fid
    ENVI_FILE_QUERY, layer_fid, DIMS=dims, DATA_TYPE=data_type
    
    layer_fids = [layer_fids, layer_fid]
    layer_pos = [layer_pos, 0L]
    layer_dims = [[layer_dims], [dims]]
  ENDFOR
  
  output_image = output_folder + PATH_SEP() + suffix + '_stack.dat'
  out_proj = ENVI_GET_PROJECTION(FID=layer_fids[0], PIXEL_SIZE=out_ps)
  ENVI_DOIT, 'ENVI_LAYER_STACKING_DOIT', FID=layer_fids, DIMS=layer_dims, $
    POS=layer_pos, OUT_NAME=output_image, /EXCLUSIVE, R_FID=layer_stack_fid, $
    OUT_PS=out_ps, OUT_DT=data_type, OUT_PROJ=out_proj, INTERP=0
    
  ; Saves dates to a csv file
  date_codes = REFORM(dates, [1, count])
  date_array = [date_codes, STRMID(date_codes, 0, 4), STRMID(date_codes, 4, 3)]
  output_csv_file = output_folder + PATH_SEP() + suffix + '_stack_times.csv'
  s = SIZE(date_array, /Dimensions)
  xsize = s[0]
  lineWidth = 1600
  sData = STRTRIM(date_array, 2)
  sData[0:xsize-2, *] = sData[0:xsize-2, *] + ','
  OPENW, lun, output_csv_file, /Get_Lun, Width=lineWidth
  PRINTF, lun, 'DATE_CODE, YEAR, JULIAN_DAY'
  PRINTF, lun, sData
  FREE_LUN, lun
  
  ; Saves image list to a txt file (needed for TIMESAT)
  image_files = REFORM(image_list, [1, count])
  output_csv_file = output_folder + PATH_SEP() + suffix + '_image_files.txt'
  s = SIZE(image_files, /Dimensions)
  xsize = s[0]
  lineWidth = 1600
  sData = STRTRIM(image_files, 2)
  sData[0:xsize-2, *] = sData[0:xsize-2, *]
  OPENW, lun, output_csv_file, /Get_Lun, Width=lineWidth
  PRINTF, lun, count
  PRINTF, lun, sData
  FREE_LUN, lun
  
  PRINT, "Processing time for ", STRTRIM(suffix, 2), $
    ": ", STRTRIM(ROUND(SYSTIME(1) - suffix_time), 2), " seconds"
  PRINT, "------------------------------------------------------------"
ENDFOR

PRINT, "************************************************************"
PRINT, "             Completed MODIS layer stacking."
PRINT, "************************************************************"
PRINT, "Total processing time: ", STRTRIM(ROUND(SYSTIME(1) - overall_time),2), $
  " seconds"
  
END
