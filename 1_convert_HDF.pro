COMPILE_OPT idl2, hidden

overall_time = SYSTIME(1)

e = ENVI(/HEADLESS)

;MODIS_product = 'MOD13Q1'
MODIS_product = 'MYD13Q1'
input_folder = 'G:\Data\Nepal\Imagery\MODIS\' + MODIS_product 
image_list = FILE_SEARCH(input_folder + PATH_SEP() + $
   MODIS_product + '.A*.h25v06.*.hdf', count=count, /TEST_READ)
year_regex = MODIS_product + '.A[0-9]{7}.'

output_folder = 'D:\Workspace\TEMP\'

; Choose grids and setup parameters for conversion from HDF to ENVI format
gd_name = 'MODIS_Grid_16DAY_250m_500m_VI'
;sd_names = ['250m 16 days NDVI', '250m 16 days EVI', '250m 16 days VI Quality']
sd_names = ['250m 16 days EVI', '250m 16 days VI Quality', '250m 16 days pixel reliability']
; Create a WGS-84 projection
out_proj = ENVI_PROJ_CREATE(/GEOGRAPHIC)
; Note that INTERP_METHOD=6 means nearest neighbor
interp_method = 6
; out_method = 1 means only output reprojected data in ENVI format
out_method = 1
; Specify 250 m pixel size in degrees
out_ps_x = .00224577d
out_ps_y = .00224577d

FOR i=0, count-1 DO BEGIN
  image_time = SYSTIME(1)
  
  input_hdf = image_list[i]
  
  PRINT, 'Running HDF to ENVI format conversion on ', STRTRIM(input_hdf, 2)
  
  year_string = STREGEX(FILE_BASENAME(input_hdf), year_regex, /extract)
  out_root = STREGEX(year_string, '[0-9]{7}', /extract)
  
  r_fid_array = []
  r_fname_array = []
  CONVERT_MODIS_DATA, IN_FILE=input_hdf, OUT_PATH=output_folder, $
    OUT_ROOT=out_root, /HIGHER_PRODUCT, /GRID, GD_NAME=gd_name, $
    SD_NAMES=sd_names, OUT_METHOD=out_method, OUT_PROJ=out_proj, $
    OUT_PS_X=out_ps_x, OUT_PS_Y=out_ps_y, NUM_X_PTS=50, NUM_Y_PTS=50, $
    INTERP_METHOD=interp_method, R_FID_ARRAY=r_fid_array, $
    R_FNAME_ARRAY=r_fname_array
    
  PRINT, 'Cropping to Chitwan AOI.'
  ; Below works since only one file is being output, the projected file
  reproj_fid = r_fid_array[0]
  
  ENVI_FILE_QUERY, reproj_fid, DIMS=orig_dims, NB=nb
  ; Subset to Chitwan AOI
  Xmap = [83.6d, 85.0d]
  Ymap = [27.9d, 27.3d]
  ; now convert coordinates into pixel position:
  ; the transformation function uses the image geographic information:
  ENVI_CONVERT_FILE_COORDINATES, reproj_fid, XF, YF, XMap, YMap
  XF=ROUND(XF)
  YF=ROUND(YF)
  
  ; read a subset of each band
  cropped_dims = [-1, XF, YF]
  evi = ENVI_GET_DATA(FID=reproj_fid, DIMS=cropped_dims, pos=0L)
  quality = ENVI_GET_DATA(FID=reproj_fid, DIMS=cropped_dims, pos=1L)
  reliability = ENVI_GET_DATA(FID=reproj_fid, DIMS=cropped_dims, pos=2L)
  
  ; Now mask out pixels with quality issues
  ;evi = evi * (reliability EQ 0 OR reliability EQ 1)
  
  ; read mapinfo from original file to save it in the final file
  map_info = ENVI_GET_MAP_INFO(FID=reproj_fid)
  ; Adjust the upper left X and Y coordinates in the map_info to take account of
  ; the fact that the upper left X and Y have moved in the cropped image relative
  ; to the original image the map_info was derived from:
  map_info.MC[2] = map_info.MC[2] + map_info.PS[0] * XF[0]
  map_info.MC[3] = map_info.MC[3] - map_info.PS[1] * YF[0]
  
  output_evi_image = out_root + MODIS_product + '_EVI.img'
  ENVI_WRITE_ENVI_FILE, evi, OUT_NAME=output_evi_image, $
    MAP_INFO=map_info, R_FID=evi_fid
    
  output_quality_image = out_root + MODIS_product + '_quality.img'
  ENVI_WRITE_ENVI_FILE, quality, OUT_NAME=output_quality_image, $
    MAP_INFO=map_info, R_FID=quality_fid
    
  output_reliability_image = out_root + MODIS_product + '_reliability.img'
  ENVI_WRITE_ENVI_FILE, reliability, OUT_NAME=output_reliability_image, $
    MAP_INFO=map_info, R_FID=reliability_fid
    
  PRINT, "Deleting intermediate files."
  ENVI_FILE_MNG, id=reproj_fid, /REMOVE, /DELETE
  ENVI_FILE_MNG, id=evi_fid, /REMOVE
  ENVI_FILE_MNG, id=quality_fid, /REMOVE
  ENVI_FILE_MNG, id=reliability_fid, /REMOVE
  
  PRINT, "Processing time for ", STRTRIM(FILE_BASENAME(input_hdf), 2), $
    ": ", STRTRIM(ROUND(SYSTIME(1) - image_time), 2), " seconds"
  PRINT, "------------------------------------------------------------"
ENDFOR

ENVI_BATCH_EXIT
PRINT, "************************************************************"
PRINT, "      Completed MODIS format conversion and subsetting"
PRINT, "************************************************************"
PRINT, "Total processing time: ", STRTRIM(ROUND(SYSTIME(1) - overall_time),2), $
  " seconds"
  
END