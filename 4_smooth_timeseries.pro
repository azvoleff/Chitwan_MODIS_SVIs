COMPILE_OPT idl2, hidden

!PATH = EXPAND_PATH('+C:\Users\azvoleff\Code\IDL\coyote\') + ';' + !PATH

overall_time = SYSTIME(1)

e = ENVI(/HEADLESS)

input_data_path = 'D:\Workspace\TEMP\EVI_stack.dat'
filtered_data_path = 'D:\Workspace\TEMP\EVI_stack_filtered.dat'

orig_raster = e.OpenRaster(input_data_path)
orig_data = orig_raster.GetData()

filt_data = orig_data
filt_data[*] = 0

dims = SIZE(orig_data, /DIMENSIONS)

cgProgressBar = OBJ_NEW("CGPROGRESSBAR", /Cancel)
cgProgressBar -> Start
FOR col_num=0, (dims[0] - 1) DO BEGIN
  IF cgProgressBar -> CheckCancel() THEN BEGIN
    ok = DIALOG_MESSAGE('The user cancelled operation.')
    RETURN
  ENDIF
  
  FOR row_num=0, (dims[1] - 1) DO BEGIN
    pixel_data = orig_data[col_num, row_num, *]
    
    ; Make sure pixel_data is a 1 dimensional vector so convolve doesn't chock on it
    pixel_data = REFORM(pixel_data, [N_ELEMENTS(pixel_data)])
    
    savgol_filter = SAVGOL(4, 4, 0, 3)
    filt_data[col_num, row_num, *] = CONVOL(pixel_data, savgol_filter, /EDGE_TRUNCATE)
    
    ;plot_x_vals = FINDGEN(N_ELEMENTS(pixel_data))
    ;p1 = plot(plot_x_vals, pixel_data, NAME='Original data')
    ;p2 = plot(plot_x_vals, pixel_data_filtered, $
    ;  NAME='Smoothed data', /OVERPLOT, THICK=2, COLOR=[0, 0, 255])
    ;l=LEGEND(/AUTO_TEXT_COLOR)
  ENDFOR
  cgProgressBar -> Update, (FLOAT(col_num)/FLOAT(dims[0]))*100.
ENDFOR
cgProgressBar -> Destroy

filt_raster = e.CreateRaster(filtered_data_path, filt_data, $
  INHERITS_FROM=orig_raster)
filt_raster.Save

PRINT, "------------------------------------------------------------"
PRINT, "Processing time ", STRTRIM((ROUND(SYSTIME(1) - overall_time)/60.), 2), $
  " minutes"
  
END