COMPILE_OPT idl2, hidden

overall_time = SYSTIME(1)

e = ENVI(/HEADLESS)

EVI_file_path = 'D:\Workspace\TEMP\EVI_stack.dat'

EVI_stack = e.OpenRaster(EVI_file_path)

x1 = 0
y1 = 0
x2 = 0
y2 = 0

pixel_data = EVI_stack.GetData(SUB_RECT=[x1, y1, x2, y2])

; Make sure pixel_data is a 1 dimensional vector so convolve doesn't chock on it 
pixel_data = REFORM(pixel_data, [N_ELEMENTS(pixel_data)])

plot_x_vals = FINDGEN(N_ELEMENTS(pixel_data))
p1 = plot(plot_x_vals, pixel_data, NAME='Original data')

savgol_filter = SAVGOL(4, 4, 0, 3)
pixel_data_filtered = CONVOL(pixel_data, savgol_filter, /EDGE_TRUNCATE)

p2 = plot(plot_x_vals, pixel_data_filtered, $
  NAME='Smoothed data', /OVERPLOT, THICK=2, COLOR=[0, 0, 255])

l=LEGEND()

PRINT, "------------------------------------------------------------"
PRINT, "Processing time ", STRTRIM((ROUND(SYSTIME(1) - overall_time)/60.), 2), $
  " minutes"