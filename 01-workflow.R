# This calls a python script that will create the proper folders
# It will split composite tifs into single channel tifs in proper folders

# previous step of .czi to .tiff was done in imageJ using `czi_to_composite_tiff` macro 

 
# composite .tif to single channel .tif ####

# Change here your input dir
input_dir <- '/media/mike/Elements/Axio\\ Scan/raw_data/MG952/001/001'

first <- "/home/mike/miniconda3/bin/python batch_composite_to_single_tiff.py -input_dir" 

# install.packages("reticulate")
#reticulate::use_python("YOUR PATH TO PYTHON 3")
# CHECK IT HERE....
#reticulate::py_config()

# otherwise try with the system command
# takes a while but will give you progress bar
system(paste(first, input_dir))