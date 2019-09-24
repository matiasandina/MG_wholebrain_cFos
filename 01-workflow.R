# This calls a python script that will create the proper folders
# It will split composite tifs into single channel tifs in proper folders

# .czi to .tiff in imageJ using `czi_to_composite_tiff` macro 
# check path to imageJ is correct

# This will print a lot of 
# [WARN] Unknown IlluminationType value 'Fluorescence' will be stored as "Other"
# don't worry, it's fine
# each call takes A WHILE

# getwd() should be "/home/mike/MG_wholebrain_cFos" (or the equivalent MG_wholebrain_cFos folder)

macro_to_run <- list.files(getwd(), "czi_to_composite_tiff", full.names = TRUE)

system(paste("/home/mike/Downloads/Fiji.app/ImageJ-linux64 --run",
             macro_to_run))


#### MANUAL STEP ######
# 1) delete low resolution images & color photos
# 2) manually arrange all images into one directory, you can name it 001

# fix names ####
# the imageJ macro puts nasty things into the files, we can remove that
# select the directory with all the images (i.e., the one you named 001)

root_path <- SMART::choose_directory()
# get the animal ID from path
animal_id <- stringr::str_extract(root_path, "MG[0-9]+")

old_names <- list.files(root_path, pattern=".tif", full.names = TRUE)
new_names <- gsub(pattern = ".czi #", replacement = "_", x = old_names, fixed = TRUE)

# we also fix the space between slide and the number
new_names <-gsub(pattern = "slide ", "slide-", x = new_names)

file.rename(from=old_names, to=new_names)

# composite .tif to single channel .tif ####


# fix put \\ if the path has spaces (needed for the console command) 
fix_spaces <- function(x) gsub(x, pattern = " ", replacement = "\\ ", fixed = TRUE)

# create the input directory for the command line call
input_dir <- fix_spaces(root_path)

first <- "/home/mike/miniconda3/bin/python batch_composite_to_single_tiff.py -input_dir" 

# install.packages("reticulate")
#reticulate::use_python("YOUR PATH TO PYTHON 3")
# CHECK IT HERE....
#reticulate::py_config()

# otherwise try with the system command
# takes a while but will give you progress bar
system(paste(first, input_dir))