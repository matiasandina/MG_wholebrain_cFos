# This calls a python script that will create the proper folders
# It will split composite tifs into single channel tifs in proper folders

# .czi to .tiff in imageJ using `czi_to_composite_tiff` macro 
# check path to imageJ is correct

# This will print a lot of 
# [WARN] Unknown IlluminationType value 'Fluorescence' will be stored as "Other"
# don't worry, it's fine  
# each call takes A WHILE

# Find imageJ on your system
imageJ <- choices::choose_files(title="Choose your ImageJ executable file", multiple = FALSE)
# getwd() should be "/home/mike/MG_wholebrain_cFos" (or the equivalent MG_wholebrain_cFos folder)
macro_to_run <- list.files(getwd(), "czi_to_composite_tiff", full.names = TRUE)
  
fix_spaces <- function(x){ 
  # fix spaces
  x <- gsub(x, pattern = " ", replacement = "\\ ", fixed = TRUE)
  # convert path for windows
  if (R.version$os == "mingw32"){
    x <- gsub("/", replacement = "\\\\", x)
  }
  return(x)
  }

system(paste(fix_spaces(imageJ) ,"--run",
               fix_spaces(macro_to_run)))


#### MANUAL STEP ######
# 1) delete low resolution images & color photos
# 2) manually arrange all images into one directory, you can name it 001

# fix names ####
# the imageJ macro puts nasty things into the files, we can remove that
# select the directory with all the images (i.e., the one you named 001)

root_path <- choices::choose_directory()
# get the animal ID from path
animal_id <- stringr::str_extract(root_path, "MG[0-9]+")

old_names <- list.files(root_path, pattern = ".tif", full.names = TRUE)
new_names <- gsub(pattern = ".czi #", replacement = "_", x = old_names, fixed = TRUE)

# we also fix the space between slide and the number
new_names <- gsub(pattern = "slide ", "slide-", x = new_names)

file.rename(from = old_names, to = new_names)

# composite .tif to single channel .tif ####

# create the input directory for the command line call
input_dir <- fix_spaces(root_path)

find_python <- function(){
  if (R.version$os == "mingw32"){
    python_options <- system("where python", intern = TRUE)  
    } else {
    python_options <- system("which -a python3", intern = TRUE)
    
  }
  if(length(python_options) == 0) {
    stop("COULDN'T FIND PYTHON, DON'T CONTINUE, CHECK YOUR PATH")
  } else {
    return(python_options)
  }
}

# find python
python_options <- find_python()
python3 <- choices:::numeric_menu(opts = python_options,
                                  prompt = "Choose your python path (recommended: /usr/bin/python3)")


python_command <- paste(python3, "batch_composite_to_single_tiff.py", "-input_dir")

# install.packages("reticulate")
#reticulate::use_python("YOUR PATH TO PYTHON 3")
# CHECK IT HERE....
#reticulate::py_config()

# otherwise try with the system command
# takes a while but will give you progress bar
system(paste(python_command, input_dir))
