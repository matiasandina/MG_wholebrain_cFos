# COMPLETELY REMOVE ANY OPENCV from your computer
# wholebrain will build opencv from ROpenCVLite 

# devtools::install_github("tractatus/wholebrain", INSTALL_opts = c('--no-lock'))
# For ubuntu Make sure magick 
# sudo apt-get install libmagick++-dev
# install.packages("magick")

# devtools::install_github("mjin1812/SMART")


library(wholebrain)
library(SMART)
library(imager)
source("registration_MLA.R")

# Do setup config ####

setup <- setup_pl()
setup <- im_sort(setup)
setup <- get_savepaths(setup)


trace(regi_loop, edit=TRUE)

setup <- choice_MLA(setup, xpos=c(100, 500, 900))

# Run the autoloop
regi_loop(setup, autoloop = TRUE, filter = filter)


setwd("/media/mike/Elements/Axio Scan/raw_data/MG952/001")
filename <- file.path(pattern="small_2019_08_19__0001_16.tif")

# seg<-segment(filename) 
get.atlas.image(coordinate = 2, close.image=FALSE)



# replace registration2 for interactive
# to run line by line
debug(registration_MLA)
debug(registration)
regi <- registration_MLA(filename,
                      coordinate = 2,
                      verbose=TRUE,
                      num.nested.objects = 0,
                      filter = DAPI_filter)



## Call python
# We have to reproduce this call in a Sys.call()
python read_large.py -img_filename "/media/mike/Elements/Axio Scan/raw_data/MG952/001/2019_08_19__0001_11.tif" -df_path "/media/mike/Elements/Axio Scan/raw_data/MG952/001/small_2019_08_19__0001_11.csv" -display_crop True -display_contours False