# step 2
# Import libraries #####
library(wholebrain)
library(SMART)
library(dplyr)
library(choices)

# To remove all objects but not functions
# rm(list = setdiff(ls(), lsf.str()))

source("resize_pad.R")
source("find_contours.R")
source("registration_MLA.R")
source("create_setup.R")

# This little function comes in handy to fix issues with the path
# when running from different computers, everything up to raw_data will be different
# so we switch that by the local environment's "raw_data" folder
# helper ro fix the working environment
fix_working_environment <- function(saved_path, local_path){
# this will explode if the folder structure doesn't work as expected...
stringr::str_replace(saved_path, ".+raw_data/", local_path)
}

# little helper function to order filter list and avoid repeating code
# ofl stands for "order filter list"
ofl <- function(match_df_2, filter_list){
  # match all in the new order
  new_order <- sapply(match_df_2$old_names, function(x) which(basename(names(filter_list)) == x))
  
  # we need to match the filters with the images
  ordered_filter_list <- filter_list[new_order]
  
  # Check if the reordering was good    
  message("Checking the order was done properly, should return TRUE")
  print(identical(basename(names(ordered_filter_list)), match_df_2$old_names)) # should return TRUE
  
  return(ordered_filter_list)
}

# Get Directory #####
# Please choose directory "001" within the animal you are trying to analyze
root_path <- choose_directory()
# get the animal ID from path
animal_id <- stringr::str_extract(root_path, "MG[0-9]+")
raw_data <- stringr::str_extract(root_path, ".+raw_data/")
raw_data <- ifelse(is.na(raw_data),
                   paste0(dirname(dirname(root_path)), "/"),
                   raw_data)

# find original files
# c0 is dapi files  
files <- list.files(path = file.path(root_path, 'c0'),
                    pattern = ".tif", full.names = TRUE)

# C0 -> resize_pad #####
# This takes some minutes per animal
# prints a bunch to console
# pb <- progress_estimated(length(files))
resize_list <- purrr::map(files, function(x) resize_pad(filename = x, pad=200))

# write list to file
write.csv(bind_rows(resize_list),
          file.path(root_path, paste0(animal_id, "_resize_list.csv")),
          row.names=FALSE)
              
resize_list <- read.csv(file.path(root_path, paste0(animal_id, "_resize_list.csv")),
                                  stringsAsFactors = FALSE)

# small -> find_contours ####

files <- list.files(path = file.path(root_path, 'small'),
                    pattern = ".tif", full.names = TRUE)
# This is faster than the previous step
filter_list <- purrr::map(files, function(x) find_contours(filename = x))

# apply the names
names(filter_list) <- files

# save the list to file
saveRDS(filter_list, file = file.path(root_path, 'small', "filter_list"))

# read previously saved file
filter_list <- readRDS(file = file.path(root_path, 'small', "filter_list"))


# get image_folder
image_folder <- bind_rows(resize_list) %>%
  dplyr::select(outname) %>%
  head(n=1) %>% pull() %>% dirname()

print(root_path)
print(image_folder)

# possible path problems here...
image_folder <- file.path(root_path, "small")

# Manual Step: Match image to atlas #####
# manual way matching AP level and images
match_df <- match_image_to_atlas(image_folder)

# check if the matching was correct
match_df <- inspect_AP_match(match_df)

# save the df! 
saveRDS(match_df, file.path(root_path, "atlas_img_path_df"))

# read it from file if already done
match_df <- readRDS(file.path(root_path, "atlas_img_path_df"))

## solve problem with duplicated

match_df_2 <- match_df %>% arrange(desc(mm.from.bregma))

# We can rename the images accordingly
# ONCE WE RENAME THEM, WE CAN'T ACCESS TO THEM ANYMORE VIA THE PATH
# WE MIGHT NEED TO ADJUST THAT IN ALL PREVIOUS FUNCTIONS THAT USE THE SMALL!!

match_df_2 <- rename_AP(match_df_2)

# we can get the old filenames with...
# lala <- list.files("raw_data/MG952/001/001/small/", 'tif', full.names = TRUE)
# file.rename(from = lala, to=gsub(lala, pattern = "_Z[0-9]+", replacement = ""))

match_df_2 <- match_df_2 %>%
  # convert to basename to make things simpler with paths
  mutate(old_names = basename(stringr::str_remove(image_file, pattern = "_Z0*[0-9]*")))

# Stop point ####
# save the new df! 
saveRDS(match_df_2, file.path(root_path, "ordered_atlas_img_path_df"))
  
# Create setup object ######
# read previously saved files
filter_list <- readRDS(file = file.path(root_path, 'small', "filter_list"))
match_df_2 <- readRDS(file.path(root_path, "ordered_atlas_img_path_df"))
## From here, we can get our setup values
setup <- create_setup(match_df_2)

# ofl stands for "order filter list"
ordered_filter_list <- ofl(match_df_2, filter_list)

# Automatic Registration -- Skip if already done ######
# Takes ! 15 minutes, will create regis object and save images to places (check setup$savepaths$out_auto_registration) 
# this is a good first pass but needs manual correction
regi_loop(setup, ordered_filter_list, autoloop = TRUE, brightness = 40)

# Manual Step: inspection of regi images #### 
plates <- inspect_wrong_regi_plates(setup)
# load previous regi
previous_regi <- list.files(file.path(root_path, "R_data"), full.names = TRUE)
load(file = previous_regi)

# Manual fix of registration ####
regi_loop(setup, regis = regis,
          touchup = plates, brightness = 20,
          filter = ordered_filter_list, autoloop = FALSE)

# TODO: fix this thing
# This variable gets written into a weird thing of length 19
# things like "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/R_data/Animal_2.20950382, 2.10835878, 1.60263359, 1.50148855, 1.29919847, 1.0969084, 0.99576336, 0.79347328, 0.692328244274809, 0.59118321, 0.08545802, -0.31912213740458, -0.42026718, -1.12828244, -1.22942748, -1.5328626, -1.73515267, -1.83629771, -2.03858779_2019-09-11_1.RData" 
# setup$savepaths$envir_savepath

# Stop point #####  
# Save regis!
save(regis, file = previous_regi)
  





