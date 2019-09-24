# choose working directory
root_path <- choose_directory()
# get the animal ID from path
animal_id <- stringr::str_extract(root_path, "MG[0-9]+")

# Make the croppings

# prepare paths for python call #####
# Get the files...they are supposedly in order
# TODO: can we MAKE SURE instead of 'supposedly' ?

df_paths <- list.files(file.path(root_path, "contours"), full.names = TRUE)
c0_files <- list.files(file.path(root_path, "c0"), full.names = TRUE)
c1_files <- list.files(file.path(root_path, "c1"), full.names = TRUE)

# fix put \\ if the path has spaces (needed for the console command) 
fix_spaces <- function(x) gsub(x, pattern = " ", replacement = "\\ ", fixed = TRUE)

df_paths <- fix_spaces(df_paths)
c0_files <- fix_spaces(c0_files)
c1_files <- fix_spaces(c1_files)

python_command <- "/home/mike/miniconda3/bin/python read_large.py " 

# helper function to move files

move_crops <- function(root_path, folder_name, pattern="left|right"){
  # it is expecting to move files from a root path to a folder within root path
  original_filepaths <- list.files(root_path, pattern = "left|right",
                                   full.names = TRUE)
  
  if(length(original_filepaths) == 0){
    print(root_path)
    print(list.files(root_path))
    stop("No files found. \nCheck whether the root file provided contains such files")
    }
  
  # create folder
  new_folder <- file.path(root_path, folder_name)
  if(!dir.exists(new_folder)){
    message("Creating folder: > ", new_folder)
    dir.create(new_folder)
  }
  
  # get the names
  file_ext <- tools::file_ext(original_filepaths)
  file_names <- tools::file_path_sans_ext(basename(original_filepaths))
  # create the new path
  file_names <- file.path(new_folder, file_names)
  # paste extension
  file_names <- paste0(file_names, ".", file_ext)
  
  message("Moving files")
  file.rename(from= original_filepaths , to = file_names)
  return("Done :)")  
}


## Call python ####
# This will happen in a for loop and for each channel on each iteration
for(i in 1:length(c0_files)){
  # c0 command
  c0_call <- paste(python_command,
                  '-img_filename', c0_files[i], 
                  '-df_path', df_paths[i],
                  '-grouping_var', "parent_roi_number",
                  "-display_crop False -display_contours True")
  message("cropping c0 files")
  message("Calling ", c0_call)
  
  # actual call
  system(c0_call)
  
  # now move the crops
  move_crops(root_path, "c0_crops", pattern="left|right")
  # c1 command
  c1_call <- paste(python_command,
                  '-img_filename', c1_files[i],
                  '-df_path', df_paths[i],
                  '-grouping_var', "parent_roi_number",
                  "-display_crop False -display_contours False")
  message("cropping c1 files")
  message("Calling ", c1_call)
  
  # actual call
  system(c1_call)
  
  move_crops(root_path, "c1_crops", pattern="left|right")
}
  


## Single tif to composite ####


# let's perform a sanity check, to make sure the files are correctly ordered
sanity_check <- tibble::tibble(
  c0 = list.files(file.path(root_path, "c0_crops"), full.names = TRUE),
  c1 = list.files(file.path(root_path, "c1_crops"), full.names = TRUE)) %>%
  mutate(x = stringr::str_remove(basename(c0), "_c0"),
         y = stringr::str_remove(basename(c1), "_c1"),
         flag = identical(x,y))

if(any(sanity_check$flag == FALSE)){
  message("ERROR IN THE ORDER OF FILES.\nCheck `sanity_check` with View(sanity_check).\nDO NOT PROCEED WITH PIPELINE!")
} else {
    message("Files match, proceed with pipeline :)")
  }

## at the end of the day, we also rely on python listdir_fullpath to have the same sorting arrangement
## we believe that if the naming is consistent, files will be correctly sorted

## Call python ####
python_command_2 <- "/home/mike/miniconda3/bin/python batch_single_tiff_to_composite.py" 

input_dir_c0 <- paste("-input_dir_c0", fix_spaces(file.path(root_path, "c0_crops")))
input_dir_c1 <- paste("-input_dir_c1", fix_spaces(file.path(root_path, "c1_crops")))

# This command will create stacked images
system(paste(python_command_2, input_dir_c0, input_dir_c1))
# This command will make the composite
call_imagej <- "/home/mike/Downloads/Fiji.app/ImageJ-linux64 --headless --run"
# getwd() should be MG_wholebrain_cFos folder
macro_to_run <- list.files(getwd(), "merge_composites.ijm", full.names = TRUE)
# mind the commas ('') and double commas, no need to change spaces 
# it should be 'input = filepath' with filepath between this commas "" 
options(useFancyQuotes = FALSE)
composite_folder <- shQuote(paste0("input=", dQuote(file.path(root_path, "composites"))))


system(paste(call_imagej, macro_to_run, composite_folder))

## subdivide images into training and testing set ####

images <- tibble(composites = list.files(file.path(root_path, "composites"),
                                         pattern = ".tif",
                                         full.names=TRUE),
       parent_roi = str_extract(composites, "parent_roi_.*_composite")) %>%
  mutate(parent_roi=str_remove_all(parent_roi, "parent_roi_|_composite")) 

training_dir <- file.path(root_path, "composites", "training")
test_dir <- file.path(root_path, "composites", "test")
if(!dir.exists(training_dir)){
  dir.create(training_dir)
}
if(!dir.exists(test_dir)){
  dir.create(test_dir)
}


# Get 30% of images for each group to train
training <- images %>% group_by(parent_roi) %>% sample_frac(0.3) %>%
  mutate(new_path = file.path(training_dir, basename(composites)))
# move files
file.rename(training$composites, training$new_path)

# get the ones that are not in training
testing <- anti_join(images, training) %>%
  mutate(new_path = file.path(test_dir, basename(composites)))
file.rename(testing$composites, testing$new_path)

# keep track of what happened here
partition_list <- list(training=training, testing=testing)

save(partition_list,
     file = file.path(root_path, "composites/train_test_partition.Rdata"))