# step 3
# This script will prepare the folders for cropping (step 4)
# To do that, we need 2 things
# 1) we have to decide what brain regions we are going to use
# 2) match regions with whatever is present on regi file
# Import libraries #####
library(wholebrain)
library(SMART)
library(dplyr)
library(choices)

source("create_roi_id_table.R") # there are some functions here

# Get directory #####
# Please choose directory "001" within the animal you are trying to analyze
root_path <- choose_directory()
# get the animal ID from path
animal_id <- stringr::str_extract(root_path, "MG[0-9]+")
raw_data <- stringr::str_extract(root_path, ".+raw_data/")


# Get contours ####
# If returning on a new session, we need to read things from before
filter_list <- readRDS(file = file.path(root_path, 'small', "filter_list"))
match_df_2 <- readRDS(file.path(root_path, "ordered_atlas_img_path_df"))
ordered_filter_list <- ofl(match_df_2, filter_list)
# load regi
previous_regi <- list.files(file.path(root_path, "R_data"), full.names = TRUE)
load(file = previous_regi)


# grab all the info from the regis object
contour_list <- prep_data(regis, ordered_filter_list)

# you can visually inspect with
# gg_brain(contour_list, grayscale = TRUE)


# TAG contour_lists with metadata #####

# TODO: CAUTION! THIS IS A HACKY WAY OF BINDING DATA! 
# to match with the names we will need to create the 'order' column
# EPSatlas$plate.info has the `order` column going from 0 to legth - 1 of our data
# to match them, we shift the contour.id (order = contour.id - 1)
contour_list <- purrr::map(.x = contour_list,
                           .f = function(tt)
                             mutate(tt, order = as.factor(as.numeric(contour.ID) - 1)))


all_plates <- platereturn(match_df_2$mm.from.bregma)
# this helps keeping the names after the next purrr command
names(all_plates) <- names(contour_list)

# load atlas & ontology
load(file.path(.libPaths()[1],"wholebrain","data","EPSatlas.RData"))


contour_list <- purrr::map(seq_along(all_plates), 
                           function(tt){
                             # tag the list with the specific contour
                             mutate(contour_list[[tt]], plate = all_plates[tt]) %>%
                               # mind the atlas being subsetted at all_plates[tt] (with tt the index of all_plates)
                               left_join(EPSatlas$plate.info[[all_plates[tt]]],
                                         by="order")
                           }
)

# These warnings might appear, it's fine
#Warning messages:
#  1: Column `order` joining factors with different levels, coercing to character vector

# Get the acronmys #### 

# We need to translate from acronym to structure_id
# We have to choose the parents of the structures we care about and use a recursive get children
# This comes from SMART package
all_possible_parents <- sort(unique(wholebrain::acronym.from.id(wholebrain:::ontology$parent)))

rois <- select.list(all_possible_parents,
                           multiple = TRUE,
                           graphics = TRUE,
                           title = "Select Parent Brain Regions")
check_rois <- function(){
  message("Double check your Brain Regions")
  print(rois)
  message("If something missing, re-run `rois <- select.list(...)` command")
}
# check rois
check_rois()

# Get the numeric ids to filter by structure_id
structure_ids <- id.from.acronym(rois)

contour_list <- contour_list %>%
  purrr:::map(
    function(tt)
      left_join(tt, create_roi_id_table(tt), by=c("contour.ID", "structure_id")) %>%
      filter(structure_id %in% structure_ids) %>%
      mutate(parent_roi_number = as.numeric(as.factor(parent)))
  )

# we have to rename using the previously held names
names(contour_list) <- names(all_plates)

# Save contours to .csv files ####
if(!dir.exists(file.path(root_path, "contours"))){
  dir.create(file.path(root_path, "contours"))
}

# do some cleaning of the names
csv_file_names <- tools::file_path_sans_ext(basename(names(contour_list)))
csv_file_names <- stringr::str_remove(csv_file_names, "small_")
csv_file_names <- stringr::str_replace(csv_file_names, "_c0", ".csv")

# make the paths
csv_file_names <- file.path(root_path, "contours", csv_file_names) 

purrr::map(1:length(csv_file_names), function(tt){
  write.csv(x = contour_list[[tt]],
            file = csv_file_names[[tt]],
            row.names=FALSE)
})
