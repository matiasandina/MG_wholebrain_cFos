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

# ofl stands for "order filter list"
ofl <- function(match_df_2, filter_list){
  # match all in the new order
  new_order <- sapply(match_df_2$old_names, function(x) which(basename(names(filter_list)) == x))
  
  if(is.list(new_order)){
    stop("match_df_2$old_names and names(filter_list)\ndon't match. Check manually")
  }
  
  # we need to match the filters with the images
  ordered_filter_list <- filter_list[new_order]
  
  # Check if the reordering was good    
  message("Checking the order was done properly, should return TRUE")
  print(identical(basename(names(ordered_filter_list)), match_df_2$old_names)) # should return TRUE
  
  return(ordered_filter_list)
}


# Get directory #####
# Please choose directory "001" within the animal you are trying to analyze
root_path <- choose_directory()
# get the animal ID from path
animal_id <- stringr::str_extract(root_path, "MG[0-9]+")
raw_data <- stringr::str_extract(root_path, ".+raw_data/")
# in case there's no "raw_data" folder
raw_data <- ifelse(is.na(raw_data),
                   paste0(dirname(dirname(root_path)), "/"),
                   raw_data)

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
# all_possible_parents <- sort(unique(wholebrain::acronym.from.id(wholebrain:::ontology$parent)))

# rois <- select.list(all_possible_parents,
#                           multiple = TRUE,
#                           graphics = TRUE,
#                           title = "Select Parent Brain Regions")

# get all children
# rois <- get_all_children(rois)

#rois <- c(rois, "....")

fn <- paste(animal_id, "cropping_area.xlsx", sep = "_")
filename <- file.path(root_path, fn)
filename <- "C:\\Users\\Lightsheet\\Downloads\\cropping area.xlsx"
df <- readxl::read_xlsx(filename, col_names = LETTERS[1:3])

# PVT is on the list two times
# SSp has other text that has to be removed
# ZI is written Zl

rois <- unlist(df, use.names=FALSE)
rois <- rois[complete.cases(rois)]
# remove SSp thing
# rois <- str_replace(rois, " \\(.+", "")
rois <- rois[!duplicated(rois)]
# rois <- gsub("Zl", "ZI", rois)

print(rois)

roi_frame <- rois %>% tibble::enframe() %>%
  dplyr::select(-name) %>% 
  arrange(value) %>%  
  mutate(children = purrr::map(value, get_all_children)) 

roi_frame <-
  roi_frame %>%
  mutate(is_null = sapply(children, is.null),
         children2 = ifelse(is_null, value, children),
         parent = ifelse(is_null, get.acronym.parent(value), value)) %>% 
  dplyr::select(-is_null) 

final_rois <- unlist(roi_frame$children2)



check_rois <- function(rois){
  message("Double check your Brain Regions")
  print(rois)
  message("If something missing, re-run `rois <- select.list(...)` command")
}
# check rois
check_rois(final_rois)

# Get the numeric ids to filter by structure_id
structure_ids <- id.from.acronym(final_rois)

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


# Check that there are no empty data frames -------------------------------
# it might be the case that some files are empty
# This can be due to two reasons
# 1) Your chosen brain areas are not in some plates
# 2) There were errors in the code and the left_join did not work properly
# We will check if they are empty and remove them before going on

check_empty_df <- function(csv_file_names){
  possibly_empty <- 
    sapply(csv_file_names, file.size) %>% 
    tibble::enframe() %>% 
    # check for less than 1 KB
    filter(value/1000 < 1) %>% 
    pull(name)
  
  empty_file_table <- 
    sapply(possibly_empty, function(tt) 
      nrow(readr::read_csv(tt))) %>% 
    tibble::enframe() %>% 
    dplyr::rename(n_rows = value)
  
  if (nrow(empty_file_table) > 0) {
    message("It appears like some files are empty")
    print(empty_file_table)
    prompt <- askYesNo("Do you want to delete them?")
    if (prompt){
      message("Removing file(s)...")
      file.remove(empty_file_table$name)
    } else {
      message("Files were not removed, 04-workflow.R will crash!\nFix this manually before going on!")
    }
    
  } 
}  

# actually check
check_empty_df(csv_file_names)
