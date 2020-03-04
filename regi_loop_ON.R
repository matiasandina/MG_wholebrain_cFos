## Regi loop overnight (or to run as a Job in the job console)
library(wholebrain)
library(SMART)
library(dplyr)

# CAUTION!
# hard coded path
setwd("/home/choilab/MG_wholebrain_cFos")
source("registration_MLA.R")

# helper ro fix the working environment
fix_working_environment <- function(saved_path, local_path){
  # this will explode if the folder structure doesn't work as expected...
  stringr::str_replace(saved_path, ".+raw_data/", local_path)
}

# CAUTION!
# hard coded path
raw_data <- "/media/choilab/Elements/Axio Scan/raw_data/"

animals <- list.files(raw_data)

# remove MG952 becuase it has this weird 001/001 folder thing
animals <- animals[!animals=="MG952"]

# check whether we have match_df_2   
mdf <- purrr::map(animals,
function(animal){
  root_path <- file.path(raw_data, animal, "001")
  data.frame(
  animal=animal,
  mdf=file.exists(file.path(root_path, "ordered_atlas_img_path_df")),
  stringsAsFactors = FALSE
  )
}
)

# check whether we have a regis or not 
r <- purrr::map(animals,
  function(animal){
      data.frame(
      animal=animal,
      regis=as.logical(file.exists(file.path(paste0(raw_data, animal), "001", "R_data",
               paste0(animal, "_regis.Rdata")))),
      manual_regis = length(list.files(file.path(paste0(raw_data, animal), "001", "Registrations_manual"))),
      stringsAsFactors = FALSE
    )
  }
)

mdf <- bind_rows(mdf)
r <- bind_rows(r)

animal_df <- left_join(mdf, r, by = "animal") %>%
  # if we have the match_df but not the regis object, do registration
  mutate(flag = ifelse(as.numeric(mdf) == 1 & as.numeric(regis) == 0, TRUE, FALSE)) %>%
  # filter the animals
  filter(flag == TRUE)


for(animal in animal_df$animal){
  # make the path again
  root_path <- file.path(raw_data, animal, "001")
  
  # Read data #### 
  
  match_df_2 <- readRDS(file.path(root_path, "ordered_atlas_img_path_df"))
  
  filter_list <- readRDS(file = file.path(root_path, 'small', "filter_list"))
  
  # match all in the new order
  new_order <- sapply(match_df_2$old_names, function(x) which(basename(names(filter_list)) == x))
  
  # we need to match the filters with the images
  ordered_filter_list <- filter_list[new_order]
  
  # Create setup object ######
  ## From here, we can get our setup values
  
  setup <- list()
  # First AP
  setup$first_AP <- first(match_df_2$mm.from.bregma)
  # Last AP
  setup$last_AP <- last(match_df_2$mm.from.bregma)
  # First Z value
  # not truly needed if partial instead of wholebrain?
  setup$first_z <- NULL 
  # Last Z value
  # not truly needed if partial instead of wholebrain?
  setup$last_z <- NULL
  
  setup$regi_AP <- match_df_2$mm.from.bregma 
  setup$regi_z <- stringr::str_extract(match_df_2$image_file, pattern = "Z[0-9]+")  
  # replace zeros and Z
  setup$regi_z <- as.numeric(stringr::str_remove(setup$regi_z, pattern = "Z0+"))
  
  setup$image_paths <- NULL
  setup$regi_channel <- file.path(root_path, "small")
  # I WANT TO ONLY DO REGISTRATION ON DAPI CHANNEL...
  # REPEATING SEG CHANNEL SO IT DOES NOT BREAK, DOES IT TAKE LONGER !?!?!
  setup$seg_channel <- setup$regi_channel
  
  # im_sort(setup) ## im_sot not working
  # doing it manually
  setup$image_paths$regi_paths <- fix_working_environment(match_df_2$image_file, raw_data) 
  setup$image_paths$seg_paths <-  fix_working_environment(match_df_2$image_file, raw_data)
  
  # output folder for registration files...
  setup$output <- root_path
  setup <- get_savepaths(setup)
  
  regi_loop(setup, ordered_filter_list, autoloop = TRUE, brightness = 40)
  
  # save regis object ####
  # "/media/mike/Elements/Axio Scan/raw_data/MG952/001/001/R_data/MG952_regis.RData"
  new_name <- paste0(animal, "_regis.Rdata")
  save(regis, file = file.path(root_path, 'R_data', new_name))
  
  # delete all previous objects
  rm(filter_list)
  rm(ordered_filter_list)
  rm(match_df_2)
  # regis gets assigned to the global environment
  rm(regis, envir = .GlobalEnv)
  rm(setup)
  rm(root_path)
  
}  
 
  
