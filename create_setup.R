# This function is a helper to create the setup object
# we avoid repetition
create_setup <- function(match_df_2){
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
  
  return(setup)
}