## This script is meant to put files into folders according to the animal
## it will check



move_animals_to_folders <- function(){
  
  ## Feed the file pattern you are looking for
  
  file_options <- c('.czi', '.tif')
  
  file_pattern <- rChoiceDialogs::rselect.list(choices = file_options,
                                               title = "Select file type",
                                               multiple = FALSE  
                                              )
  
  # check input
  if(length(file_pattern) == 0){
    stop("must select a file type.\nIf your file extension is not available edit `file_options` in source code.")
    }
  
  # prompt for directory
  mydir <- rChoiceDialogs::rchoose.dir()
  

  # Get raw files
  all_files <- list.files(path= mydir, pattern = file_pattern)
  
  # This will give character(0) if you are in the wrong place
  all_files
  
  # make folder names
  all_animals <- unique(stringr::str_extract(all_files, "RV[0-9]+"))
  
  # Make the path
  animals_dir <- file.path(mydir, all_animals)
  
  # Create directories if needed
  lapply(animals_dir, function(x){
    ifelse(!dir.exists(x),
           dir.create(x),
           FALSE)
    
  })
  
  ## move files into 
  
  for (i in all_animals){
    
    # make a boolean of the files to get
    
    file_bool <- stringr::str_detect(string = all_files,
                                     pattern = i)
    
    # These are the files we'll move
    file_group <- all_files[file_bool]
    
    # make the new names
    new_names <- file.path(mydir, i, file_group)
    
    # Make the full.path
    file_group <- file.path(mydir, file_group)
    
    # move the files
    file.rename(from = file_group, to = new_names)
    
    message(i, " was moved to ./", i,". Done!")
    
  }
  
  
message("Function finished. Everything Done :)")   
   
} 

