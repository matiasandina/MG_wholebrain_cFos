## ZEN puts extra annoying names to folders and exported files 
## This script removes that 

## It still needs quite a bit of changing directories and running several times
## Don't be afraid, you will not loose data by renaming.  
## Worse case scenario, the file.rename(...) returns FALSE

library(rChoiceDialogs)
library(stringr)

# Get directory
dir_to_clean <- jchoose.dir(caption="Choose dir to clean names")

# Get folders recursive=FALSE!

my_folderlist <- list.files(dir_to_clean,
                          full.names = TRUE)

# Get the files within folders recursive=TRUE!

my_filelist <- list.files(dir_to_clean,
                          recursive = TRUE,
                          full.names = TRUE)



# Look for the pattern Image Export

folders_to_rename <- my_folderlist[grepl(pattern = "Image Export",
                                       x=my_folderlist)]

files_to_rename <- my_filelist[grepl(pattern = "Image Export",
                                     x=my_filelist)]

# Check if files_to_rename is empty, should return FALSE
is.character(files_to_rename) && length(files_to_rename)==0


# Replace the "-Image Export-[0-9]+" pattern

new_folder_names <- str_replace_all("-Image Export-[0-9]+",
                                    string = folders_to_rename,
                                    replacement = "")


new_file_names <- str_replace_all("-Image Export-[0-9]+",
                              string = files_to_rename,
                              replacement = "")

# first the folders, then the files
file.rename(folders_to_rename, new_folder_names)

# If everything gave TRUE, do it for the files
file.rename(files_to_rename, new_file_names)