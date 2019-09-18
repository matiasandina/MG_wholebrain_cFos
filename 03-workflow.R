# Make the croppings

# Get the acronyms #####
load(file.path(.libPaths()[1],"wholebrain","data","ontology.RData"))


# Get the files...they are supposedly in order
# TODO: can we MAKE SURE instead of 'supposedly' ?

df_paths <- list.files(file.path(root_path, "contours"), full.names = TRUE)
c0_files <- list.files(file.path(root_path, "c0"), full.names = TRUE)
c1_files <- list.files(file.path(root_path, "c1"), full.names = TRUE)

python_command <- "/home/mike/miniconda3/bin/python read_large.py -image_filename" 



write.csv(df, "raw_data/MG952/001/001/contour_key.csv", row.names = FALSE)


## Call python
# We have to reproduce this call in a Sys.call()
python read_large.py -img_filename "/media/mike/Elements/Axio Scan/raw_data/MG952/001/2019_08_19__0001_11.tif" -df_path "/media/mike/Elements/Axio Scan/raw_data/MG952/001/small_2019_08_19__0001_11.csv" -display_crop True -display_contours False

# We run this for channel 0 images
for(i in 1:length(c0_files)){
  # c0 command
  paste(python_command, c0_files[i], 
        '-df_path', df_paths[i],
        pass all the k values and fix the original code to accept multiple values
        "-display_crop False -display_contours False")  
  # c1 command
  paste(python_command, c1_files[i],
        '-df_path', df_paths[i],
        "-display_crop False -display_contours False")
}


system()


# We run this for channel 1 images