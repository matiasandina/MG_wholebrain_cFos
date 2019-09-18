# this function will make small images out of a directory

library(imager)

resize_pad <- function(filename, target_x = 1000, pad=NULL, pad_type="xy"){
  # load image
  img <- load.image(filename)
  
  # TODO: Check target_x is number greater than zero
  # Compute scaling factor using target_x
  scaling_factor <- target_x / dim(img)[1] 
  new_dim <- img %>% dim() * scaling_factor
  
  # Resize
  img <- img %>% resize(size_x = new_dim[1],
                        size_y = new_dim[2],
                        interpolation_type = 5)
  
  # Pad image
  if(!is.null(pad)){
    message(sprintf("Adding zero-pad of %s to %s", as.character(pad), pad_type))
    img <- img %>% pad(axes = pad_type, pad)
  }
  
  
  message(sprintf("Resizing image to %s by %s",
                  as.character(dim(img)[1]), as.character(dim(img)[2])))
  
  # create new folder on root folder
  new_folder <- file.path(dirname(dirname(filename)),
                          "small")
  if(!dir.exists(new_folder)){
    dir.create(new_folder)
  }
  
  # Generate name
  outname <- file.path(new_folder,
                       paste("small", basename(filename),
                             sep="_"))
  # Save
  imager::save.image(img, outname)
  
  message(sprintf("Saved %s", outname))
  
  return_list <- list(filename = filename,
                      outname = outname,
                      target_x = target_x,
                      pad = pad,
                      pad_type = pad_type)
  

  return(return_list)
}
