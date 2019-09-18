# this function take small images out of a directory
# It will find contours and also create a filter for dapi segementation

library(imager)

find_contours <- function(filename){
  
  # print some things
  
  message(sprintf("Processing %s", filename))
  
  # Let's do image processing first
  img <- load.image(filename)
  # stick only to first channel (tiffs might have 2)
  if(dim(img)[3] > 1){
    img <- img[, , 1, ] %>% as.cimg()
  }
  
  out <- img %>% threshold() %>%
    clean(2) %>%
    imager::fill(15)
  
  cont_list <- imager::contours(out)
  
  # polyarea calculates the area of a polygon defined by the vertices with coordinates x and y. 
  # Areas to the left of the vertices are positive, those to the right are counted negative.
  # We can wrap this function
  get_max_area <- function(cont_list){
    purrr::map(cont_list, function(li, x, y) abs(pracma::polyarea(li$x, li$y)))
  }
  
  areas <- get_max_area(cont_list)
  # get max contour
  biggest <- cont_list[[which.max(areas)]] 
  
  # We need to change the way the "level" is entered
  # registration is expecting "contour.ID" and same length
  biggest$contour.ID <- rep(biggest$level, length(biggest$x))
  
  # THIS IS A QUITE LARGE CONTOUR, we could downsample... 
  # automatic.correspondences gets slow with this one
  
  
  # Create filter for registration
  DAPI_filter<-structure(list(alim = c(50, 50),
                              threshold.range = c(50000, 60000L),
                              eccentricity = 999L,
                              Max = 5000,
                              Min = 150,
                              brain.threshold = 270L,
                              resize = 0.5,
                              blur = 7L,
                              downsample = 2,
                              biggest = biggest))
  
  return(DAPI_filter)
}