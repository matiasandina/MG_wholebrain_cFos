# After training on Ilastik, this script will help quantify


# if need to install rhdf5
# install.packages("BiocManager")
# BiocManager::install(pkgs = "rhdf5")

# load packages
library("rhdf5")
library("tidyverse")
library("choices")
library("progress")

source("select_rect.R")
# contour props -----------------------------------------------------------
# helper to get the properties of the contours
contour_props <- function(cont_list){
  if (is_empty(cont_list)){
    return(data.frame(x=NA, y=NA, area=0))
  }
  purrr::map(cont_list, function(li, x, y){
    area <- abs(pracma::polyarea(li$x, li$y))
    centroid <- pracma::poly_center(li$x, li$y)
    out <- data.frame(x = centroid[1], 
                     y = centroid[2], 
                     area = area)
  
    return(out)
  }
  )
}

# dot.plot helper for plots -----------------------------------------------
dot.plot <- function(df, animal_id, mode="value", folder = NULL){
  # TODO: add value_left value_right cell_dens_left cell_dens_right
  
  if(mode=="value"){
    df <- select(df, -contains("cell_dens")) %>% 
      rename(left = value_left,
             right = value_right)
    y_label <- "Cell Counts"
  } else {
    df <- select(df, -contains("value")) %>% 
      rename(left = cell_dens_left,
             right = cell_dens_right)
    y_label <- "Cell Density"
  }
  
  p <- df %>%   
    mutate(parent = fct_reorder(factor(parent), max_count, .desc = T)) %>% 
    ggplot(aes(x=parent))+
    geom_segment(aes(xend=parent, y=left, yend=right))+
    geom_point(aes(y = left, color="left"))+ 
    geom_point(aes(y=right, color="right"))+ 
    coord_flip()+
    scale_color_manual(values=c("orange", "black"))+
    facet_wrap(~AP, scales ="free_y", labeller = label_both)+
    #facet_grid(genotype~AP, scales="free", labeller = label_both)+
    labs(x="", y=y_label, color="Side")+
    theme_bw()
  #scale_y_log10()+
  #annotation_logticks(sides = "bottom")
  fn <- paste0(animal_id, "_", unique(df$AP), "_dot_plot.png")
  if(is.null(folder)) {
    folder <- getwd()
  }
  fn <- file.path(folder, fn)
  ggsave(fn, p)
}


# Check contour length ----------------------------------------------------

check_contour_length <- function(fos_contours){
  for (crop in 1:length(fos_contours)){
    if(length(fos_contours[[crop]]) > 0) {
      for (detection in 1:length(fos_contours[[crop]])){
        x_len <- length(fos_contours[[crop]][[detection]]$x)
        y_len <- length(fos_contours[[crop]][[detection]]$y)
        
        same_length <- identical(x_len, y_len)
        if(same_length == FALSE | x_len < 3) {
          message(sprintf("Crop %s detection %s has unequal length or length < 3. See below:", crop, detection))
          print(fos_contours[[crop]][[detection]])
          # remove
          fos_contours[[crop]][[detection]] <- NULL
          message("This was fixed by removing that detection to prevent further errors.")
          # stop()
        }
      }
    }

  }
  usethis::ui_done("Contours have same length on xy and `length(contour) > 3`\nProceed with pipeline")
  return(fos_contours)
}



# find biggest contour on image for calculating ROI area ------------------
# this comes from the wholebrain pipeline but has been modified
find_contours <- function(filename){
  
  # print some things
  message(sprintf("Processing %s", filename))
  
  # Let's do image processing first
  img <- imager::load.image(filename)
  # stick only to first channel (tiffs might have 2)
  if(dim(img)[3] > 1){
    img <- img[, , 1, ] %>% imager::as.cimg()
  }
  
  pad_size <- 50
  
  out <- img %>%
    # very permisive thresholding!
    imager::threshold(0.8) %>%
    imager::clean(2) %>% 
    imager::pad(nPix = pad_size, axes = "xy")
  
  
  #plot(out)
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
  
  # adjust the pad size/2 because pad on both xy sides
  biggest$x <- biggest$x - pad_size/2
  biggest$y <- biggest$y - pad_size/2
  biggest$area <- areas[[which.max(areas)]] %>% unlist()
    
  return(biggest)
}

# this function is used to split a vector into chunks
# we will process our information in batches
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

# process data function ----
process_data <- function(h5_files){
  n_batches <- min(c(10, length(h5_files)))
  batches <- chunk2(h5_files, n=n_batches)
  # pb bar
  pb <- progress_bar$new(
    format = "  processing batch [:bar] :percent eta: :eta",
    total = n_batches, clear = FALSE, width= 60)
  
  results <- list()
  # We do this in a for loop to be able to discard intermediate results
  for (batch in batches){
    # read exported data 
    li <- purrr::map(batch, function(tt) h5read(tt, name="exported_data"))
    
    # get the fos on channel 2
    fos_plane <- purrr::map(li,
                            function(tt) tt[,,2])
    # remove li to free memory (several Gb !!)
    # and do garbage collection
    rm(li); gc() 
    
    # threshold for probability of positive case 
    threshold_prob <- 0.6
    fos_positive <- purrr::map(fos_plane,
                               function(tt) ifelse(tt < threshold_prob, 0, 1))
    
    # and do garbage collection
    rm(fos_plane); gc() 
    
    # dilate with sq kernel of 3
    fos_positive <- purrr::map(fos_positive,
                               function(tt) imager::dilate_square(
                                 imager::as.cimg(tt),
                                 3))
    
    # might need to threshold first
    # takes A LONG time, nlevels=1 is key to get only one contour
    fos_contours <- purrr::map(fos_positive,
                               function(tt) imager::contours(imager::as.cimg(tt), nlevels=1))
    
    # get it out onto the result list
    results <- append(results, fos_contours)
    pb$tick()
  }
  
  return(results)
}


# check for previous data -------------------------------------------------

load_previous_Rdata <- function(save_name, object_name) {
  
  # check the object names
  if(object_name %in% c("roi_area_list", "fos_contours") == FALSE){
    stop("`object_name` must be either 'roi_area_list' or 'fos_contours'")
  }
  # check you use proper pair of save_name and object name
  if(str_detect(save_name, object_name) == FALSE){
    usethis::ui_stop(c(
      "Provided objects don't match",
      "check that `object_name` is the object stored at the path of `save_name`",
      object_name,
      save_name
    ))
  }

  if(object_name %in% ls() == FALSE) {
    tryCatch(
      {
        suppressMessages(load(save_name, envir = .GlobalEnv))
        usethis::ui_done(c(paste(object_name, "retrieved from previous run!"),
                           "Skip to next section"))
      },
      error=function(e) usethis::ui_warn(c(paste(object_name, "might not exist"),
                                                   "run following code section to create it")) 
    )
  }
}


# Load Ilastik results ----------------------------------------------------

# composite folder and get the files
composite_folder <- choose_directory(title = "Select composites folder")
images <- list.files(composite_folder, recursive = T, pattern="tif", full.names = T)
h5_files <- list.files(composite_folder, recursive = T, pattern = "h5", full.names = T) 
if (length(images) != length(h5_files)){
  usethis::ui_stop("`length(images)` is different from `length(h5_files)`. Check input!")
}
root_path <- dirname(composite_folder)

animal_id <- str_extract(root_path, "MG[0-9]+")

file_props <- purrr::map(h5_files, h5ls) %>% bind_rows()
#explore file properties
file_props

# Get ROI area ----
# get the areas for all images
save_name <- file.path(root_path, "results",
                       paste(animal_id,"roi_area_list.Rdata", sep="_")
)

# try to load
load_previous_Rdata(save_name, "roi_area_list")
# if loaded properly, skip next 4 lines
options("max.contour.segments"= 300000)
roi_area_list <- map(images, find_contours)
names(roi_area_list) <- images
dir.create(file.path(root_path, "results"))
save(roi_area_list, file=save_name)

# continue here
roi_areas <- map(roi_area_list, "area") %>%
  enframe() %>%
  unnest(value) %>%
  rename(area=value)
# check that the areas were properly calculated
map(images[1:5], 
    function(tt){
      imager::load.image(tt) %>% imager::as.cimg() %>% 
        imager::frame(1) %>% plot
      lines(roi_area_list[[tt]]$x,
            roi_area_list[[tt]]$y,
            col="yellow")
    }
    )


# fos contours ------------------------------------------------------------
save_name <- file.path(root_path, "results",
                       paste(animal_id,"fos_contours.Rdata", sep="_")
)

load_previous_Rdata(save_name, "fos_contours")
# if loaded properly, next 3 lines
fos_contours <- process_data(h5_files)
# check contour length, sometimes detections have contour length < 3
# that will create errors downstream
fos_contours <- check_contour_length(fos_contours)
# STOP POINT -----
save(fos_contours, file=save_name)

# Calculate the areas and centroids ----
fos_areas <- purrr::map(fos_contours, 
                          function(tt) contour_props(tt))

# bind the list of lists within each image
fos_areas <- purrr::map(fos_areas, function(tt) bind_rows(tt))


# Inspection of detections ------------------------------------------------
# second channel
par(mar = c(0.1,0.1,0.1,0.1))
# modify here to check different amount of images
plots_to_check <- 5
# we do random sampling
# do not run again if you want to check same section!
fos_samples <- sample(length(images), plots_to_check)
for(i in fos_samples){
  # the actual image
  img <- images[i] %>% imager::load.image() %>% imager::frame(2) 
  select_rect(img, n=4, detections = fos_areas[[i]])
  #fos_positive[[i]] %>% imager::as.cimg() %>% plot()
  #text(fos_areas[[i]]$x - 50,
  #     fos_areas[[i]]$y,
  #     labels = round(fos_areas[[i]]$area),
  #     col = "white")
  #points(fos_areas[[i]]$x, fos_areas[[i]]$y, col="red")
  #points(fos_areas[[i]]$x[fos_areas[[i]]$area>25],
  #       fos_areas[[i]]$y[fos_areas[[i]]$area>25],
  #       col="green")
  
}

#purrr::walk(fos_contours[[1]][1:1000],function(v) lines(v$x,
#                                 v$y,
#                                 col="red"))



#windows()
#fos_positive[[1]][1:200,1:800] %>% imager::as.cimg() %>% plot()
#purrr::walk(fos_contours[[1]][1:1000],function(v) lines(v$x,
#                                                        v$y,
#                                                        col="red"))


# Count the cells -----
area_threshold <- 50
names(fos_areas) <- images
cell_counts <- sapply(fos_areas,
                      function(tt) tt %>% filter(area>area_threshold) %>% nrow()) 

# we need a measure of density not only counts!
# grab image props from file_props
file_props %>%
  select(dim) %>%
  separate(dim, into=c("rows","cols", "chann"),
           sep = " x ") %>%
  mutate_all(as.numeric) %>% 
  mutate(name = images) %>% 
  select(name, everything()) -> im_w_h

cell_counts <-   
cell_counts %>% 
  enframe() %>% 
  mutate(parent = str_extract(name, "parent_roi_.+_"),
         parent = str_remove_all(parent, "parent_roi|_"),
         side = str_extract(name, pattern="left|right"))

# bind with the image width and height info
cell_counts <- 
cell_counts %>%
  # here we get image props
  #left_join(im_w_h, by="name") %>% 
  # here we get the actual areas from contour
  left_join(roi_areas, by="name") %>% 
   mutate(cell_dens = value / area) 


# Get the AP coordinates from match_df_2 ----------------------------------

match_df_2 <- readRDS(file.path(root_path, "ordered_atlas_img_path_df"))

match_df_2 <- 
match_df_2 %>% 
  mutate(img_source = str_remove(old_names, pattern='_c0.tif'),
         img_source = str_remove(img_source, "small_"))

cell_counts <-
cell_counts %>% 
  mutate(img_source = str_extract(name, "MG[0-9]+-slide.+_"),
         img_source = str_remove(img_source, "_left.+|_right.+")) 


if(sum(complete.cases(cell_counts)) != nrow(cell_counts)){
  usethis::ui_warn("cell_counts contains `NA`s!")
} else {
  usethis::ui_done("Data looks complete :)")
}

# merge
cell_counts <-
cell_counts %>%
  # there shouldn't be NAs... this has to be looked into
  filter(complete.cases(.)) %>% 
  left_join(dplyr::select(match_df_2, img_source, mm.from.bregma))

# save cell counts
readr::write_csv(cell_counts,
                 file.path(root_path, "results", paste0(animal_id, "_counts.csv")))


cell_counts %>% 
  mutate(AP = factor(round(mm.from.bregma, 2)),
         AP = fct_rev(AP)) %>% 
  # we need to remove the things that make observations unique
  # so that we can pivot wider
  select(-area) %>% 
  pivot_wider(-name, names_from = side,
              values_from = c(value, cell_dens)) %>% 
  # create a max count 
  group_by(img_source, parent) %>% 
  mutate(max_count = max(value_left, value_right),
         max_density = max(cell_dens_left, cell_dens_right)) %>% 
  ungroup() -> 
  cell_counts_wide


split(cell_counts_wide, cell_counts_wide$img_source) %>% 
  purrr::map(dot.plot, animal_id, folder=file.path(root_path, "results"))

# Write the table to export as csv
readr::write_csv(cell_counts_wide, file.path(root_path, "results", paste0(animal_id, "_counts_wide.csv")))




