# After training on Ilastik, this script will help quantify


# if need to install rhdf5
# install.packages("BiocManager")
# BiocManager::install(pkgs = "rhdf5")

# load packages
library("rhdf5")
library("tidyverse")
library("choices")

source("select_rect.R")
# contour props -----------------------------------------------------------
# helper to get the properties of the contours
contour_props <- function(cont_list){
  purrr::map(cont_list, function(li, x, y){
    area = abs(pracma::polyarea(li$x, li$y))
    centroid = pracma::poly_center(li$x, li$y)
    return(data.frame(x=centroid[1], y=centroid[2], area=area))
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




# Load Ilastik results ----------------------------------------------------

# composite folder and get the files
composite_folder <- choose_directory(title = "Select composites folder")
images <- list.files(composite_folder, recursive = T, pattern="tif", full.names = T)
h5_files <- list.files(composite_folder, recursive = T, pattern = "h5", full.names = T) 
root_path <- dirname(composite_folder)

animal_id <- str_extract(root_path, "MG[0-9]+")

file_props <- purrr::map(h5_files, h5ls) %>% bind_rows()
#explore file properties
file_props

# read exported data 
li <- purrr::map(h5_files, function(tt) h5read(tt, name="exported_data"))

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

# dilate with sq kernel of 3
fos_positive <- purrr::map(fos_positive,
                           function(tt) imager::dilate_square(
                             imager::as.cimg(tt),
                             3))

# might need to threshold first
# takes A LONG time, nlevels=1 is key to get only one contour
fos_contours <- purrr::map(fos_positive,
                           function(tt) imager::contours(imager::as.cimg(tt), nlevels=1))

# STOP POINT -----
# create directory for results
dir.create(file.path(root_path, "results"))
save_name <- file.path(root_path, "results",
                       paste(animal_id,"fos_contours.Rdata", sep="_")
                       )
save(fos_contours, file=save_name)

#readRDS(file = file.path(root_path, "results", paste(animal_id, "fos_contours", sep="_")))
#qq <- file.path(root_path, "results", paste(animal_id, "fos_contours", sep="_"))
#read_rds(path = qq)


# 
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
# grab the area of each image from file_props
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
  left_join(im_w_h, by="name") %>% 
   mutate(roi_area = rows * cols,
          cell_dens = value / roi_area) %>% 
  select(-rows, -cols, -chann)



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


# merge
cell_counts <-
cell_counts %>%
  # there shouldn't be NAs... this has to be looked into
  filter(complete.cases(.)) %>% 
  left_join(dplyr::select(match_df_2, img_source, mm.from.bregma))

cell_counts %>% 
  mutate(AP = factor(round(mm.from.bregma, 2)),
         AP = fct_rev(AP)) %>% 
  # we need to remove the things that make observations unique
  # so that we can pivot wider
  select(-roi_area) %>% 
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
readr::write_csv(cell_counts_wide, file.path(root_path, "results", paste0(animal_id, "_counts.csv")))


# Plots we want -----------------------------------------------------------

# Variables
# * AP: many
# * Treatment: CD3(4), Saline (4), Saline(2), IL17-ICV(3)
# * Genotype: WT, MIA, CNTNAP2, FMR1

# Select representative AP levels (+- 0.5 mm) and average within roi and side
# Then average between animals of same genotype~treatment for same AP and ROI 
# Then dot plot
# color code by Treatment, not left/right
# facet_grid(genotype ~ AP)


