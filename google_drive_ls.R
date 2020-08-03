#devtools::install_github("tidyverse/googledrive")
library(tidyverse)
library(googledrive)
t1 <- Sys.time()
animal_folders <- drive_ls("MG_cFos", recursive = FALSE)
Sys.time() - t1
# ~ 30 sec

t1 <- Sys.time()
#animal <- animal_folders[1, "id"]

process_animal <- function(animal_folders, animal){
  
  target_folder <- filter(animal_folders, name == animal)
  
  full_ls <- drive_ls(path = as_id(target_folder), recursive = TRUE)
  
  patterns <- list(
    # we look for a folder named c0, one ofe the products of workflow-01.R
    c0 = filter(full_ls, name == "c0"),
    # if it doesn't have the small folder it means no workflow-02.R
    small = filter(full_ls, str_detect(name, "^small$")),
    match_df = filter(full_ls, str_detect(name, "ordered_atlas_img_path")),
    autoregis = filter(full_ls, str_detect(name, "_regis.Rdata")),
    manualregis = filter(full_ls, str_detect(name, "Registrations_manual"))
  )
  
  # bind_rows will drop empty data frames (maybe we can complete)
  out <- patterns %>% bind_rows(.id = "stage") %>%
    mutate(animal_id = animal)
  #%>%  complete(stage = names(patterns))
  
  if(nrow(out) == 0) {
   # create something that basically says "go to 01-workflow.R" 
   out <- tibble(stage = "c0",
                 name="c0",
                 id = NA_character_,
                 drive_resource = list(),
                 animal_id = animal)  
  }
  return(out)
}

process_animal_slow <- purrr::safely(
  purrr::slowly(process_animal,
                rate_delay(100)))

t1 <- Sys.time()
#purrr::map(animal_folders$name,
#           function(tt) process_animal_slow(animal_folders, animal = tt)) %>%
#  bind_rows() -> big_df
li <- list()
for (i in animal_folders$name){
  print(paste("trying animal", i))
  li[[i]] <- process_animal_slow(animal_folders, i)
  
}
Sys.time() - t1

# we create a priority table for binding
priority_table <- data.frame(
  priorities  = 1:5,
  stage = c("manualregis", "autoregis", "match_df", "small", "c0"),
  TODO  = c("Crop ROIs", "Do manual regis", "Do auto regis", "02-workflow.R", "01-workflow.R")
)

big_df <- map(li, "result") %>% bind_rows()


# manualregis folder might exist but be empty
# mutate way gives an error with path more than one file ?!
#big_df <- big_df %>% 
#  mutate(empty = ifelse(stage == "manualregis",
#                        nrow(drive_ls(as_id(id))),
#                        NA))

big_df$empty <- NA
for(i in 1:nrow(big_df)){
  if (big_df$stage[i] == "manualregis"){
    print(big_df$animal_id[i])
    big_df$empty[i] <- nrow(drive_ls(as_id(big_df$id[i])))
  }

}

# remove those with manualregis folder but no files inside
big_df <- big_df %>%  filter(is.na(empty) | empty != 0)


big_df %>%
  dplyr::select(-id, -drive_resource) %>%
  left_join(priority_table) %>%
  arrange(animal_id, priorities) %>%
  group_by(animal_id) %>%
  slice(1) %>%
  #select(-prio) %>%
  ungroup() -> animal_stages

# assign correct levels
animal_stages <- animal_stages %>%
  mutate(TODO = factor(TODO,
                     levels = c("01-workflow.R", "02-workflow.R", 
                                "Do auto regis", 
                                "Do manual regis",
                                "Crop ROIs"))) %>%
  arrange(stage, TODO) %>%
  mutate(x = as.numeric(TODO)) %>%
  group_by(x) %>% mutate(y = 1:length(x))

ggplot(animal_stages, aes(TODO, y, color=TODO)) +
  # don't drop if one level has zero
  scale_x_discrete(drop=FALSE)+
  geom_text(aes(label = animal_id))+
  theme(legend.position="none")+
  labs(x="", y="number of animals")


Sys.time() - t1

# =================================================

# look for animals to upload
# upload not working well right now so don't run

all_animals <- 
animal_folders %>%
full_join(data.frame(name=list.files("/media/choilab/Elements/Axio Scan/raw_data/"),
                                     source1="2tb",
                                     stringsAsFactors = FALSE)) %>%
  full_join(data.frame(name = list.files("/media/choilab/TOSHIBA EXT/Axio Scan/"),
                       source2 = "toshiba",
                       stringsAsFactors = FALSE)) 

# all the animals we want to upload are in the Toshiba drive
all_animals %>%
  filter(is.na(id), !str_detect(name, "-")) %>%
  pull(name) -> animals_to_upload

animals_to_upload <- sapply(
  animals_to_upload, function(tt) list.files("/media/choilab/TOSHIBA EXT/Axio Scan/", pattern=tt, full.names = TRUE)
)

# Currently not working awesome...
#drive_folder <- as_dribble("MG_cFos")
#drive_upload(animals_to_upload[1], path=drive_folder, name=names(animals_to_upload[1]))

