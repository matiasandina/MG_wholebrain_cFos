library(tidyverse)
## Animals by stage
# this script will give a description of what animals have to go where they are in the pipeline

## 01-workflow ####

raw_data <- "/media/choilab/Elements/Axio Scan/raw_data/"

## External hard drives have the same folder structure
# TODO: add identifier to know what disk are we scanning
# we can implement something like a command line call
# the problem is that it's difficult with /dev/sdb or /dev/sdc or whatever
# sudo smartctl -i /dev/sdb | grep -i serial

# Maybe we store the device info in a txt and we attempt to read it!
# Ask in stack overflow! 

# helper to find disk
external_disk_id <- function(){
  # this tries 2 disks...might not work if pen drives and other stuff going on
  cmd_command <- 'sudo -kS smartctl -i /dev/sd%s | grep -i serial'
  cmd_command <- sprintf(cmd_command, letters[1:2])
  a <- lapply(cmd_command, function(x) system(x, input=readline("Enter sudo password: "), intern = TRUE))
  # they have 3 spaces between "Serial Number:" and the actual number  
  a <- str_remove(unlist(a), "Serial Number:    ")
  return(a)
}

# we will be using the second one 
external_hd <- external_disk_id()[2]

animals <- list.files(raw_data)

li <- list()
for(animal in animals){
  files <- list.files(file.path(raw_data, animal))
  tt <- stringr::str_detect(files, ".czi")
  if(sum(as.numeric(tt)) == length(tt)){
    li[animal] <- animal
  }
}

print("The following animals need to be run starting on 01-workflow.R")
print("-----------------------------")
to_run <- unlist(li)
if (is.null(to_run)) print("All animals completed this stage!") else print(to_run)
rm(li)
print("-----------------------------")




## 02-workflow ####

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
  # assign a TODO column
  # we assume we always finish an animal manual regis before jumping to another one
  mutate(TODO = 
           case_when(manual_regis > 0 ~ "Crop ROIs",
                     !(animal %in% to_run) & manual_regis == 0 & regis == TRUE ~ "Do manual regis",
                     manual_regis == 0 & flag == TRUE ~ "Do auto regis",
                     !(animal %in% to_run) & mdf == FALSE ~ "02-workflow.R",
                     animal %in% to_run ~ "01-workflow.R")
         
         )


# mutate levels for factor

animal_df <- animal_df %>%
  mutate(TODO = factor(TODO,
                       levels = c("01-workflow.R", "02-workflow.R", 
                                  "Do auto regis", 
                                  "Do manual regis",
                                  "Crop ROIs")))


animal_df <- arrange(animal_df, TODO) %>%
  mutate(x = as.numeric(TODO)) %>%
  group_by(x) %>% mutate(y = 1:length(x))

ggplot(animal_df, aes(TODO, y, color=TODO)) +
  # don't drop if one level has zero
  scale_x_discrete(drop=FALSE)+
  geom_text(aes(label = animal))+
  theme(legend.position="none")+
  labs(x="", y="number of animals",
       title = paste("hard drive id:", external_hd))
