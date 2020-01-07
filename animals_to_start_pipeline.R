
raw_data <- "/media/mike/Elements/Axio Scan/raw_data/"

animals <- list.files(raw_data)

li <- list()
for(animal in animals){
  files <- list.files(file.path(raw_data, animal))
  tt <- stringr::str_detect(files, ".czi")
  if(sum(as.numeric(tt)) == length(tt)){
    li[animal] <- animal
  }
}

to_run <- unlist(li)
rm(li)