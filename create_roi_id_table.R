# This is a functional tool to get the acronyms and parents of structures
# df is expecting to find a column named contour.ID
# this column is basically a contour identifier of the sort 1:n
# see 02-workflow.R to know how it gets created 
# this function also allows to filter by providing structure_ids at the end

create_roi_id_table <- function(df, structure_ids=NULL){
  
  # load ontology
  if(!"x" %in% ls(envir = .GlobalEnv)){
  load(file.path(.libPaths()[1],"wholebrain","data","ontology.RData"),
       envir = .GlobalEnv)
  }
  
  # get the distinct combinations
  output <- distinct(df, contour.ID, structure_id) %>%
    # get the proper identifiers
    mutate(
      acronym = acronym.from.id(structure_id), 
      # this will throw a warning
      parent  = suppressWarnings(get.acronym.parent(acronym)))
  
  
  if(is.null(structure_ids)){
    # do no nothing (no need to filter)
  } else {
    # we need to filter
    output <- output %>% filter(structure_id %in% structure_ids)
  }
  return(output)
  
}

