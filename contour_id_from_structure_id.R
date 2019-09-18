
# this function is a wrapper to filter contour data frame and return the 
# unique contour.ID values to be passed to the python command
# this might not be useful....se function create_roi_id_table
contour_id_from_structure_id <- function(df, structure_ids){
  if(length(structure_ids) > 0){
    return(
    filter(df, structure_id %in% structure_ids) %>%
      pull(contour.ID) %>% unique()
    )
  } else {
    return(NA)
  }
}


# we need to identify  

