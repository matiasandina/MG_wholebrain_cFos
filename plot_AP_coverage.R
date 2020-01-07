library(tidyverse)

plot_AP_coverage <- function(raw_data){
  # enter raw data folder
  file_list <- list.files(raw_data, pattern="ordered_atlas_img_path_df",
                          recursive=TRUE, full.names = TRUE)
  
  animals <- stringr::str_extract(file_list, "MG[0-9]+")
  
  li <- purrr::map(file_list,
                   function(x) readRDS(x))
  names(li) <- animals
  
  df <- bind_rows(li, .id = "animal")
  
  coverage <- df %>% group_by(mm.from.bregma) %>%
    count()
  
  p1 <- ggplot(df, aes(mm.from.bregma, animal)) +
    # useful but really ugly
    # geom_vline(aes(xintercept = mm.from.bregma), lty=2, col="red")+
    geom_point(size=2)+
    scale_x_reverse(breaks=seq(-5, 3, 0.5))+
    cowplot::theme_minimal_vgrid()+ xlab("")
  
  p2 <- ggplot(coverage, aes(mm.from.bregma, n))+
    geom_line()+
    scale_x_reverse(breaks=seq(-5, 3, 0.5))+
    cowplot::theme_cowplot()

  cowplot::plot_grid(p1, p2, nrow=2, align="hv", rel_heights = c(3,1))
  
}