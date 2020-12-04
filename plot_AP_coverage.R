library(tidyverse)
library(cowplot)

plot_AP_coverage <- function(raw_data, remove_y = FALSE){
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
    geom_point(size=1)+
    scale_x_reverse(breaks=seq(-5, 3, 0.5))+
    cowplot::theme_minimal_vgrid()+ xlab("")
  
  if (remove_y){
    p1 <- p1 +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  }
  
  p2 <- ggplot(coverage, aes(mm.from.bregma, n))+
    geom_area()+
    scale_x_reverse(breaks=seq(-5, 3, 0.5))+
    cowplot::theme_cowplot()
  
  
  title <- ggdraw() +
    draw_label(
      "AP Coverage",
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  subtitle <- ggdraw() +
    draw_label(
      "Each row is one animal. Each dot is a section for a given AP level",
      x = 0, size = 9, 
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )
  
  main <- cowplot::plot_grid(title, subtitle, nrow=2)
  
  cowplot::plot_grid(
    main,
    p1,  p2, nrow=3,
                     axis = "t", align="hv",
                     rel_heights = c(0.4, 2,1))
  
}