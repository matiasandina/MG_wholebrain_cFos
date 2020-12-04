

select_rect <- function(image, n, detections=NULL){
  # this little function has A LOT of dependencies....
  selecting <- TRUE
  while(selecting){
    # plot initial image
    windows()
    image %>% plot()
    
    
    usethis::ui_info(glue::glue("Select", n, "points to crate a box", .sep = " "))
    pts <- locator(n, type = "o", col="white", lwd=2)
    bbox <- pts %>% as.data.frame() %>% sp::SpatialPoints() %>% sp::bbox()
    
    bbox <- as.data.frame(apply(bbox, 2, round))
    
    xmin <- max(bbox[1,]$min, 1)
    xmax <- min(bbox[1,]$max, imager::width(img))
    ymin <- max(bbox[2,]$min, 1)
    ymax <- min(bbox[2,]$max, imager::height(img))
    
    # only good on windows for now
    windows()
    as.matrix(image)[xmin:xmax, ymin:ymax] %>% imager::as.cimg() %>% plot
    if(!is.null(detections)){
      
      #adjust detections to bbox
      detections_adj <- detections
      detections_adj$x <- detections_adj$x - xmin
      detections_adj$y <- detections_adj$y - ymin
      text(detections_adj$x - 20,
           detections_adj$y,
           labels = round(detections_adj$area),
           col = "white")
      points(detections_adj$x, detections_adj$y,
             col="red", cex=2)
      points(detections_adj$x[detections_adj$area>50],
             detections_adj$y[detections_adj$area>50],
             col="green", cex=2)
    }
    selecting <- ask_yes_no("Do you want to select another one")
    # clean stuff and start over
    while(dev.cur()>1) dev.off()
  }
  close_windows <- ask_yes_no("Do you want to close all plots")
  if(close_windows){
    while (dev.cur()>1) dev.off()
  }
}
