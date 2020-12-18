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

library(tidyverse)


# Match the animals we want -----------------------------------------------
animal_key <- read_csv("animal_key_list.csv")
# we assume getwd() is MG_cFos
animals_folder <- dirname(getwd()) 
animals <- list.dirs(animals_folder, recursive = FALSE)
target_animals <- str_detect(animals, paste(animal_key$animal_id, collapse = "|"))
target_animals <- animals[target_animals]  


# make the factor levels for plots
animal_key <- animal_key %>% 
  mutate(genotype = factor(genotype, levels=c("WT", "Cntnap2")),
         treatment = factor(treatment, levels = c("saline", "CD3")))


# Get the available cell counts -------------------------------------------
counts_files <- list.files(file.path(target_animals, "001", "results"),
                           pattern="_counts.csv", full.names = T)


# Read data ---------------------------------------------------------------
df <- map(counts_files, read_csv) %>%
  bind_rows() %>% 
  mutate(animal_id = str_extract(img_source, pattern = "MG[0-9]+"))


# selected AP breaks
AP <- c(2.5, 2.0, 1.6, 1.0, 0.5, 0.09, -0.5, -1.0, -1.5, -1.9, -2.5, -3.1)
# margin in mm to bin things into the same AP
margin <- 0.25

AP_key <- 
tibble::tibble(AP=AP,
              from=AP-margin,
              to=AP+margin)
      
# make the breaks
df <- df %>% 
  select(animal_id, img_source, parent, side, value, cell_dens, mm.from.bregma) %>% 
  # filter those in range
  filter(data.table::inrange(mm.from.bregma,
                             lower = AP_key$from,
                             upper = AP_key$to))

# Inspect that the filters are working and there is no overlap
df %>% 
 ggplot(aes(mm.from.bregma)) +
  annotate("rect", xmin=AP_key$from, xmax=AP_key$AP,
           fill="red", alpha=0.3, ymin=-Inf, ymax=Inf)+
  annotate("rect", xmin=AP_key$AP, xmax=AP_key$to,
           fill="blue", alpha=0.3, ymin=-Inf, ymax=Inf)+
  geom_bar()+
  geom_vline(xintercept = AP, lwd=1) +
  scale_x_reverse()



# Inspect dataset ---------------------------------------------------------
# heavily heavily skewed!
ggplot(df, aes(value))+
  geom_histogram(bins=100)

quantile(df$value,
         probs = seq(0,1,0.02)) %>%
  enframe(name = "percent", value="Cell_Count") %>% 
  View()

## we can transform that with log10 + 1 
ggplot(df, aes(log10(value+1)))+
  geom_histogram(bins=100)

# Try to find whether we have data that has counts that are too large
# At the same time, try to find whether we have difference in left/right
df %>%
  group_by(animal_id, img_source, parent) %>%
  summarise(diff=abs(first(value)-last(value)),
            total=first(value)+last(value),
            frac_diff = diff/total,
            n=n(),
            max_val = max(value, na.rm = T),
            min_val = min(value, na.rm=T)) %>% 
  ggplot(aes(max_val, min_val))+
  geom_abline(intercept = -150, slope=1, col="red")+
  geom_point(alpha=0.5, aes(color=min_val > -150 + max_val))



# Filter out outliers  ----------------------------------------------------
df <- 
df %>% 
  group_by(animal_id, img_source, parent) %>% 
  mutate(max_val = max(value, na.rm = T),
         min_val = min(value, na.rm =T)) %>% 
  filter(min_val > -150 + max_val) 

# There is a warning saying some NA points were removed
# I've seen this happen when both sides have zero cells and NAs come from 0/0 error
# Try to do to inspect whether it's because total cells are 0 on both sides
# %>% filter(is.na(frac_diff))

find_closest <- function(values, AP_targets){
  
  target_index <- sapply(values,
                         function(tt) which.min(abs(tt - AP_targets)))
  return(
    AP_targets[target_index]
  )
}  

# Put everyone in their own bins -----
side_averages <- 
df %>% 
  mutate(AP_bin = find_closest(mm.from.bregma, AP)) %>% 
  group_by(animal_id, AP_bin, parent, side) %>% 
  summarise(side_val = mean(value))

# there's one NA so we remove that
group_N <- animal_key %>% 
  count(genotype, treatment) %>% 
  filter(!is.na(genotype))

# Merge with data key -----------------------------------------------------
side_averages <-
side_averages %>% 
  left_join(animal_key) 

# FOR TUESDAY
side_averages %>% 
ggplot(aes(genotype, side_val, color=treatment))+
  ggbeeswarm::geom_quasirandom(dodge.width = 1) +
  #coord_flip()+
  geom_boxplot(position=position_dodge(1),
               , width=0.1, fill='black')+
  labs(title="cFos counts for each group",
       subtitle = "Each dot is the mean for one brain region side")

# FOR TUESDAY
side_averages %>% 
  ggplot(aes(genotype, log10(side_val + 1 ), fill=treatment))+
  geom_boxplot(width=0.5) +
  geom_text(data=group_N,
            aes(genotype, y=-0.5, label=paste0("N=",n)),
            position=position_dodge(0.5))+
  labs(title="cFos count",
       subtitle="Counts were averaged within AP_bins for the same side")+
  #facet_wrap(~side) +
  NULL


# Average within group to get a "representative" animal -------------------
group_summaries <- 
side_averages %>% 
  group_by(genotype,treatment,AP_bin, parent, side) %>% 
  summarise(
    n = n(),
    group_val = mean(side_val),
    group_sd = sd(side_val))    
  
  group_summaries %>% 
  ggplot(aes(group_val, group_sd))+
  geom_point()+
  facet_grid(treatment ~ genotype)+
  labs(title="mean vs sd plot")
  
  group_summaries %>% 
    ggplot(aes(genotype, group_val, color=treatment))+
    ggbeeswarm::geom_quasirandom(dodge.width = 1) +
    coord_flip()+
    geom_boxplot(position=position_dodge(1),
                 width=0.1, fill='black') +
    labs(title="cFos counts for each group",
         subtitle = "Each dot is the mean for one brain region")
  
  # AP
  # FOR TUESDAY
  group_summaries %>% 
    ggplot(aes(AP_bin, log10(group_val+1)))+
    geom_point(alpha=0.5)+
    facet_grid(genotype~treatment)
  
  group_summaries %>% 
    group_by(genotype, treatment, AP_bin) %>% 
    summarise(max_val = max(log10(group_val+1), na.rm=T),
              min_val = min(log10(group_val+1), na.rm=T)) %>% 
    ggplot(aes(AP_bin))+
    geom_ribbon(aes(ymax=max_val,
                    ymin=min_val))+
    facet_grid(genotype~treatment)
  
  
# dot.plot helper for plots -----------------------------------------------
dot.plot <- function(df, mode="value", folder = NULL){
   if(mode=="value"){
  #   df <- select(df, -contains("cell_dens")) %>% 
  #     rename(left = value_left,
  #            right = value_right)
     y_label <- "Cell Counts"
   } else {
  #   df <- select(df, -contains("value")) %>% 
  #     rename(left = cell_dens_left,
  #            right = cell_dens_right)
     y_label <- "Cell Density"
   }
  
  # we assume just one AP value for df
  AP <- unique(df$AP)[1]
  
  p <- df %>%   
    mutate(parent = fct_reorder(factor(parent), max_count, .desc = T)) %>% 
    ggplot(aes(x=parent))+
    geom_segment(aes(xend=parent, y=left, yend=right))+
    geom_point(aes(y = left, color="left"))+ 
    geom_point(aes(y=right, color="right"))+ 
    coord_flip()+
    scale_color_manual(values=c("orange", "black"))+
    #facet_wrap(~AP, scales ="free_y", labeller = label_both)+
    facet_grid(genotype~treatment, scales="free_y", labeller = label_both)+
    labs(title = paste("AP:", AP, "mm"),
         subtitle= "Each point is the average for the group * treatment at each brain side",
         x="", y=y_label, color="Side")+
    theme_bw()
  #scale_y_log10()+
  #annotation_logticks(sides = "bottom")
  #fn <- paste0(animal_id, "_", unique(df$AP), "_dot_plot.png")
  #if(is.null(folder)) {
  #  folder <- getwd()
  #}
  #fn <- file.path(folder, fn)
  #ggsave(fn, p)
  return(p)
}
  
group_summaries_wide <-
group_summaries %>%
  pivot_wider(names_from = side,
              values_from = c(group_val, group_sd))

group_summaries_wide %>% 
  ungroup() %>% 
  rename(
    AP = AP_bin,
    left = group_val_left,
    right= group_val_right) %>%
  mutate(max_count = max(left, right)) %>% 
  group_split(AP) %>% 
  map(., dot.plot)


# heatmaps ----------------------------------------------------------------
# only one value for each region for each animal
animal_averages <-
  side_averages %>% 
  group_by(genotype, treatment, animal_id, AP_bin, parent) %>% 
  summarise(animal_val = mean(side_val))

animal_averages %>% 
  ggplot(aes(animal_id, interaction(AP_bin, parent)))+
  geom_tile(aes(fill=log10(animal_val+1)))+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(genotype~treatment, scales="free")
  
animal_averages_wide <- 
animal_averages %>%
  mutate(inter = paste(AP_bin, parent, sep="_")) %>%
  pivot_wider(-c(AP_bin, parent),
              names_from = inter,
              values_from = animal_val)

# we need to fill NAs with values
# we can go two ways, filling by the average structure or filling by the animal
# This is using the column as average
col_animal_ave <- 
animal_averages_wide %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.x),
                                mean(.x, na.rm = TRUE),
                                .x))

# calculate the rowmeans before
row_means <- animal_averages_wide %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  rowMeans(na.rm=T)

row_animal_ave <-
animal_averages_wide %>% 
  ungroup() %>% 
  mutate(row_mean = row_means) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.x),
                                row_mean,
                                .x))

# This is still unclustered
col_animal_ave %>% 
  reshape2::melt(id.vars=c("genotype", "treatment", "animal_id")) %>% 
  ggplot(aes(animal_id, variable))+
  geom_tile(aes(fill=log10(value+1)))+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(genotype~treatment, scales="free")

# Cluster data using euclidean distance
hclusters <- 
col_animal_ave %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  mutate_all(~log10(. + 1)) %>% 
  dist() %>%
  hclust() 
hclusters %>% plot


ggdendro::ggdendrogram(hclusters, rotate = TRUE)

tibble::tibble(
hclusters$order,
col_animal_ave$animal_id[hclusters$order],
col_animal_ave$genotype[hclusters$order],
col_animal_ave$treatment[hclusters$order]
)

col_animal_ave %>% 
#  mutate(animal_id = factor(
#    animal_id, levels = col_animal_ave$animal_id[hclusters$order]
#  )) %>% 
  reshape2::melt(id.vars=c("genotype", "treatment", "animal_id")) %>% 
  mutate(value = log10(value + 1)) %>% 
  ggplot(aes(animal_id, variable))+
  geom_tile(aes(fill=value))+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(treatment~genotype, scales="free_x")



# Principal component analysis --------------------------------------------
library("FactoMineR")
library("factoextra")
col_animal_ave %>%
 ungroup() %>%
  mutate_if(is.numeric, ~log10(. + 1 )) %>% 
  PCA(scale.unit = TRUE, ncp = 5, graph = TRUE, quali.sup = 1:3) -> pca

corr_matrix_plot <-   function(x){
  treatment <- x$treatment
  genotype <- x$genotype
  
  p <- x %>% select_if(is.numeric) %>%
    mutate_all(~log10(. + 1)) %>% 
    corrr::correlate() %>% 
    corrr::stretch() %>% 
    ggplot(aes(x, y, fill=r))+
    geom_tile()+
    scale_fill_viridis_c()+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(title = paste(genotype[1], treatment[1]),
         subtitle = paste("n=", length(genotype)),
         x="", y="")
  return(p)
} 
# correlation matrix
col_animal_ave %>%
  group_split(treatment, genotype) %>% 
  purrr::map(
  corr_matrix_plot
  ) -> cor_plot_list 

cowplot::plot_grid(plotlist = cor_plot_list)

# percentage of variance explained
fviz_screeplot(pca, ncp=10)
plot.PCA(pca, axes = c(1,2), choix=c("ind"))
fviz_pca_ind(pca, geom="point",
             col.ind =interaction(col_animal_ave$genotype, col_animal_ave$treatment),
             addEllipses = TRUE)

# add network correlation plot
# selected regions
# SSp


# FOR TUESDAY
# 1) coverage plot
# 2) 

