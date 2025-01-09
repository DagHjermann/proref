
#
# create raw data and proref plot
#
# - can be run for all data (data = full data set, non-background shown as smaller grey points)
#     or for only background stations
# - under/over LOQ shown as triangle or dot
# - colour palette is Set1 if max 8 background stations (https://colorbrewer2.org/#type=qualitative&scheme=Set1&n=8)  
# - proref is shown as horizontal line as well as label

create_proref_plot <- function(data, data_proref){
  gg1 <- ggplot(data, aes(YEAR, Concentration)) +
    geom_jitter(
      aes(color = Station_bg, size = Background, shape = LOQ), width = 0.2) +
    scale_size_manual(values = c("Other"= 1, "Background"=2)) + 
    scale_shape_manual(values = c("Under LOQ" = 6, "Over LOQ" = 16)) +
    geom_hline(
      data = data_proref, aes(yintercept = PROREF), 
      colour = "red", linetype = "dashed") +
    facet_wrap(vars(Analysis), nrow = 1) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 12), 
      legend.text =  element_text(size = 12)
    )
  
  # If we have max 8 stations, use brewer Set1 palette, otherwise we stick 
  #   with the default palette
  number_bg_stations <- table(data$Station_bg) %>% length()
  if (number_bg_stations <= 8){
    gg1 <- gg1 +
      scale_colour_brewer(palette = "Set1", na.value = "grey80")
  }
  
  # Add proref label
  proreflabel_x <- ggplot_build(gg1)$layout$panel_scales_x[[1]]$range$range[1]
  gg2 <- gg1 +
    geom_text(
      data = data_proref, aes(label = paste0("PROREF = ", PROREF), 
                                 x = proreflabel_x, y = +Inf), 
      colour = "red", hjust = 0, vjust = 1.5)
  
  gg2
  
}

