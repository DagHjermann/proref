
#
# Comparing 1992-2022 with 2003-2022
#

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(ggrepel)
library(glue)

#
# GET/CALCULATE DATA ----
#

#
# Read data ----
#

datasets <- tibble::tribble(
  ~analysis_name, ~fn_rawdata, ~fn_series, ~fn_result, ~year1, ~year2,
  "Original (1992-2022)", "54_data_2024.rds", "54_dataseries_2024.rds", "54_result_detailed_2024-11-29.rds", 1992, 2022,
  "Original (2003-2022)", "54_data_2024.rds", "54_dataseries_2024.rds", "54_result_detailed_2003-2022_2025-01-05-T1812.rds", 2003, 2022,
  "LOQ-filtered (2003-2022)", "54_data_2024_loqfilter3x.rds", "54_dataseries_2024_loqfilter3x.rds", "54_result_detailed_2003-2022_2025-01-20-T1603.rds", 2003, 2022
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# Names of data frames:
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# datasets
# result_detailed -> lookup_background (only used once|)
# data_all_comb + lookup_background -> data_backgr_all -> data_proref
# result_detailed -> number_of_backgr_stations -> temporary_plotdata
# result_detailed -> result_bystation
# selection by parameter + species:
#   result_detailed -> result_sel
#   data_proref -> data_proref_sel
#   data_all -> data_sel
#   data_backgr_all -> data_backgr_sel
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


read_data <- function(fn){
  readRDS(paste0("Data/", fn))
}

result_detailed <- bind_rows(
  bind_cols(data.frame(Analysis = datasets$analysis_name[1]), read_data(datasets$fn_result[1])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[2]), read_data(datasets$fn_result[2])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[3]), read_data(datasets$fn_result[3]))
) %>%
  mutate(Analysis = fct_inorder(Analysis))


#
# Calculate proref value ----
#

lookup_background <- result_detailed %>%
  select(Analysis, Station, LATIN_NAME, PARAM, Background1b) %>%
  rename(Background = Background1b)

#
# .-- raw data ----
# 
data_all_comb <- bind_rows(
  bind_cols(data.frame(Analysis = datasets$analysis_name[1]), read_data(datasets$fn_rawdata[1]) %>% filter(YEAR %in% datasets$year1[1]:datasets$year2[1])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[2]), read_data(datasets$fn_rawdata[2]) %>% filter(YEAR %in% datasets$year1[2]:datasets$year2[2])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[3]), read_data(datasets$fn_rawdata[3]) %>% filter(YEAR %in% datasets$year1[3]:datasets$year2[3]))
) %>%
  mutate(Analysis = fct_inorder(Analysis))

#
# .-- background stations ----
#
data_backgr_all <- lookup_background %>%
  # These parameters are only found in raw data file number 1 - they were fixed later
  filter(!grepl("SCCP__", PARAM)) %>%
  filter(!grepl("MCCP__", PARAM)) %>%
  filter(!grepl("Krysen__", PARAM)) %>%
  select(Analysis, Station, LATIN_NAME, PARAM, Background) %>%
  left_join(data_all_comb %>%
              filter(!is.na(Concentration)) %>%
              select(Analysis, Station = STATION_CODE, LATIN_NAME, PARAM, YEAR, Concentration, FLAG1), 
            by = join_by(Analysis, Station, LATIN_NAME, PARAM), 
            relationship = "one-to-many") %>%
  filter(!is.na(Background)) %>%
  mutate(
    Station_bg = case_when(
      Background %in% "Other" ~ NA,
      Background %in% "Background" ~ Station)
  ) %>%
  arrange(LATIN_NAME, PARAM, Analysis, desc(Background), Station_bg) %>%
  mutate(LOQ = case_when(
    is.na(FLAG1) ~ "Over LOQ",
    TRUE ~ "Under LOQ"))

#
# .-- proref value (90th percentile)----
#
data_proref <- data_backgr_all %>% 
  filter(Background %in% "Background") %>%
  group_by(Analysis, LATIN_NAME, PARAM) %>%
  summarize(PROREF = quantile(Concentration, 0.9) %>% signif(3),
            Median = quantile(Concentration, 0.5) %>% signif(3),
            n_measurements = n(),
            Backgr_stations = paste(unique(Station_bg), collapse = ","),
            n_stations = length(unique(Station_bg)),
            .groups = "drop")

nrow(data_proref)

if (FALSE){
  writexl::write_xlsx(data_proref, "Data/55_proref.xlsx")
}


#
# VARIOUS DATA/PLOTS ----
#

#
# . number of background stations ---- 
#

number_of_backgr_stations <- result_detailed %>%
  filter(Background1b %in% "Background") %>%
  count(Analysis, PARAM, LATIN_NAME) %>%
  tidyr::pivot_wider(names_from = Analysis, values_from = n) %>%
  mutate(
    diff1 = `Original (2003-2022)` - `Original (1992-2022)`,
    diff2 = `LOQ-filtered (2003-2022)` - `Original (2003-2022)`) %>%
  arrange(diff1, diff2)

#
# . largest differences, 1992-2022 vs 2003-2022
#

temporary_plotdata <- number_of_backgr_stations %>%
  count(`Original (1992-2022)`, `Original (2003-2022)`)

ggplot(temporary_plotdata, aes(x = `Original (1992-2022)`, y = `Original (2003-2022)`, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  geom_text(
    data = temporary_plotdata %>% filter(n < 300), 
    aes(label = n), color = "white", size = 3) +
  geom_text(
    data = temporary_plotdata %>% filter(n >= 300), 
    aes(label = n), color = "black", size = 3) +
  labs(title = "Number of background stations per substance, 1992-2022 vs 2003-2022")

table(number_of_backgr_stations$`Original (1992-2022)`, number_of_backgr_stations$diff1)

#
# . largest differences, 2003-2022 vs 2003-2022
#

temporary_plotdata <- number_of_backgr_stations %>%
  count(`Original (2003-2022)`, `LOQ-filtered (2003-2022)`)

ggplot(temporary_plotdata, aes(x = `Original (2003-2022)`, y = `LOQ-filtered (2003-2022)`, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  geom_text(
    data = temporary_plotdata %>% filter(n < 100), 
    aes(label = n), color = "white", size = 3) +
  geom_text(
    data = temporary_plotdata %>% filter(n >= 100), 
    aes(label = n), color = "black", size = 3) +
  labs(title = "Number of background stations per substance, 2003-2022, without and with filtering LOQ")

table(number_of_backgr_stations$`Original (1992-2022)`, number_of_backgr_stations$diff1)


#
# . find stations classified as background most/least often  
#
result_bystation <- result_detailed %>%
  # filter(grepl("30", Station)) %>%
  count(Analysis, LATIN_NAME, Station, Background1b) %>%
  tidyr::pivot_wider(names_from = Background1b, values_from = n, values_fill = 0) %>%
  mutate(fraction_background = Background/(Background + Other)) %>%
  arrange(LATIN_NAME, Analysis, fraction_background)

result_bystation_mussel <- result_bystation %>%
  filter(Analysis == "LOQ-filtered (2003-2022)" & LATIN_NAME == "Mytilus edulis") %>%
  mutate(Station = fct_inorder(Station))

ggplot(result_bystation_mussel, aes(Station, fraction_background)) +
  geom_col() +
  coord_flip()

result_bystation_cod <- result_bystation %>%
  filter(Analysis == "LOQ-filtered (2003-2022)" & LATIN_NAME == "Gadus morhua") %>%
  mutate(Station = fct_inorder(Station))

ggplot(result_bystation_cod, aes(Station, fraction_background)) +
  geom_col() +
  coord_flip()

#
# . proref value with number of background stations -----
#

data_proref_wide <- data_proref %>%
  tidyr::pivot_wider(names_from = Analysis, names_prefix = "proref ", values_from = PROREF) %>%
  left_join(number_of_backgr_stations, by = join_by(LATIN_NAME, PARAM)) %>%
  select(
    LATIN_NAME, PARAM, 
    `proref Original (1992-2022)`, n1 = `Original (1992-2022)`, 
    `proref Original (2003-2022)`, n2 = `Original (2003-2022)`,
    `proref LOQ-filtered (2003-2022)`, n3 = `LOQ-filtered (2003-2022)`)

ggplot(data_proref_wide, aes(`proref Original (1992-2022)`, `proref Original (2003-2022)`)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

ggplot(data_proref_wide, aes(`proref Original (2003-2022)`, `proref LOQ-filtered (2003-2022)`)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

if (FALSE){
  writexl::write_xlsx(
    list(data_proref_wide,
         data.frame(info = "n1, n2, n3 = number of background stations")), 
    "Data/55_proref_wide.xlsx")
}

#
# BBF etc. ----
#
# Concluded (with Merete G, Anders, Kine) that all these are the same
# Called BBJF, see 54
#

if (FALSE){

data_all_comb %>%
  filter(Analysis == "Original (1992-2022)" & PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW")) %>%
  xtabs(~YEAR + PARAM, .)

data_all_comb[[1]] %>%
  filter(Analysis == "Original (1992-2022)" & PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW")) %>%
  xtabs(~YEAR + STATION_CODE, .)

data_all_comb[[1]] %>%
  filter(Analysis == "Original (1992-2022)" & PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW") & STATION_CODE %in% c("I131A", "30A")) %>%
  xtabs(~YEAR + PARAM, .)

data_all_comb[[1]] %>%
  filter(Analysis == "Original (1992-2022)" & PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW") & STATION_CODE %in% c("I301", "I131A", "30A", "I969")) %>%
  mutate(LOQ = ifelse(is.na(FLAG1), "Over LOQ", "Under LOQ")) %>%
  ggplot(aes(YEAR, Concentration, shape = LOQ, colour = PARAM)) +
  # geom_smooth(se = FALSE) +
  geom_point() +
  scale_shape_manual(values = c(19,6)) +
  facet_wrap(vars(STATION_CODE)) +
  theme_bw()
  
}


#
# FOR APP ----
# 

#
# . for ui (choose parameter and species) ----
#

param <- "CU__WW"
param <- "CB118__WW"
species_common <- "mussel"
species_common <- "cod"

if (species_common == "mussel"){
  species <- "Mytilus edulis"
} else if (species_common == "cod"){
  species <- "Gadus morhua"
}

# Choose analysis 
d1 <- 1
d2 <- 3
analysis1 <- datasets$analysis_name[d1]
analysis2 <- datasets$analysis_name[d2]


#
# . for server ----
#

#
# .-- select data ---- 
#

result_sel <- result_detailed %>%
  filter(Analysis %in% c(analysis1, analysis2),
         PARAM == param,
         LATIN_NAME == species) %>%
  mutate(Analysis = fct_drop(Analysis))

data_proref_sel <- data_proref %>%
  filter(Analysis %in% c(analysis1, analysis2),
         PARAM == param,
         LATIN_NAME == species) %>%
  mutate(Analysis = fct_drop(Analysis))

data_sel <- data_all_comb %>%
  filter(Analysis %in% c(analysis1, analysis2),
         PARAM == param,
         LATIN_NAME == species) %>%
  mutate(Analysis = fct_drop(Analysis))

data_backgr_sel <- data_backgr_all %>%
  filter(Analysis %in% c(analysis1, analysis2),
         PARAM == param,
         LATIN_NAME == species) %>%
  mutate(Analysis = fct_drop(Analysis))

#
# .-- stations----
#

# bg1 <- result_sel %>%
#   filter(Analysis == result_txt1, Background1b == "Background") %>%
#   pull(Station) 
# bg2 <- result_sel %>%
#   filter(Analysis == result_txt2, Background1b == "Background") %>%
#   pull(Station) 

#
# .-- overall median by station ----
#

data_overall_median <- data_backgr_sel %>%
  ungroup() %>%
  group_by(Analysis, Station) %>%
  summarize(Median = median(Concentration),
            Min_year = min(YEAR),
            Max_year = max(YEAR),
            Station_bg = first(Station_bg),
            .groups = "drop") %>%
  mutate(
    Background = case_when(
      !is.na(Station_bg) ~ "Background",
      is.na(Station_bg) ~ "Other")) 

#
# .-- median by station and year ----
#

data_medians_sel <- data_backgr_sel %>%
  ungroup() %>%
  group_by(Analysis, Station, YEAR) %>%
  summarize(Concentration = median(Concentration),
            Percent_below_LOQ = mean(!is.na(FLAG1))*100,
            Half_below_LOQ = (Percent_below_LOQ >= 50),
            Station_bg = first(Station_bg),
            .groups = "drop") %>%
  mutate(
    Background = case_when(
      !is.na(Station_bg) ~ "Background",
      is.na(Station_bg) ~ "Other"))

#
# . for ui (comparison plots) ----
#

#
# .-- plot 1: algorithm plot ----
#

# For plotting line at 2x the cleanest station
line_2x_cleanest <- result_sel %>%
  group_by(Analysis) %>%
  summarize(Median2 = 2*first(Median2))

ggplot(result_sel, aes(Rank, Median2)) +
  geom_line() +
  geom_point(aes(col = Background1b)) +
  geom_text_repel(aes(label = glue("{Station} (P = {P_round})")), hjust = 0, 
                  nudge_x = 0.5, point.padding = 0.2, box.padding = 0.25, direction = "y") +
  # Line at 2x the cleanest station
  geom_hline(data = line_2x_cleanest, aes(yintercept = Median2), 
             linetype = "dashed", colour = "brown") +
  # Proref line
  geom_hline(
    data = data_proref_sel, aes(yintercept = PROREF),
    colour = "blue2", linetype = "dashed") +
  expand_limits(y = 0) +
  facet_wrap(vars(Analysis)) 


#
# .-- plot 2: overall median + time range plot ----
#
ggplot(data_overall_median %>%
         arrange(Analysis, desc(Background)), aes(x = Min_year, xend = Max_year, y = Median)) +
  geom_segment(aes(col = Background)) +
  # Proref line
  geom_hline(
    data = data_proref_sel, aes(yintercept = PROREF),
    colour = "blue2", linetype = "dashed") +
  expand_limits(y = 0) +
  facet_wrap(vars(Analysis))


#
#  .-- plot 3: annual medians ----
#

ggplot(data_medians_sel %>% filter(Background != "Other"), 
       aes(YEAR, Concentration, group = Station)) +
  geom_line(
    data = data_medians_sel %>% filter(Background %in% "Other"), color = "grey70") +
  geom_line(aes(color = Station_bg)) +
  geom_point(aes(color = Station_bg, shape = Half_below_LOQ)) +
  scale_shape_manual(values = c("TRUE" = 6, "FALSE" = 16)) +
  geom_hline(data = data_proref_sel, aes(yintercept = PROREF), 
             linetype = "dashed", colour = "blue") +
  scale_y_log10() +
  facet_wrap(vars(Analysis))


#
#  .-- plot 4: plot of raw data and proref ----
#

# For setting default y axis range
max_bg = data_backgr_sel %>%
  filter(Background %in% "Background") %>%
  pull(Concentration) %>%
  max()

# test
# ggplot(data_backgr_sel, aes(YEAR, Concentration)) +
#   geom_jitter(
#     aes(color = Station_bg, size = Background, shape = LOQ), width = 0.2) +
#   scale_size_manual(values = c("Other"=1, "Background"=2)) +
#   scale_shape_manual(values = c("Under LOQ" = 6, "Over LOQ" = 16)) +
#   geom_hline(
#     data = data_proref_sel, aes(yintercept = PROREF),
#     colour = "red", linetype = "dashed") +
#   scale_y_log10() +
#   facet_wrap(vars(Analysis), nrow = 1) +
#   theme_bw()

create_proref_plot <- function(data, data_proref){
  gg1 <- ggplot(data, aes(YEAR, Concentration)) +
    geom_jitter(
      aes(color = Station_bg, size = Background, shape = LOQ), width = 0.2) +
    scale_size_manual(values = c("Other"=1, "Background"=2)) +
    scale_shape_manual(values = c("Under LOQ" = 6, "Over LOQ" = 16)) +
    geom_hline(
      data = data_proref, aes(yintercept = PROREF),
      colour = "blue", linetype = "dashed", linewidth = 1) +
    facet_wrap(vars(Analysis), nrow = 1) +
    theme_bw()
  
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
      colour = "blue", hjust = 0, vjust = 1.5)
  
  gg2

}

gg_all <- create_proref_plot(data_backgr_sel, 
                             data_proref_sel)
gg_bg <- create_proref_plot(data_backgr_sel %>% filter(Background %in% "Background"),
                            data_proref_sel)

# Show all data
gg_all
# Show all data, restrict y axis to background station data
gg_all + ylim(0, max_bg)
gg_all + ylim(0, max_bg) + scale_y_log10()
# Show only background station data
gg_bg
gg_bg + scale_y_log10()


# MAP ----

# Map data - copy from 'Input_data':
# milkys_example_coord <- readxl::read_excel("../Input_data/Files_to_Jupyterhub_2021/Kartbase_edit.xlsx")
# readr::write_csv(milkys_example_coord, "Input_data/coordinates.csv")

library(leaflet)

stations <- read.csv("Input_data/coordinates.csv") %>%
  rename(Station = STATION_CODE, x = Long, y = Lat)  %>%
  filter(Station %in% unique(result_detailed$Station))

lookup_species <- data.frame(
  species = c("Cod", "Blue mussel"),
  LATIN_NAME = c("Gadus morhua", "Mytilus edulis"),
  color = c("red", "blue")
)

stations_for_map <- result_detailed %>%
  distinct(Station, LATIN_NAME) %>%
  left_join(stations, by = join_by(Station)) %>%
  left_join(lookup_species, by = join_by(LATIN_NAME))

leaflet() %>% 
  setView(lng = 13 , lat = 66, zoom = 4) %>%
  addTiles() %>%
  addCircleMarkers(data=stations_for_map, 
                   ~x , ~y, layerId=~Station, popup=~Station, 
                   color="black",  fillColor=~color, 
                   radius=5, weight = 1, stroke = TRUE, fillOpacity = 0.8)

  
  # addCircleMarkers(data=stations_selected_click, ~x , ~y, radius=12 , color="green",  fillColor="green", stroke = TRUE) %>%
  # addCircleMarkers(data=stations_selected_species, ~x , ~y, layerId=~STATION_CODE, popup=~STATION_CODE, radius=8 , color="black",  fillColor=~color, stroke = TRUE, fillOpacity = 0.8)



