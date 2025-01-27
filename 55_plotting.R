
#
# Comparing 1992-2022 with 2003-2022
#


library(dplyr)
library(ggplot2)
library(forcats)
library(ggrepel)

#
# data ----
#

datasets <- tibble::tribble(
  ~analysis_name, ~fn_rawdata, ~fn_series, ~fn_result, ~year1, ~year2,
  "Original (1992-2022)", "54_data_2024.rds", "54_dataseries_2024.rds", "54_result_detailed_2024-11-29.rds", 1992, 2022,
  "Original (2003-2022)", "54_data_2024.rds", "54_dataseries_2024.rds", "54_result_detailed_2003-2022_2025-01-05-T1812.rds", 2003, 2022,
  "LOQ-filtered (2003-2022)", "54_data_2024_loqfilter3x.rds", "Data/54_dataseries_2024_loqfilter3x.rds", "54_result_detailed_2003-2022_2025-01-20-T1603.rds", 2003, 2022
)

# Full data (note: INCLUDING )
data_all2 <- readRDS("Data/54_data_2024.rds") %>%
  select(STATION_CODE, LATIN_NAME, PARAM, YEAR, Concentration, FLAG1)
df_series_sel <- readRDS("Data/54_dataseries_2024.rds")

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
# .-- raw data and proref value (90th percentile)----
#
# Proref value = 90th percentile 
#

lookup_background <- result_detailed %>%
  select(Analysis, Station, LATIN_NAME, PARAM, Background1b) %>%
  rename(Background = Background1b)

data_all2_comb <- bind_rows(
  bind_cols(data.frame(Analysis = datasets$analysis_name[1]), read_data(datasets$fn_rawdata[1]) %>% filter(YEAR %in% datasets$year1[1]:datasets$year2[1])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[2]), read_data(datasets$fn_rawdata[2]) %>% filter(YEAR %in% datasets$year1[2]:datasets$year2[2])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[3]), read_data(datasets$fn_rawdata[3]) %>% filter(YEAR %in% datasets$year1[3]:datasets$year2[3]))
)

data_all_backgr <- lookup_background %>%
  filter(!grepl("SCCP__", PARAM)) %>%
  filter(!grepl("MCCP__", PARAM)) %>%
  filter(!grepl("Krysen__", PARAM)) %>%
  select(Analysis, Station, LATIN_NAME, PARAM, Background) %>%
  left_join(data_all2_comb %>%
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
# Proref
#
data_proref <- data_all_backgr %>% 
  filter(Background %in% "Background") %>%
  group_by(Analysis, LATIN_NAME, PARAM) %>%
  summarize(PROREF = quantile(Concentration, 0.9) %>% signif(3),
            .groups = "drop")


#
# VARIOUS DATA/PLOTS ----
#

#
# . find largest differences
#

result_diff <- result_detailed %>%
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

result_diff_summ <- result_diff %>%
  count(`Original (1992-2022)`, `Original (2003-2022)`)

ggplot(result_diff_summ, aes(x = `Original (1992-2022)`, y = `Original (2003-2022)`, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  geom_text(
    data = result_diff_summ %>% filter(n < 300), 
    aes(label = n), color = "white", size = 3) +
  geom_text(
    data = result_diff_summ %>% filter(n >= 300), 
    aes(label = n), color = "black", size = 3) +
  labs(title = "Number of background stations per substance, 1992-2022 vs 2003-2022")

table(result_diff$`Original (1992-2022)`, result_diff$diff1)

#
# . largest differences, 1992-2022 vs 2003-2022
#

result_diff_summ <- result_diff %>%
  count(`Original (2003-2022)`, `LOQ-filtered (2003-2022)`)

ggplot(result_diff_summ, aes(x = `Original (2003-2022)`, y = `LOQ-filtered (2003-2022)`, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  geom_text(
    data = result_diff_summ %>% filter(n < 100), 
    aes(label = n), color = "white", size = 3) +
  geom_text(
    data = result_diff_summ %>% filter(n >= 100), 
    aes(label = n), color = "black", size = 3) +
  labs(title = "Number of background stations per substance, 2003-2022, without and with filtering LOQ")

table(result_diff$`Original (1992-2022)`, result_diff$diff1)


#
# . find stations classified as background most/least often  
#
result_bystation <- result_detailed %>%
  # filter(grepl("30", Station)) %>%
  count(Analysis, LATIN_NAME, Station, Background1b) %>%
  tidyr::pivot_wider(names_from = Background1b, values_from = n) %>%
  mutate(fraction_background = Background/(Background + Other)) %>%
  arrange(LATIN_NAME, Analysis, desc(fraction_background))

result_bystation_mussel <- result_bystation %>%
  filter(Analysis == "2003-2022" & LATIN_NAME == "Mytilus edulis") %>%
  mutate(Station = fct_inorder(Station))


ggplot(result_bystation, aes(Station))

#
# BBF etc.
#

data_all2 %>%
  filter(PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW")) %>%
  xtabs(~YEAR + PARAM, .)

data_all2 %>%
  filter(PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW")) %>%
  xtabs(~YEAR + STATION_CODE, .)

data_all2 %>%
  filter(PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW") & STATION_CODE %in% c("I131A", "30A")) %>%
  xtabs(~YEAR + PARAM, .)

data_all2 %>%
  filter(PARAM %in% c("BBF__WW", "BBJF__WW", "BBJKF__WW") & STATION_CODE %in% c("I301", "I131A", "30A", "I969")) %>%
  mutate(LOQ = ifelse(is.na(FLAG1), "Over LOQ", "Under LOQ")) %>%
  ggplot(aes(YEAR, Concentration, shape = LOQ, colour = PARAM)) +
  # geom_smooth(se = FALSE) +
  geom_point() +
  scale_shape_manual(values = c(19,6)) +
  facet_wrap(vars(STATION_CODE)) +
  theme_bw()
  



#
# FOR APP ----
# 

#
# . for ui (choose parameter and species) ----
#

param <- "CU__WW"
param <- "CB105__WW"
species_common <- "mussel"

if (species_common == "mussel"){
  species <- "Mytilus edulis"
} else if (species_common == "cod"){
  species <- "Gadus morhua"
}

#
# . for server ----
#

#
# .-- select data ---- 
#
result_sel <- result_detailed %>%
  filter(PARAM == param & LATIN_NAME == species)
data_sel <- data_all2 %>%
  filter(PARAM == param & LATIN_NAME == species)


#
# .-- stations----
#
bg1 <- result_sel %>%
  filter(Analysis == result_txt1, Background1b == "Background") %>%
  pull(Station) 
bg2 <- result_sel %>%
  filter(Analysis == result_txt2, Background1b == "Background") %>%
  pull(Station) 

#
# .-- median values ----
#
data_medians_sel <- data_sel %>%
  group_by(STATION_CODE, YEAR) %>%
  summarize(Concentration = median(Concentration),
            Percent_below_LOQ = mean(!is.na(FLAG1))*100,
            Half_below_LOQ = (Percent_below_LOQ >= 50),
            .groups = "drop") %>%
  rename(Station = STATION_CODE) %>%
  mutate(
    Background = case_when(
      (Station %in% bg1) & !(Station %in% bg2) ~ "Background 1",
      !(Station %in% bg1) & (Station %in% bg2) ~ "Background 2",
      (Station %in% bg1) & (Station %in% bg2) ~ "Background 1+2",
      TRUE ~ "Other"),
    Background = factor(Background, 
                        levels = c("Background 1", "Background 2", "Background 1+2", "Other"))
  ) %>%
  arrange(rev(Background))

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
  geom_hline(data = line_2x_cleanest, aes(yintercept = Median2), linetype = "dashed", colour = "brown") +
  expand_limits(y = 0) +
  facet_wrap(vars(Analysis))

#
# .-- plot 2: median + time range plot ----
#
ggplot(result_sel, aes(x = Min_year, xend = Max_year, y = Median2)) +
  geom_segment(aes(col = Background1b)) +
  expand_limits(y = 0) +
  facet_wrap(vars(Analysis))

#
#  .-- plot 3: plot of medians (just a single plot) ----
#

ggplot(data_medians_sel %>% filter(Background == "Other"), aes(YEAR, Concentration, group = Station)) +
  geom_line(color = "grey70") +
  scale_y_log10() +
  geom_line(data = data_medians_sel %>% filter(Background != "Other"), 
            aes(color = Background)) +
  geom_point(data = data_medians_sel %>% filter(Background != "Other"), 
             aes(color = Background, shape = Half_below_LOQ)) +
  scale_color_manual(
    values = c("Background 1" = "red", "Background 2" = "blue", "Background 1+2" = "purple", "Other" = "grey70")
  ) +
  scale_shape_manual(
    values = c("TRUE" = 6, "FALSE" = 16)
  )

#
#  .-- plot 4: plot of raw data and proref ----
#

# Select data
data_all_backgr_sel <- data_all_backgr %>%
  filter(PARAM == param, 
         LATIN_NAME %in% species)

data_proref_sel <- data_proref %>%
  filter(PARAM == param, 
         LATIN_NAME %in% species)

# For setting default y axis range
max_bg = data_all_backgr_sel %>%
  filter(Background %in% "Background") %>%
  pull(Concentration) %>%
  max()

create_proref_plot <- function(data, data_proref){
  gg1 <- ggplot(data, aes(YEAR, Concentration)) +
    geom_jitter(
      aes(color = Station_bg, size = Background, shape = LOQ), width = 0.2) +
    scale_size_manual(values = c("Other"=1, "Background"=2)) +
    scale_shape_manual(values = c("Under LOQ" = 6, "Over LOQ" = 16)) +
    geom_hline(
      data = data_proref, aes(yintercept = PROREF),
      colour = "red", linetype = "dashed") +
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
      data =data_proref_sel, aes(label = paste0("PROREF = ", PROREF),
                                 x = proreflabel_x, y = +Inf),
      colour = "red", hjust = 0, vjust = 1.5)
  
  gg2

}

gg_all <- create_proref_plot(data_all_backgr_sel, 
                             data_proref_sel)
gg_bg <- create_proref_plot(data_all_backgr_sel %>% filter(Background %in% "Background"),
                            data_proref_sel)

# Show all data
gg_all
# Show all data, restrict y axis to background station data
gg_all + ylim(0, max_bg)
# Show background station data
gg_bg

