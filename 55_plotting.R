
library(dplyr)
library(ggplot2)
library(forcats)

#
# data ----
#
data_all2 <- readRDS("Data/54_data_2024.rds")
df_series_sel <- readRDS("Data/54_dataseries_2024.rds")

result_detailed1 <- readRDS("Data/54_result_detailed_2024-11-29.rds")
result_txt1 <- "Original (1992-2022)"
result_detailed2 <- readRDS("Data/54_result_detailed_2003-2022_2025-01-05-T1812.rds")
result_txt2 <- "2003-2022"

result_detailed <- bind_rows(
  bind_cols(data.frame(Analysis = result_txt1), result_detailed1),
  bind_cols(data.frame(Analysis = result_txt2), result_detailed2)
) %>%
  mutate(Analysis = fct_inorder(Analysis))

#
# choose parameter and species ----
#
param <- "CU__WW"
species_common <- "mussel"

if (species_common == "mussel"){
  species <- "Mytilus edulis"
} else if (species_common == "cod"){
  species <- "Gadus morhua"
}

#
# for server ----
#
result_sel <- result_detailed %>%
  filter(PARAM == param & LATIN_NAME == species)
data_sel <- data_all2 %>%
  filter(PARAM == param & LATIN_NAME == species)


# Stations 
bg1 <- result_sel %>%
  filter(Analysis == result_txt1, Background1b == "Background") %>%
  pull(Station) 
bg2 <- result_sel %>%
  filter(Analysis == result_txt2, Background1b == "Background") %>%
  pull(Station) 

# Medians
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

# Combine medians to a data frame, and add the 'Background1b' column    
# Note: specifying 'Background1b' as the background we use here (HARD-CODED)
df_medians <- annualmedians_result$data_medians_list %>% 
  bind_rows() %>%
  rename(Station = STATION_CODE) %>%
  left_join(result_sel %>% select(Station, Background1b),
            by = join_by(Station), relationship = "many-to-one")



#
# plots ----
#

#
# plot 1: algorithm plot 
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
# plot 2: median + time range plot
#
ggplot(result_sel, aes(x = Min_year, xend = Max_year, y = Median2)) +
  geom_segment(aes(col = Background1b)) +
  expand_limits(y = 0) +
  facet_wrap(vars(Analysis))

#
#  plot 3: plot of medians
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

  

