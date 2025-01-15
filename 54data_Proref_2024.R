

### Raw data for analysis 
# - We will also take basis into account  
# - To simplify matters, we just modify PARAM so it also contains basis information

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Packages -----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 

library(dplyr)
library(tidyr)
library(ggplot2)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Read data -----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 


# For simplicity, instead of introducing the "Basis" parameter in 
# all functions it's needed, we instead combine PARAM 
# and Basis using double underscore into a modified PARAM value.
# E.g. PFOS wet-weight becomes 'PFOS__WW'  

data_all2 <- readRDS("Input_data/2022/105_data_with_uncertainty_2023-09-12.rds") %>%
  rename(YEAR = MYEAR)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Modify data structure ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 

data_temporary <- data_all2 %>%
  pivot_longer(
    cols = c(VALUE_WW, VALUE_WWa, VALUE_DW, VALUE_DWa, VALUE_FB, VALUE_FBa), 
    names_to = "Basis",
    values_to = "Concentration"
  ) %>%
  mutate(
    # change e.g. "VALUE_WW" into "WW":    
    Basis = sub("VALUE_", "", Basis, fixed = TRUE),
    # combine PARAM and Basis into a modified PARAM value:  
    PARAM = paste0(PARAM, "__", Basis)
  ) %>%
  filter(!is.na(Concentration))

data_all2 <- data_temporary


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Remove some stations  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 


# "B" mussel stations (industry data, from Langøya?)
xtabs(~STATION_CODE, data_all2 %>% filter(substr(STATION_CODE, 1, 1) == "B"))

data_all2 <- data_all2 %>%
  filter(!substr(STATION_CODE, 1, 1) == "B")

# Stations used for very few years
check <- data_all2 %>%
  distinct(STATION_CODE, YEAR) %>%
  count(STATION_CODE) %>%
  arrange(n)
# check
table(check$n)

# Remove those with less than 5 years 
data_all2 <- data_all2 %>%
  group_by(STATION_CODE) %>%
  mutate(no_years = length(unique(YEAR))) %>%
  ungroup %>%             # otherwise, 'df_series_sel' below will end up with 30000 rows
  filter(no_years >= 5)

check <- data_all2 %>%
  distinct(STATION_CODE, YEAR) %>%
  count(STATION_CODE) %>%
  arrange(n)
# check
table(check$n)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Check CHR vs. krysen ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 


data_all2 %>%
  filter(PARAM %in% c("Krysen__WW", "CHR__WW", "BAP__WW")) %>%
  xtabs(~YEAR + PARAM, .)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Change some parameters in list of parameters to be analysed  -------------
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 


data_all2 <- data_all2 %>%
  mutate(
    PARAM = case_when(
      # Older SCCP are just given as "SCCP", not SCCP excl/incl. LOQ
      grepl("SCCP__", PARAM) ~ sub("SCCP__", "SCCP eksl. LOQ__", PARAM),
      grepl("MCCP__", PARAM) ~ sub("MCCP__", "MCCP eksl. LOQ__", PARAM),
      # Krysen = CHR (chrysene)
      grepl("Krysen__", PARAM) ~ sub("Krysen__", "CHR__", PARAM),
      # BBF, BBJF and BBJKF
      # = benzo[b]fluoranthene, benzo[b,j]fluoranthene, and benzo[b,j,k]fluoranthene
      # should all be BBJF (checked with MGR, ARU and KBA)
      grepl("BBF__", PARAM) ~ sub("BBF__", "BBJF__", PARAM),
      grepl("BBJKF__", PARAM) ~ sub("BBJKF__", "BBJF__", PARAM),
      TRUE ~ PARAM
    )
  )


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Check occurence of very high less-thans ----  
#
# I.e. <LOQ values where LOQ is much higher than the smallest detect   
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 

show_plots <- TRUE
show_plots <- FALSE

# Pick 1: PCBs
params <- c("CB28__WW", "CB52__WW", "CB101__WW", "CB105__WW", "CB118__WW", "CB138__WW", 
          "CB153__WW", "CB180__WW")
# Pick 2: After last plot below  
params <- c("CB28__WW", "CB52__WW", "CB101__WW", "PFOSA__WW", "BDE99__WW", "PB__WW", "TPhP__WW", "HBCDG__WW")

spp <- "Gadus morhua"
spp <- "Mytilus edulis"

# For the entire data set  
df_plot <- data_all2 %>%
  filter(grepl("__WW$", PARAM)) %>%
  # filter(YEAR %in% 2012:2023) %>%
  group_by(LATIN_NAME, PARAM, YEAR) %>%
  summarize(
    # Proportion of less-thans
    prop_lessthan = mean(!is.na(FLAG1)),
    # Range of LOQ in less-thans
    max_lessthan = max(Concentration[!is.na(FLAG1)]),
    min_lessthan = min(Concentration[!is.na(FLAG1)]),
    median_lessthan = median(Concentration[!is.na(FLAG1)]),
    # Value in detects (non-less-thans)   
    min_detect = min(Concentration[is.na(FLAG1)]),
    median_detect = median(Concentration[is.na(FLAG1)]),
    # Proportion of less-thans higher than 3x or 5x the smallest detect  
    prop_lessthan_high3 = mean(!is.na(FLAG1) & Concentration > 3*min_detect),
    prop_lessthan_high5 = mean(!is.na(FLAG1) & Concentration > 5*min_detect)
  )

# Plot the proportion of less-thans (recent years)
gg <- ggplot(df_plot %>% filter(PARAM %in% params, LATIN_NAME %in% spp, YEAR %in% 2012:2023), aes(YEAR, PARAM, fill = prop_lessthan, label = round(prop_lessthan, 2))) +
  geom_tile() +
  scale_fill_viridis_c(begin = 0.3) +
  geom_text()
if (show_plots) {gg}

# Plot the maximum of less-thans (all years) 
gg <- ggplot(df_plot %>% filter(PARAM %in% params, LATIN_NAME %in% spp, YEAR >= 2000), aes(YEAR, PARAM, fill = max_lessthan, label = round(max_lessthan, 2))) +
  geom_tile() +
  scale_fill_viridis_c(begin = 0.3) +
  geom_text(size = 3)
if (show_plots) {gg}

# For a given parameter-....  
param <- "CB101__WW"
param <- "CB28__WW"
param <- "CB52__WW"
param <- "BDE99__WW"

# Show LOQ value of less-thans (black line with range)
# - median value of detetcts (red)
# - minimum value of detetcts (blue)
gg <- ggplot(df_plot %>% filter(PARAM %in% param, LATIN_NAME %in% spp), aes(YEAR, median_lessthan)) +
  geom_linerange(aes(ymin = min_lessthan, ymax = max_lessthan)) +
  geom_line() +
  geom_point() +
  geom_point(aes(y = median_detect), color = "red2") +
  geom_point(aes(y = min_detect), color = "blue") +
  labs(title = param) 
# plot
if (show_plots) {gg }
# log scale
if (show_plots) {gg + scale_y_log10() }
# restricted y scale
if (show_plots) {gg + coord_cartesian(ylim = c(0,1)) }

# proportion of less-thans (black)
#   and proportion of data that would be filtered out by
#   "larger than 3x of min detect" (orange) and "larger than 5x of min detect" (red) rules
gg <- ggplot(df_plot %>% filter(PARAM %in% param, LATIN_NAME %in% spp), aes(YEAR, prop_lessthan)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = prop_lessthan_high3), color = "orange") +
  geom_line(aes(y = prop_lessthan_high5), color = "red") +
  labs(title = param)
# plot
if (show_plots) {gg }

# all selected parameters by "params": 
# proportion of of data that would be filtered out by "larger than 3x of min detect" rule
gg <- ggplot(df_plot %>% filter(PARAM %in% params, LATIN_NAME %in% spp), aes(YEAR, prop_lessthan_high3, color = PARAM)) +
  geom_line() +
  geom_point()
# plot
if (show_plots) {gg }

# data: all parameters where at least in one year, 30% or more of the data will be filtered out 
# shown as colour / text: proportion of of data that would be filtered out

df_plot_temporary <- df_plot %>% 
  filter(LATIN_NAME %in% c("Mytilus edulis", "Gadus morhua")) %>%
  filter(YEAR >= 2000) %>%
  # select only data with parameters where at least one year, >30% of the data are filtered out   
  group_by(LATIN_NAME, PARAM) %>%
  mutate(
    prop_lessthan_high3_max = max(prop_lessthan_high3),
    prop_lessthan_high5_max = max(prop_lessthan_high5))

# rule for filtering:  by "larger than 3x of min detect" rule
gg <- df_plot_temporary %>%
  filter(prop_lessthan_high3_max > 0.3) %>% 
  # plot (as above)   
  ggplot(aes(YEAR, PARAM, fill = prop_lessthan_high3, label = round(prop_lessthan_high5, 2))) +
  geom_tile() +
  scale_fill_viridis_c(begin = 0.3) +
  geom_text(size = 3) +
  facet_wrap(vars(LATIN_NAME)) +
  labs(title = "proportion of of data that would be filtered out",
       subtitle = "using 'larger than 3x of min detect' rule") 
# plot
if (show_plots) {gg }

# rule for filtering:  by "larger than 5x of min detect" rule
gg <- df_plot_temporary %>%
  filter(prop_lessthan_high5_max > 0.3) %>% 
  # plot (as above)   
  ggplot(aes(YEAR, PARAM, fill = prop_lessthan_high5, label = round(prop_lessthan_high5, 2))) +
  geom_tile() +
  scale_fill_viridis_c(begin = 0.3) +
  geom_text(size = 3) +
  facet_wrap(vars(LATIN_NAME)) +
  labs(title = "proportion of of data that would be filtered out",
       subtitle = "using 'larger than 5x of min detect' rule")
# plot
if (show_plots) {gg }



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Filtering out very high less-thans ----  
#
# Filtering <LOQ values where LOQ >3x the smallest detect value in that year   
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 

# to avoid warning messages
min_safe <- function(x){
  if (length(x) == 0){
    NA
  } else {
    min(x)
  }
}
# test
# x <- 1:5
# y <- x > 10
# min_safe(x[y])

data_all2_filtered <- data_all2 %>%
  group_by(LATIN_NAME, PARAM, YEAR) %>%
  mutate(
    # Smallest detect value (non-less-thans)   
    min_detect = min_safe(Concentration[is.na(FLAG1)])
  ) %>%
  filter(
    !(FLAG1 %in% "<" & Concentration > 3*min_detect)
  )

cat("Rows before filtering high less-thans: ", 
    nrow(data_all2)/1E6, "millions\n")
cat("Rows removed by filtering filtering high less-thans: ", 
    nrow(data_all2)/1E6 - nrow(data_all2_filtered)/1E6, "millions\n")

# Show values of one paramerter / species before and after filtering 
param <- "CB52__WW"
spp <- "Gadus morhua"
gg <- bind_rows(
  data_all2 %>%
    filter(PARAM %in% param, LATIN_NAME %in% spp) %>% 
    mutate(filter = "1 - before filtering"), 
  data_all2_filtered %>%
    filter(PARAM %in% param, LATIN_NAME %in% spp) %>% 
    mutate(filter = "2 - after filtering")
  ) %>%
  ggplot(aes(YEAR, Concentration, color = is.na(FLAG1))) +
  geom_jitter(width = 0.25) +
  scale_y_log10() +
  labs(title = paste(param, "-", spp)) +
  facet_wrap(vars(filter))
if (show_plots) {gg} 

data_all2 <- data_all2_filtered

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Create 'df_series_sel'----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 


# Old:
# df_series_sel <- readxl::read_excel("Info/Proref table - MS version 17.xlsx")
# New: all combinations  
df_series_sel <- data_all2 %>%
  ungroup() %>%
  distinct(PARAM, LATIN_NAME, STATION_CODE) %>% 
  count(PARAM, LATIN_NAME) %>%
  rename(Species = LATIN_NAME)

# For testing 1 - just one parameter/species:
# df_series_sel <- df_series_sel %>% 
#   filter(PARAM == "CD__WW" & Species == "Gadus morhua")
# For testing 2 - a few random parameter/species:
# df_series_sel <- df_series_sel %>% sample_n(10)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Check parameters   
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 


do_check <- FALSE
# do_check <- TRUE

if (do_check){
  
  xtabs(~PARAM, data_all2 %>% filter(grepl("SCCP", PARAM)))
  xtabs(~PARAM, df_series_sel %>% filter(grepl("SCCP", PARAM)))
  
  xtabs(~PARAM, data_all2 %>% filter(grepl("PAH", PARAM)))
  xtabs(~PARAM, df_series_sel %>% filter(grepl("PAH", PARAM)))
  
  data_all2 %>%
    filter(PARAM %in% c("Krysen__WW", "CHR__WW", "BAP__WW")) %>%
    xtabs(~YEAR + PARAM, .)
  
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Save ------
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 

saveRDS(data_all2, "Data/54_data_2024_loqfilter3x.rds")  
saveRDS(df_series_sel, "Data/54_dataseries_2024_loqfilter3x.rds")  

