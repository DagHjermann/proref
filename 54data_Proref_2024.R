

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
      grepl("SCCP__", PARAM) ~ sub("SCCP__", "SCCP eksl. LOQ__", PARAM),
      grepl("MCCP__", PARAM) ~ sub("MCCP__", "MCCP eksl. LOQ__", PARAM),
      grepl("Krysen__", PARAM) ~ sub("Krysen__", "CHR__", PARAM),
      TRUE ~ PARAM
    )
  )


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Create 'df_series_sel'----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 


# Old:
# df_series_sel <- readxl::read_excel("Info/Proref table - MS version 17.xlsx")
# New: all combinations  
df_series_sel <- data_all2 %>%
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

saveRDS(data_all2, "Data/54_data_2024.rds")  
saveRDS(df_series_sel, "Data/54_dataseries_2024.rds")  

