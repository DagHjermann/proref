
# PROREF for sum parameters without LOQ   


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 0. Packages etc.----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# setwd("C:/Data/seksjon 212/Milkys")

library(data.table)
library(dplyr)
library(purrr)
#library(ggplot2)
#library(xlsx)

source("51fun_Calculate_PROREF_functions.R")

# Default png
png2 <- function(..., units = "in", res = 200, type="cairo", antialias="default")
  png(..., units = units, res = res, type=type, antialias=antialias)

# Set years to use for background
years_backgr <- 1992:2016

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# If you want to get original data - run start of 51 "main script"
#
# data_all_original <- data_all

# From "PFOS" version:
# data_all <- readRDS("../Milkys2_pc/Files_from_Jupyterhub_2019/Raw_data/109_adjusted_data_2020-08-05.rds") %>%
#   rename(YEAR = MYEAR) %>%
#   as.data.frame()

data_all <- readRDS("../Milkys2_pc/Files_from_Jupyterhub_2021/Raw_data/109_adjusted_data_2022-09-01.rds") %>%
  rename(YEAR = MYEAR) %>%
  as.data.frame()

data_all %>%
  filter(YEAR >= 2010 & grepl("exloq", PARAM)) %>% # View()
  xtabs(~PARAM + YEAR, .)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2 Make variables for number (N_over_LOQ) and proportion (P_over_LOQ) of data above LOQ ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# xtabs(~PARAM, data_all)
# xtabs(~STATION_CODE, data_all)

# str(data_all)

# Percentage of data above LOQ, by year and parameter
tab <- xtabs(~YEAR + PARAM + is.na(FLAG1), data_all)
# round(tab[,,"TRUE"]/apply(tab, 1:2, sum), 3)

data_all <- data_all %>%
  group_by(STATION_CODE, YEAR, LATIN_NAME, TISSUE_NAME, PARAM) %>%
  dplyr::mutate(
    N = n(), 
    N_over_LOQ = sum(!grepl("<", FLAG1)),
    P_overLOQ = round(N_over_LOQ/N, 3),
    VALUE_WW = round(VALUE_WW, 5), 
    VALUE_DW = round(VALUE_DW, 5), 
    VALUE_FB = round(VALUE_FB, 5)
  )

# Set to ordinary data frame
data_all <- as.data.frame(data_all)

# names(data_all) %>% dput()

x <- c("YEAR", "STATION_CODE", "STATION_NAME", "SAMPLE_DATE", "TISSUE_NAME", 
       "LATIN_NAME", "SAMPLE_NO", "REPNO", "PARAM", "UNIT", 
       "VALUE_WW", "VALUE_DW", "VALUE_FB", "FLAG1", 
       "SAMPLE_ID2", "N", "N_over_LOQ", "P_overLOQ")

setdiff(x, names(data_all))
# [1] "SAMPLE_NO"  "REPNO"      "SAMPLE_ID2"
# Not needed?






#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 4 Test function for replicated analysis ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


get_background_values_rep( 
  "BDE6S_exloq", "Gadus morhua", "VALUE_WW", 
  data = data_all, 
  years_backgr = years_backgr, 
  rep = 10)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 6 For table in paper (selected determinands, only WW) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . a. Define determinants ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

tab <- data_all %>%
  filter(grepl("exloq", PARAM)) %>%  # View()
  distinct(PARAM, LATIN_NAME, STATION_CODE) %>%
  count(PARAM, LATIN_NAME) %>%
  filter(n >= 15)


par_cod <- tab %>% filter(LATIN_NAME == "Gadus morhua") %>% pull(PARAM)
par_bluemussel <- tab %>% filter(LATIN_NAME == "Mytilus edulis") %>% pull(PARAM)

par_order <- c(par_bluemussel, "PFAS_exloq")

# check that par_order includes all
setdiff(par_cod, par_order)          # should be nothing
setdiff(par_bluemussel, par_order)   # should be nothing

#
# No of replicates - can be low since there are no 
#
# Proof:
# data_all %>% filter(grepl("exloq", PARAM)) %>% xtabs(~is.na(FLAG1), .)

nrepl <- 2

#
# Note: We use data since 1992 (not 1991 as said in M & M)
#
years_backgr <- 1992:2016

# Make it reproducible
set.seed(4321)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b. The analysis itself ----
#    (ca 10 minutes, see saved data)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# All cod (takes 2 minutes)
t0 <- Sys.time()
df_cod_ww <- par_cod %>% 
  map_df(~get_background_values_rep(
    ., 
    "Gadus morhua", 
    "VALUE_WW",
    years_backgr,
    data = subset(data_all, YEAR %in% years_backgr), 
    rep = nrepl ))
Sys.time() - t0

# saveRDS(df_cod_ww, "Data/51_df_cod_ww.rds")
# xtabs(~STATION_CODE, data_all)


# All blue mussel without "Bxx" stations (takes 4 minutes)
t0 <- Sys.time()
df_bluemussel_ww <- par_bluemussel %>% 
  map_df(~get_background_values_rep(
    ., 
    "Mytilus edulis", 
    "VALUE_WW", 
    years_backgr,
    data = subset(data_all, YEAR %in% years_backgr & substr(STATION_CODE,1,1) != "B"), 
    rep = nrepl))
Sys.time() - t0


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . d. Combine data (without Bxx stations) ----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

proref_ww <- bind_rows(
  df_cod_ww,
  df_bluemussel_ww) %>% 
  mutate(PARAM = factor(PARAM, levels = par_order),
         LATIN_NAME = factor(LATIN_NAME, levels = c("Gadus morhua", "Mytilus edulis"))
  ) %>%                   
  arrange(LATIN_NAME, PARAM, TISSUE_NAME, Variable)

#
# Have a look at data
#
proref_ww %>% 
  select(PARAM, LATIN_NAME, Variable, 
         N_stations_base, N_base, N_overLOQ_base,  
         Stations_proref, N_stations_proref, N_proref, N_overLOQ_proref, 
         Q95) %>%
  View()

library(ggplot2)

ggplot(proref_ww, aes(PARAM, Q95, color = LATIN_NAME)) +
  geom_point() +
  ggeasy::easy_rotate_x_labels(angle = -45) +
  scale_y_log10()

#
# Write combined data to Excel
#
names(proref_ww)

info <- data.frame(
  Column = c(
    "Variable",
    "Years_start", "Years_end", 
    "N_stations_base", "N_base", "N_overLOQ_base", 
    "Stations_proref", 
    "N_stations_proref", "N_proref", "N_overLOQ_proref", 
    "Median", "Q95", "Max"),
  Description = c(
    "VALUE_WW = wet weight",
    "First year used", "Last year used", 
    "Number of stations that PROREF could choose from",
    "Number of measurements that PROREF could choose from",
    "Number of above-LOQ measurements that PROREF could choose from",
    "PROREF (background) stations",
    "Number of stations in PROREF",
    "Number of measurements in PROREF stations",
    "Number of above-LOQ measurements in PROREF stations",
    "Median of measurements in PROREF stations", 
    "95th percentile of measurements in PROREF stations (the PROREF value itself!)", 
    "Max of measurements in PROREF stations")
)

openxlsx::write.xlsx(
  list(
    Data = proref_ww %>% select(-CI90, -CI95, -Q90, -Repl),
    Info = info),
  file = "Data/51 Proref - ww for sum determinands (excl LOQ).xlsx")



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3 Example, one parameter ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

### Step 1: get data series (for several stations)  
# Result: object with medians (per year) and medians (per station)

#
# View data
#
data_all %>%
  filter(PARAM == "BDE6S_exloq" & LATIN_NAME == "Gadus morhua") %>% View("BDE6S_exloq")

# debugonce(get_lower_medians)

#
# New data and original threshold (min_years = 5, min_indiv_per_year = 2))
#
rm(X)
X <- get_lower_medians("BDE6S_exloq", sp = "Gadus morhua", ti = "Lever", variable = "VALUE_WW", 
                       data = subset(data_all, YEAR %in% years_backgr),
                       min_indiv_per_year = 2, min_years = 5)
str(X)
# No stations have 5 years of data ( BDE6S_exloq Gadus morhua Lever VALUE_WW )


### Step 2: Find difference  between (example): station 1-3 vs. station 4, when stations are ordered in increasing order by median

find_set_difference(X, 4)


### Step 3: Find all differences 1 vs. 2, 1-2 vs. 3, 1-3 vs. 4, etc. (stations in increasing order)

# This does the same as find_set_differences(X) but returns NULL if there is no data 


test1 <- differences_increasing_conc("BDE6S_exloq", sp = "Gadus morhua", ti = "Lever", variable = "VALUE_WW", 
                       data = subset(data_all, YEAR %in% 1992:2019), 
                       min_years = 5, min_indiv_per_year = 2)



get_background_values(determinant = "PFOS", species = "Gadus morhua", var_name = "VALUE_WW", 
                      data = subset(data_all, YEAR %in% 1992:2019), 
                      years_backgr = 2016:2019, 
                      min_years = 1)


get_back <- function(x) {
  get_background_values(determinant = x, species = "Gadus morhua", var_name = "VALUE_WW", 
                                      data = subset(data_all, YEAR %in% 1992:2019), 
                                      years_backgr = 2016:2019, 
                                      min_years = 1)
}
get_back("PFOS")
1:5 %>% map_dfr(~get_back("PFOS"))
1:5 %>% map_dfr(~get_back("PFOA"))


### Step 4. Make plot of differences among stations in increasing order for one compound, species and tissue

plot_increase_determ("BDE6S_exloq", species = "Gadus morhua", variable = "VALUE_WW", 
                     data = subset(data_all, YEAR %in% years_backgr))


# debugonce(plot_increase)
# debugonce(differences_increasing_conc)
# debugonce(get_lower_medians)
# debugonce(get_rawdata)
plot_increase_determ("AS", species = "Gadus morhua", variable = "VALUE_WW", 
                     data = subset(data_all, YEAR %in% years_backgr), min_indiv_per_year = 2)

result <- get_background_values("AS", "Mytilus edulis", "VALUE_WW")
result
df <- 1:10 %>% map_df(~get_background_values("AS", "Mytilus edulis", "VALUE_WW"))
df

df <- get_data_step("AS", "Mytilus edulis", "VALUE_WW", step = 1)
nrow(df)


ggplot(df_alldata, aes(x = Step, group = Step, y = VALUE_WW)) + 
  geom_violin(width = 1)

### Step 5. Make all plots for a group of chemicals   
# Don't have to specify tissue; plot to file by default

determ <- c("CD", "PB", "HG", "AS", "CR", "ZN", "NI", "CO", "CU", "AG")
plot_increase_mult(determ, species = "Gadus morhua", variable = "VALUE_WW", 
                   data = subset(data_all, YEAR %in% years_backgr), 
                   determinantgroup = "metals", plot = "")



## get_background_values
# Note that we ran 'get_background_values_OLD3' with a bug in the code, resulting in
#   skipping the part where LOQ is replaced by a random number between 1/2 LOQ and LOQ.



tab <- xtabs(~is.na(FLAG1) + PARAM, subset(data_all, LATIN_NAME == "Gadus morhua"))
perc_lessthans <- tab[1,]/apply(tab,2,sum) *100
tab[,rev(order(perc_lessthans))][,1:60]

# debugonce(get_background_values)
debugonce(get_background_values)
get_background_values("HG", "Gadus morhua", "VALUE_WW")    # years_backgr = 1992:2016  # => N = 504
get_background_values("HG", "Gadus morhua", "VALUE_WW", years_backgr = 2002:2016)      # => N = 310
get_background_values_OLD3("HG", "Gadus morhua", "VALUE_WW")

get_background_values("HG", "Gadus morhua", "VALUE_WW")    # years_backgr = 1992:2016  # => N = 504

# Perform 10 times
df <- 1:10 %>% map_df(~get_background_values("HG", "Gadus morhua", "VALUE_WW"))
df


df <- 1:10 %>% map_df(~get_background_values("AS", "Mytilus edulis", "VALUE_WW"))
df

