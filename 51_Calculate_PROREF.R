# ---
# title: "51 Calculate PROREF"
# output: html_document
# ---

# As script 50, but we _don't_ start with making data with all values in one column (data_all2)  
# (slow, wasn't really worth it)
#
# This script writes data to Excel
# ("Data/51 Proref - ww for selected determinants.xlsx")


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
## 1 Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# files have been copied from C:\Users\DHJ\OneDrive - NIVA\Documents\seksjon 212\Milkys 2017\Analyse\Data

load(file = "Input_data/2017/12d_data_all.RData")
# data_all from 12b4, this is the one to use for raw data

# Comment out for speed.... so far
# load(file = "Milkys_2017/Data/12b3_data_all_2015sp.RData")        
# data_all_2015sp - the better length-adjusted data





#
# Some cleaning of TBT?
# Not done at this stage!
#


par <- "TBT"
yrs <- 2004
sts <- c("11G", "131G", "15G", "227G1", "227G2", "22G", "36G", "76G", "98G")
data_all %>% 
  filter(PARAM %in% par & YEAR %in% yrs & STATION_CODE %in% sts) %>%
  group_by(STATION_CODE) %>%
  arrange(YEAR, STATION_CODE) %>%
  select(YEAR, VALUE_WW) 

data_all %>% 
  filter(PARAM %in% par & STATION_CODE %in% sts) %>%
  group_by(YEAR) %>%
  summarise(n = n())

sel <- 
  with(data_all, PARAM %in% par & YEAR %in% c(1999:2000, 2002:2003) & STATION_CODE %in% sts) |
  with(data_all, PARAM %in% par & YEAR %in% 2006 & STATION_CODE %in% "131G") |
  with(data_all, PARAM %in% par & YEAR %in% 2010 & STATION_CODE %in% "22G")
sum(sel)

# We don't do this now
# data_all <- data_all[!sel,]

#
# combine 227G1 + 227G2 to '227G'
#
# xtabs(~STATION_CODE + YEAR + LATIN_NAME, subset(data_all, substr(STATION_CODE,1,4) %in% "227G"))
#
sel <- substr(data_all$STATION_CODE,1,4) %in% "227G"
sum(sel)
xtabs(~STATION_CODE + YEAR + LATIN_NAME, data_all[sel,])
# OK

data_all %>%
  count(STATION_CODE) %>%
  View()

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
x %in% names(data_all)

#
# Reduce number of parameters
#
data_all <- data_all[c("YEAR", "STATION_CODE", "STATION_NAME", "SAMPLE_DATE", "TISSUE_NAME", 
                         "LATIN_NAME", "SAMPLE_NO", "REPNO", "PARAM", "UNIT", 
                         "VALUE_WW", "VALUE_DW", "VALUE_FB", "FLAG1", 
                         "SAMPLE_ID2", "N", "N_over_LOQ", "P_overLOQ")]

#
# Fix data to our liking
#
sel <- data_all$LATIN_NAME %in% c("Littorina littorea", "Nucella lapillus"); sum(sel)
data_all$LATIN_NAME[sel] <- "Littorina/Nucella"



#
# Test  
#

# One replicate  
# debugonce(get_background_values)
get_background_values(determinant = "TBT", species = "Littorina/Nucella", var_name = "VALUE_WW", 
                      years_backgr = 1992:2016,
                      data = subset(data_all, YEAR %in% years_backgr), replicate_no = 99)

# x replicates    
get_background_values_rep(determinant ="TBT", species = "Littorina/Nucella", var_name = "VALUE_WW", 
                          years_backgr = 1992:2016,  
                          data = subset(data_all, YEAR %in% years_backgr), rep = 2)

# or:  
get_background_values_rep("TBT", "Littorina/Nucella", "VALUE_WW", 1992:2016,  
                         data = subset(data_all, YEAR %in% years_backgr), rep = 2)


#
# Save data (for paper)   
#
# Copied to `K:\Avdeling\Mar\NOG\Seksjon 212\2020\PROREF artikkelen`     
# and renamed 'Hjermann_et_al_2021_PROREF_data.csv'      
if (FALSE){

  data_save <- data_all %>%
    # pick cod and blue mussel data:
    filter(LATIN_NAME %in% c("Gadus morhua", "Mytilus edulis")) %>%
    filter(substr(STATION_CODE,1,1) != "B") %>%
    # set to be ordinary data frame (not tibble)
    as.data.frame()
  
  readr::write_csv(data_save, "Data/51_PROREF_data.csv")

}





#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# NOTE: For analysis for paper - you can jump from here 
#   to part 6 (about 2/3 down)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3 Example ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Making a sorted station list (from least to most contaminanted) and make a "background station" plot
# __Note__: slightly outdated, the procedure now is a bit different

ex_param <- "NI"
ex_sp <- "Gadus morhua"
ex_ti <- "Lever"
ex_var <- "VALUE_WW"

### Step 1: get data series (for several stations)  
# Result: object with medians (per year) and medians (per station)

# debugonce(get_lower_medians)
X <- get_lower_medians(ex_param, sp = ex_sp, ti = ex_ti, variable = ex_var, data = subset(data_all, YEAR %in% years_backgr))
str(X, 1)


### Step 2: Find difference  between (example): station 1-3 vs. station 4, when stations are ordered in increasing order by median

find_set_difference(X, 4)


### Step 3: Find all differences 1 vs. 2, 1-2 vs. 3, 1-3 vs. 4, etc. (stations in increasing order)

# This does the same as find_set_differences(X) but returns NULL if there is no data 
differences_increasing_conc(ex_param, sp = ex_sp, ti = ex_ti, variable = ex_var, 
                            data = subset(data_all, YEAR %in% years_backgr))


### Step 4. Make plot of differences among stations in increasing order for one compound, species and tissue

plot_increase_determ(ex_param, species = ex_sp, variable = ex_var, 
                     data = subset(data_all, YEAR %in% years_backgr))


# debugonce(plot_increase)
# debugonce(differences_increasing_conc)
# debugonce(get_lower_medians)
# debugonce(get_rawdata)
plot_increase_determ("AS", species = "Mytilus edulis", variable = "VALUE_WW", 
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
# debugonce(get_lower_medians)
get_background_values("HG", "Gadus morhua", "VALUE_WW")    # years_backgr = 1992:2016  # => N = 504

get_background_values("4-N-NP", "Gadus morhua", "VALUE_WW")    # years_backgr = 1992:2016  # => N = 504
get_background_values("HG", "Gadus morhua", "VALUE_WW", years_backgr = 2002:2016)      # => N = 310
get_background_values_OLD3("HG", "Gadus morhua", "VALUE_WW")

get_background_values("HG", "Gadus morhua", "VALUE_WW")    # years_backgr = 1992:2016  # => N = 504

# Perform 10 times
df <- 1:10 %>% map_df(~get_background_values("HG", "Gadus morhua", "VALUE_WW"))
df


df <- 1:10 %>% map_df(~get_background_values("AS", "Mytilus edulis", "VALUE_WW"))
df

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 4 Test function for replicated analysis ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


get_background_values_rep("HCHG", "Gadus morhua", "VALUE_WW", subset(data_all, YEAR %in% years_backgr), rep = 10)


# get_background_values_rep("HG", "Gadus morhua", "VALUE_WW", subset(data_all, YEAR %in% years_backgr), rep = 2)
#
# test blue mussel *with* Bxx stations
# get_background_values_rep("AS", "Mytilus edulis", "VALUE_WW", 
#                           subset(data_all, YEAR %in% years_backgr), rep = 2)
#
# test blue mussel *without* Bxx stations
# get_background_values_rep("AS", "Mytilus edulis", "VALUE_WW", 
#                           subset(data_all, YEAR %in% years_backgr & substr(STATION_CODE,1,1) != "B"), rep = 2)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5 Check cases with high PROREF ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# . a. Arsen in blue mussel ----
# 31A, B5, I301, I023, B2, 30A	
# PROREF = 3.32, Class I uppel limit = 0.602
# 
#

plot_increase_determ("AS", species = "Mytilus edulis", variable = "VALUE_WW", 
                     data = subset(data_all, YEAR %in% years_backgr), min_indiv_per_year = 2)

get_background_values("AS", "Mytilus edulis", "VALUE_WW", subset(data_all, YEAR %in% years_backgr),min_indiv_per_year = 2)


df <- differences_increasing_conc("AS", sp = "Mytilus edulis", ti = "Whole soft body", variable = "VALUE_WW", 
                                  data = subset(data_all, YEAR %in% years_backgr), min_indiv_per_year = 2)

# Pooled data for step 1, 2 etc. in the process
# df_alldata <- 1:result$N_stations_all %>% map_df(~get_data_step("AS", "Mytilus edulis", "VALUE_WW", step = .))
# First 20 stations
df_stepdata <- 1:20 %>% map_df(~get_data_step("AS", "Mytilus edulis", "VALUE_WW", step = .))
library(ggplot2)

# All data for all stations accepted (the data for the last step)
result$N_stations_all
df_alldata <- get_data_step("AS", "Mytilus edulis", "VALUE_WW", step = result$N_stations_all)
min(df_alldata$VALUE_WW)  # 0.79

# How often is VALUE_WW < 0.602?
df_alldata %>%
  group_by(STATION_CODE) %>%
  summarize(Median = quantile(VALUE_WW, 0.5),
            Class1 = mean(VALUE_WW < 0.602)*100) %>%
  arrange(desc(Class1))

# LOQ
df_loq <- subset(data_all, YEAR %in% years_backgr) %>%
  filter(!is.na(FLAG1)) %>%
  group_by(PARAM, TISSUE_NAME, YEAR) %>%
  summarize(N = n(),
            Min = min(VALUE_WW),
            Max = max(VALUE_WW))
df_loq %>% filter(PARAM %in% "AS")

df_minimum <- subset(data_all, YEAR %in% years_backgr) %>%
  group_by(PARAM, TISSUE_NAME, YEAR) %>%
  summarize(Min = min(VALUE_WW),
            Max = max(VALUE_WW))
df_minimum %>% 
  filter(PARAM %in% "AS" & TISSUE_NAME %in% "Whole soft body") %>%
  arrange(Min)

# Also with station
df_minimum2 <- subset(data_all, YEAR %in% years_backgr) %>%
  group_by(PARAM, TISSUE_NAME, STATION_CODE, YEAR) %>%
  summarize(Min = min(VALUE_WW),
            Max = max(VALUE_WW))
df_minimum2 %>% 
  filter(PARAM %in% "AS" & TISSUE_NAME %in% "Whole soft body") %>%
  arrange(Min)


# Summary of data

# Q95 and median 
df_summary1 <- df_stepdata %>%
  group_by(Step, Stations) %>%
  summarize(N = length(VALUE_WW), 
            Q95 = quantile(VALUE_WW, 0.95),
            Median_raw = quantile(VALUE_WW, 0.5))

# median of medians, but note: this does not remove the upper 10%!
df_summary2 <- df_stepdata %>%
  group_by(Step, Stations, YEAR, STATION_CODE) %>%
  summarize(Median = quantile(VALUE_WW, 0.5)) %>%
  ungroup() %>%
  group_by(Step, Stations) %>%
  summarize(Median_medians = quantile(Median, 0.5))

df_summary1 %>% nrow()
df_summary2 %>% nrow()
df_summary <- bind_cols(df_summary1, df_summary2["Median_medians"])
df_summary

ggplot(df_stepdata, aes(x = Step, y = VALUE_WW)) + 
  geom_jitter(aes(color = LOQ), width = 0.1) +
  scale_color_manual(values = c("black", "red")) +
  geom_point(data = df_summary, aes(y = Q95), shape = "-", size = rel(10), color = "red") +
  geom_point(data = df_summary, aes(y = Median_medians), shape = "-", size = rel(10), color = "blue")

ggplot(df_95perc, aes(x = Step, y = Q95)) + 
  geom_point() +
  geom_point(data = df_95perc, aes(y = Median), shape = "-", size = rel(10), color = "blue")

#
# . b. BAP in blue mussel ----
# PROREF = 1.3, Class I upper limit = 0.769
# 
#

plot_increase_determ("BAP", species = "Mytilus edulis", variable = "VALUE_WW", 
                     data = subset(data_all, YEAR %in% years_backgr), min_indiv_per_year = 2)

result <- get_background_values("BAP", "Mytilus edulis", "VALUE_WW")
result

# Pooled data for step 1, 2 etc. in the process
# df_alldata <- 1:result$N_stations_all %>% map_df(~get_data_step("AS", "Mytilus edulis", "VALUE_WW", step = .))
# First 20 stations
df_stepdata <- 1:20 %>% map_df(~get_data_step("BAP", "Mytilus edulis", "VALUE_WW", step = .))
library(ggplot2)

# All data for all stations accepted (the data for the last step)
result$N_stations_all  # 18
df_alldata <- get_data_step("BAP", "Mytilus edulis", "VALUE_WW", step = result$N_stations_all)
min(df_alldata$VALUE_WW)  # 0.2

# How often is VALUE_WW < 0.2?
df_alldata %>%
  group_by(STATION_CODE) %>%
  summarize(Median = quantile(VALUE_WW, 0.5),
            Class1 = mean(VALUE_WW < 0.769)*100) %>%
  arrange(desc(Class1))

df_alldata %>%
  group_by(STATION_CODE) %>%
  summarize(Median = quantile(VALUE_WW, 0.5),
            Class1 = mean(VALUE_WW < 1.3)*100) %>%
  arrange(desc(Class1))

x <- df_alldata %>%
  group_by(STATION_CODE, YEAR) %>%
  summarize(Median = quantile(VALUE_WW, 0.5)) %>%
  pull(Median)
mean(x > 0.769 & x < 1.3)*100 # 8.18

df_alldata %>%
  group_by(STATION_CODE, YEAR) %>%
  summarize(Median = quantile(VALUE_WW, 0.5)) %>%
  ggplot(aes(YEAR, Median, group = STATION_CODE, color = STATION_CODE)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = c(0.769, 1.3)) +
  scale_y_log10()



# LOQ
df_loq <- subset(data_all, YEAR %in% years_backgr) %>%
  filter(!is.na(FLAG1)) %>%
  group_by(PARAM, TISSUE_NAME, YEAR) %>%
  summarize(N = n(),
            Min = min(VALUE_WW),
            Max = max(VALUE_WW))
df_loq %>% filter(PARAM %in% "AS")



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


par_cod <- c("HG","CD","CU","PB","ZN","CB_S7","DDEPP","HCHG","HCB","4-N-NP","4-N-OP","4-T-NP","4-T-OP",
             "BDE47","BDE6S","BDESS","HBCDA","PFOA","PFOS","PFOSA","SCCP","MCCP")
par_bluemussel <- c("CD","CR","CU","HG","NI","PB","AG","ZN","AS","CB_S7","DDEPP","HCB",
                    "NAP","ANT","FLU","BAA","BAP","P_S","BDE47","BDE6S","HBCDA","SCCP","MCCP","TBT")
par_snails <- c("TBT")

par_order <- c("HG","CD","CR","CU","NI","PB","AG","ZN","AS",
               "CB_S7","DDEPP","HCHG","HCB",
               "4-N-NP","4-N-OP","4-T-NP","4-T-OP",
               "NAP","ANT","FLU","BAA","BAP","P_S",
               "BDE47","BDE6S","BDESS","HBCDA","PFOA","PFOS","PFOSA",
               "SCCP","MCCP","TBT")

# check that par_order includes all
mean(par_bluemussel %in% par_order)   # should equal 1
mean(par_cod %in% par_order)          # should equal 1

#
# No of replicates
#
nrepl <- 21

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


# All blue mussel (takes 4 minutes)
t0 <- Sys.time()
df_bluemussel_ww <- par_bluemussel %>% 
  map_df(~get_background_values_rep(
    ., 
    "Mytilus edulis", 
    "VALUE_WW",
    years_backgr,
    data = subset(data_all, YEAR %in% years_backgr), 
    rep = nrepl))
Sys.time() - t0

# All blue mussel without "Bxx" stations (takes 4 minutes)
t0 <- Sys.time()
df_bluemussel_ww_noB <- par_bluemussel %>% 
  map_df(~get_background_values_rep(
    ., 
    "Mytilus edulis", 
    "VALUE_WW", 
    years_backgr,
    data = subset(data_all, YEAR %in% years_backgr & substr(STATION_CODE,1,1) != "B"), 
    rep = nrepl))
Sys.time() - t0


# All snails (4 seconds)
t0 <- Sys.time()
df_snails_ww <- par_snails %>% 
  map_df(~get_background_values_rep(
    ., 
    "Littorina/Nucella", 
    "VALUE_WW",
    years_backgr,
    data = subset(data_all, YEAR %in% years_backgr & substr(STATION_CODE,1,1) != "B"), 
    rep = nrepl))
Sys.time() - t0

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Save results
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

if (FALSE){
  
  # Backups
  file.copy("Data/51_df_cod_ww.rds", "Data/51_df_cod_ww_backup20210512.rds")
  file.copy("Data/51_df_bluemussel_ww_noB.rds", "Data/51_df_bluemussel_ww_noB_backup20210512.rds.rds")
  file.copy("Data/51_df_snails_ww.rds", "Data/51_df_snails_ww_backup20210512.rds.rds")

  # Save
  saveRDS(df_cod_ww, file = "Data/51_df_cod_ww.rds")
  saveRDS(df_bluemussel_ww, file = "Data/51_df_bluemussel_ww.rds")
  saveRDS(df_bluemussel_ww_noB, file = "Data/51_df_bluemussel_ww_noB.rds")
  saveRDS(df_snails_ww, file = "Data/51_df_snails_ww.rds")
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# If you want to read from disk
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

if (FALSE){
  df_cod_ww <- readRDS(file = "C:/Data/seksjon 212/Milkys/Data/51_df_cod_ww.rds")
  df_bluemussel_ww <- readRDS(file = "C:/Data/seksjon 212/Milkys/Data/51_df_bluemussel_ww.rds")
  df_bluemussel_ww_noB <- readRDS(file = "C:/Data/seksjon 212/Milkys/Data/51_df_bluemussel_ww_noB.rds")
  df_snails_ww <- readRDS(file = "C:/Data/seksjon 212/Milkys/Data/51_df_snails_ww.rds")
  
  ?regex
  dir("Data", pattern = "51_df_cod_ww.*")
  df_cod_ww_old <- readRDS("Data/51_df_cod_ww_backup20210512.rds") 
  
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Summarize and combine data
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

middle_index <- (nrepl+1)/2   # = 11 if nrepl = 21

# For getting median result - simply sort by Q95 (see below) and extract line number middle_index
df1 <- df_cod_ww %>%
  group_by(PARAM, LATIN_NAME, TISSUE_NAME, Variable) %>%
  arrange(Q95) %>%
  summarize_all(list(function(x) x[middle_index] )) %>%
  ungroup()

df2 <- df_bluemussel_ww %>%
  group_by(PARAM, LATIN_NAME, TISSUE_NAME, Variable) %>%
  arrange(Q95) %>%
  summarize_all(list(function(x) x[middle_index] )) %>%
  ungroup()

df3 <- df_bluemussel_ww_noB %>%
  group_by(PARAM, LATIN_NAME, TISSUE_NAME, Variable) %>%
  arrange(Q95) %>%
  summarize_all(list(function(x) x[middle_index])) %>%
  ungroup()

df4 <- df_snails_ww %>%
  group_by(PARAM, LATIN_NAME, TISSUE_NAME, Variable) %>%
  arrange(Q95) %>%
  summarize_all(list(function(x) x[middle_index])) %>%
  ungroup()

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . c. Compare proref with and without Bxx stations ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

proref_bluemussel_ww_Bxx <- bind_rows(
  data.frame(df2, Include_Bxx = "Incl Bxx", stringsAsFactors = FALSE),
  data.frame(df3, Include_Bxx = "Excl Bxx", stringsAsFactors = FALSE)) %>% 
  mutate(PARAM = factor(PARAM, levels = par_bluemussel)) %>%                   # USe 'par_bluemussel' to order correctly
  arrange(PARAM, LATIN_NAME, TISSUE_NAME, Variable)

#
# Have a look at data
#
df <- proref_bluemussel_ww_Bxx %>% 
  select(PARAM, LATIN_NAME, Variable, Stations_proref, 
         N_stations_base, N_base, N_overLOQ_base, 
         N_stations_proref, N_proref, Stations_proref, Q95)
# View(df)

library(ggplot2)

ggplot(proref_bluemussel_ww_Bxx, aes(Include_Bxx, Q95, fill = Include_Bxx)) +
  geom_col(position = "dodge") +
  facet_wrap(~PARAM, scales = "free_y")

#
# Write "Compare proref with and without Bxx station" to Excel
#
proref_bluemussel_ww_Bxx %>% 
  select(-CI90, -CI95, -Q90) %>%
  openxlsx::write.xlsx(file = "Data/51 Compare proref with and without Bxx stations.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . d. Combine data (without Bxx stations) ----  
#
# I.e. we choose the 'without Bxx stations' variant!
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

proref_ww <- bind_rows(
  data.frame(df1, Include_Bxx = "Excl Bxx", stringsAsFactors = FALSE),
  data.frame(df3, Include_Bxx = "Excl Bxx", stringsAsFactors = FALSE),
  data.frame(df4, Include_Bxx = "Excl Bxx", stringsAsFactors = FALSE)) %>% 
  mutate(PARAM = factor(PARAM, levels = par_order),
         LATIN_NAME = factor(LATIN_NAME, levels = c("Gadus morhua", "Mytilus edulis", "Littorina/Nucella"))
  ) %>%                   # USe 'par_bluemussel' to order correctly
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

ggplot(proref_ww, aes(PARAM, Q95)) +
  geom_point() +
  ggeasy::easy_rotate_x_labels(angle = -45) +
  scale_y_log10()

#
# Write combined data to Excel
#

info <- data.frame(
  Column = c("Years_start", "Years_end", 
             "N_stations_base", "N_base", "N_overLOQ_base", 
             "Stations_proref", 
             "N_stations_proref", "N_proref", "N_overLOQ_proref", 
             "Median", "Q95", "Max", 
             "Include_Bxx"),
  Description = c("First year used", "Last year used", 
                  "Number of stations that PROREF could choose from",
                  "Number of measurements that PROREF could choose from",
                  "Number of above-LOQ measurements that PROREF could choose from",
                  "PROREF (background) stations",
                  "Number of stations in PROREF",
                  "Number of measurements in PROREF stations",
                  "Number of above-LOQ measurements in PROREF stations",
                  "Median of measurements in PROREF stations", 
                  "95th percentile of measurements in PROREF stations (the PROREF value itself!)", 
                  "Max of measurements in PROREF stations", 
                  "Whether 'B' blue mussel stations (industry stations) were included or not)")
)

openxlsx::write.xlsx(
  list(
    Data = proref_ww %>% select(-CI90, -CI95, -Q90, -Repl),
    Info = info),
  file = "Data/51 Proref - ww for selected determinants.xlsx")



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 7 Add 'base' data to existing results ----
#
# I.e., data on which stations/data PROREF had to choose form 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

dir("../Milkys2_pc/Files_to_Jupyterhub_2019")

# 
# . a Data set 1  ----
# - the Proref values used in the report for the 2017 data  
# - metadata that are added: df_meta1 
#
df_report <- readxl::read_excel("../Milkys2_pc/Files_to_Jupyterhub_2019/Proref_report_2017.xlsx")  

table(df_report$Basis)

i <- 1
df_report[1,]

get_base_metadata_s <- safely(get_base_metadata)

X <- seq_len(nrow(df_report)) %>%
  purrr::map(~get_base_metadata_s(df_report$PARAM[.], 
                             df_report$LATIN_NAME[.],
                             paste0("VALUE_", df_report$Basis[.]), 
                             years_backgr = years_backgr, 
                             data = subset(data_all, YEAR %in% years_backgr),
                             proref_stations = df_report$Stations[.])
  )

# X[[1]]
X2 <- transpose(X)

# Get those that returned data
ok <- X2[[2]] %>% map_lgl(is.null)
# mean(ok)
df_meta1 <- X2[[1]][ok] %>% bind_rows()     # note: df_meta1
# nrow(df_meta1)

# Add to original data   
df_report2 <- df_report %>%
  left_join(df_meta1)%>% 
  select(-Variable)


dir("../Milkys2_pc/Files_to_Jupyterhub_2019")

# 
# . b Data set 2 ----   
# - Used in manuscript
# - metadata that are added: df_meta2 
#

df_paper <- readxl::read_excel("../Milkys2_pc/Files_to_Jupyterhub_2019/Proref_paper.xlsx")  

get_base_metadata_s <- safely(get_base_metadata)

X <- seq_len(nrow(df_paper)) %>%
  purrr::map(~get_base_metadata_s(df_paper$PARAM[.], 
                                  df_paper$LATIN_NAME[.],
                                  df_paper$Variable[.],       # note difference from data set 1
                                  years_backgr = years_backgr, 
                                  data = subset(data_all, YEAR %in% years_backgr),
                                  proref_stations = df_paper$Stations[.])
  )

# X[[1]]
X2 <- transpose(X)

# Get those that returned data
ok <- X2[[2]] %>% map_lgl(is.null)
# mean(ok)
df_meta2 <- X2[[1]][ok] %>% bind_rows()     # note: df_meta2
# nrow(df_meta1)

# Add to original data   
df_paper2 <- df_paper %>%
  left_join(df_meta2) %>%
  select(-Variable)

#
# . c. Combine ----
#

if (FALSE){
  # check columns
  x <- names(df_paper2) %in% names(df_report2)
  names(df_paper2)[!x]
  x <- names(df_report2) %in% names(df_report2)
  names(df_report3)[!x]
}

# Remove those from 'report' that are in 'paper':
df_report3 <- anti_join(df_report2, df_paper2, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis", "UNIT"))
nrow(df_report2)
nrow(df_report3)

df_combined <- bind_rows(
  df_report3,
  df_paper2 %>% select(-N_stations_all, -N_all, -Include_Bxx)
)

# Define columns and order
columns <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis",
             "Years_start", "Years_end", 
             "Stations", "N_stations", "N", "N_overLOQ", 
             "Q95", "Median", "Max",
             "N_stations_base1", "N_base1", "N_overLOQ_base1", 
             "N_stations_base2", "N_base2", "N_overLOQ_base2")

df_combined <- df_combined[columns]

  
# names(df_combined) %>% dput()


info <- data.frame(
  Column = columns,
  Description = c(
    "Parameter", "", "", "Wte weight dry weight, fat basis",
    "First year used", "Last year used", 
    "PROREF (background) stations",
    "Number of PROREF (background) stations",
    "Number of measurements in PROREF stations",
    "Number of above-LOQ measurements in PROREF stations",
    "95th percentile of measurements in PROREF stations (the PROREF value itself!)", 
    "Median of measurements in PROREF stations", 
    "Maximum of measurements in PROREF stations", 
    "Number of stations with determinant, species, tissue, basis",
    "Number of measurements with determinant, species, tissue, basis",
    "Number of above-LOQ measurements with determinant, species, tissue, basis",
    "Number of stations with enough years (5) with enough data",
    "Number of measurements with enough years (5) with enough data",
    "Number of above-LOQ measurements with enough years (5) with enough data")
)
info


openxlsx::write.xlsx(
  list(
    Data = df_combined,
    Info = info),
  file = "PROREF/51 Proref - ww for selected determinants 14-05-2021.xlsx")








