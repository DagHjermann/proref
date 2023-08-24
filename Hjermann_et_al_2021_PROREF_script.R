#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# R SCRIPT FOR THE PAPER:
#
# "Deriving Norwegian provisional high reference contaminant concentration (PROREF) 
#  in blue mussel (Mytilus edulis spp.) and Atlantic cod (Gadus morhua)"
#
# by
#
# Dag Ø. Hjermann, Merete Schøyen, Sigurd Øxnevad, Anders Ruus, and Norman W. Green
#
# The script needs two more files to run:
# The data: 
#   Hjermann_et_al_2021_PROREF_data.zip
# and a script with functions (which is sourced by this script):
#   Hjermann_et_al_2021_PROREF_functions.R
#
# If both of these files are in the working directory, this script should run without problems.
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

library(data.table)
library(dplyr)
library(purrr)
library(readr)

source("Hjermann_et_al_2021_PROREF_functions.R")

# Default png
png2 <- function(..., units = "in", res = 200, type="cairo", antialias="default")
  png(..., units = units, res = res, type=type, antialias=antialias)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 1 Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# For internal testing
# load(file = "../Milkys_2017/Data/12d_data_all.RData")

# range(data_all$SAMPLE_DATE, na.rm = TRUE)

data_all <- readr::read_csv(
  "Hjermann_et_al_2021_PROREF_data.zip", 
  col_types = cols(
    YEAR = col_double(),
    STATION_CODE = col_character(),
    STATION_NAME = col_character(),
    SAMPLE_DATE = col_integer(),
    TISSUE_NAME = col_character(),
    LATIN_NAME = col_character(),
    SAMPLE_NO = col_integer(),
    REPNO = col_integer(),
    PARAM = col_character(),
    UNIT = col_character(),
    VALUE_WW = col_double(),
    VALUE_DW = col_double(),
    VALUE_FB = col_double(),
    FLAG1 = col_character(),
    SAMPLE_ID2 = col_integer(),
    N = col_integer(),
    N_over_LOQ = col_integer(),
    P_overLOQ = col_double()),
  locale = locale(encoding = "UTF-8")
  
)

# readr::write_csv(data_all, "Hjermann_et_al_2021_PROREF_data.csv")

# table(data_all$STATION_CODE)

# set to be ordinary data frame (not tibble)
data_all <- data_all %>%
  as.data.frame()


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2 Calculating PROREF for selected determinands, only WW) ----
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
# - this is used in the case of values under LOQ
#
# NOTE: For a quick test, we suggest to set nrepl = 3
#
nrepl <- 3

# For a more thorough analysis:
# nrepl <- 21

# Set years to use as background years
#  - most of the industry had decreased their releases of contaminants by 1992
#  - we still use end year 2016 in order to not update the reference values too often 
years_backgr <- 1992:2016


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . b. The analysis itself ----
#    (ca 10 minutes)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# All cod (takes 2 minutes)
t0 <- Sys.time()
df_cod_ww <- par_cod %>% 
  map_df(~get_background_values_rep(., "Gadus morhua", "VALUE_WW", 
                                    data = subset(data_all, YEAR %in% years_backgr), rep = nrepl ))
Sys.time() - t0

# All blue mussel (takes 4 minutes)
t0 <- Sys.time()
df_bluemussel_ww <- par_bluemussel %>% 
  map_df(~get_background_values_rep(
    ., 
    "Mytilus edulis", 
    "VALUE_WW",
    data = subset(data_all, YEAR %in% years_backgr), 
    rep = nrepl))
Sys.time() - t0


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . c. summarize data sets ----  
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


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . d. Combine cod and blue mussel data ----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

proref_ww <- bind_rows(df1, df2) %>%
  mutate(PARAM = factor(PARAM, levels = par_order),
         LATIN_NAME = factor(LATIN_NAME, levels = c("Gadus morhua", "Mytilus edulis"))
  ) %>%                   # USe 'par_bluemussel' to order correctly
  arrange(LATIN_NAME, PARAM, TISSUE_NAME, Variable)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3. Plot some results ----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


library(ggplot2)

# Define a small function for plotting PROREF for one species
plot_proref <- function(species){
  gg <- ggplot(subset(proref_ww, LATIN_NAME == species),
           aes(PARAM, Q95)) +
    geom_point() +
    coord_flip() +
    labs(title = paste("PROREF values for", species),
         x = "PROREF (Q95)")
  print(gg)
}

# Plot PROREF values for cod
plot_proref("Gadus morhua")

# Plot PROREF values for blue mussel  
plot_proref("Mytilus edulis")


