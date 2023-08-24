
# From: Norman Whitaker Green <norman.green@niva.no> 
#   Sent: mandag 4. januar 2021 13:04
# To: Anders Ruus <anders.ruus@niva.no>; Dag Øystein Hjermann <Dag.Hjermann@niva.no>
#   Cc: Jonny Beyer <Jonny.Beyer@niva.no>
#   Subject: RE: EQS biota - Referanser
# 
# Til Anders: Ad PFAS - la meg purre på Dag
# 
# Heisann Dag: er du hjemme? (dvs. er du på jobb?) Uansett, godt nytt år!
#   Som jeg (vi) skrev til deg før jul (11., 15., og 17. desember), har du anledning å se på hvorfor vi har ingen PROREF for PFOS og PFOA i blåskjell? Og hvis ikke kunne du «løsne» litt på PROREF kriteria og gi oss et forslag til PROREF, og formidle til oss hvilke kriteria du måtte bryte med? Vi trenger verdiene til denne rapporten til Miljødirektoratet innen i morgen. Klarer du det? 
#   
#   Mvh
# 
# Norman 

#
# Start of script - as 51 "main script"
#


data_all_original <- data_all
data_all_new <- readRDS("../Milkys2_pc/Files_from_Jupyterhub_2019/Raw_data/109_adjusted_data_2020-08-05.rds") %>%
  rename(YEAR = MYEAR) %>%
  as.data.frame()



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3 Example ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Making a sorted station list (from least to most contaminanted) and make a "background station" plot
# __Note__: slightly outdated, the procedure now is a bit different

### Step 1: get data series (for several stations)  
# Result: object with medians (per year) and medians (per station)

data_all %>%
  filter(PARAM == "PFOS") %>%
  xtabs(~STATION_CODE + YEAR + LATIN_NAME, .)

data_all %>%
  filter(PARAM == "PFOS" & LATIN_NAME == "Mytilus edulis") %>% # View()
  xtabs(~STATION_CODE + YEAR + LATIN_NAME, .)

data_all_new %>%
  filter(PARAM == "PFOS" & LATIN_NAME == "Mytilus edulis") %>% # View()
  xtabs(~STATION_CODE + YEAR + LATIN_NAME, .)

#
# View data
#
data_all_new %>%
  filter(PARAM == "PFOS" & LATIN_NAME == "Mytilus edulis") %>% View("PFOS")

data_all_new %>%
  filter(PARAM == "PFOA" & LATIN_NAME == "Mytilus edulis") %>% View("PFOA")

# debugonce(get_lower_medians)

#
# Original data and original threshold (min_years = 5, min_indiv_per_year = 2))
#
X <- get_lower_medians("PFOS", sp = "Mytilus edulis", ti = "Whole soft body", variable = "VALUE_WW", 
                       data = subset(data_all, YEAR %in% years_backgr),
                       min_indiv_per_year = 2)
# No stations have 5 years of data ( PFOS Mytilus edulis Whole soft body VALUE_WW )

#
# New data, and original thresholds ()
#
X <- get_lower_medians("PFOS", sp = "Mytilus edulis", ti = "Whole soft body", variable = "VALUE_WW", 
                       data = subset(data_all_new, YEAR %in% 1992:2019), 
                       min_years = 1, min_indiv_per_year = 2)
str(X, 1)


### Step 2: Find difference  between (example): station 1-3 vs. station 4, when stations are ordered in increasing order by median

find_set_difference(X, 4)


### Step 3: Find all differences 1 vs. 2, 1-2 vs. 3, 1-3 vs. 4, etc. (stations in increasing order)

# This does the same as find_set_differences(X) but returns NULL if there is no data 


df_pfos <- differences_increasing_conc("PFOS", sp = "Mytilus edulis", ti = "Whole soft body", variable = "VALUE_WW", 
                       data = subset(data_all_new, YEAR %in% 1992:2019), 
                       min_years = 1, min_indiv_per_year = 2)


df_pfoa <- differences_increasing_conc("PFOA", sp = "Mytilus edulis", ti = "Whole soft body", variable = "VALUE_WW", 
                            data = subset(data_all_new, YEAR %in% 1992:2019), 
                            min_years = 1, min_indiv_per_year = 2)


# PFOS

# `summarise()` regrouping output by 'Variable', 'PARAM', 'Species', 'STATION_CODE' (override with `.groups` argument)
# Rank Station                                                   Test Variable Species Param Median1 Median2 Max_medians1 Max_medians2    W            P
# ...1      1     22A                                                      - VALUE_WW Mytilus  PFOS      NA     0.1           NA          0.1   NA           NA
# W...2     2    28A2                                           22A vs. 28A2 VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1  2.0          NaN
# W...3     3     30A                                       22A,28A2 vs. 30A VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1  4.0          NaN
# W...4     4     36A                                   22A,28A2,30A vs. 36A VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1  3.0          NaN
# W...5     5    36A1                              22A,28A2,30A,36A vs. 36A1 VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1  3.5          NaN
# W...6     6     51A                          22A,28A2,30A,36A,36A1 vs. 51A VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1  4.0          NaN
# W...7     7    98A2                     22A,28A2,30A,36A,36A1,51A vs. 98A2 VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1  9.0          NaN
# W...8     8    I241                22A,28A2,30A,36A,36A1,51A,98A2 vs. I241 VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1 11.0          NaN
# W...9     9    I965           22A,28A2,30A,36A,36A1,51A,98A2,I241 vs. I965 VALUE_WW Mytilus  PFOS     0.1     0.1          0.1          0.1  6.5          NaN
# W...10   10    I969      22A,28A2,30A,36A,36A1,51A,98A2,I241,I965 vs. I969 VALUE_WW Mytilus  PFOS     0.1     0.3          0.1          0.3  0.0 0.0005120045
# W...11   11    I964 22A,28A2,30A,36A,36A1,51A,98A2,I241,I965,I969 vs. I964 VALUE_WW Mytilus  PFOS     0.1     0.5          0.3          0.5  0.0 0.0082938981

# PFOA

# `summarise()` regrouping output by 'Variable', 'PARAM', 'Species', 'STATION_CODE' (override with `.groups` argument)
# Rank Station                                                   Test Variable Species Param Median1 Median2 Max_medians1 Max_medians2    W         P
# ...1      1    I965                                                      - VALUE_WW Mytilus  PFOA      NA     0.4           NA          0.4   NA        NA
# W...2     2     22A                                           I965 vs. 22A VALUE_WW Mytilus  PFOA     0.4     0.5          0.4          0.5  0.0 0.4795001
# W...3     3    28A2                                      I965,22A vs. 28A2 VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  2.0 0.6830914
# W...4     4     30A                                  I965,22A,28A2 vs. 30A VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  4.0 0.7518296
# W...5     5     36A                              I965,22A,28A2,30A vs. 36A VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  3.0 1.0000000
# W...6     6    36A1                         I965,22A,28A2,30A,36A vs. 36A1 VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  3.5 1.0000000
# W...7     7     51A                     I965,22A,28A2,30A,36A,36A1 vs. 51A VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  4.0 1.0000000
# W...8     8    98A2                I965,22A,28A2,30A,36A,36A1,51A vs. 98A2 VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  9.0 0.8230633
# W...9     9    I241           I965,22A,28A2,30A,36A,36A1,51A,98A2 vs. I241 VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5 11.0 0.8382565
# W...10   10    I964      I965,22A,28A2,30A,36A,36A1,51A,98A2,I241 vs. I964 VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  6.5 1.0000000
# W...11   11    I969 I965,22A,28A2,30A,36A,36A1,51A,98A2,I241,I964 vs. I969 VALUE_WW Mytilus  PFOA     0.5     0.5          0.5          0.5  7.0 1.0000000



get_background_values(determinant = "PFOS", species = "Mytilus edulis", var_name = "VALUE_WW", 
                      data = subset(data_all_new, YEAR %in% 1992:2019), 
                      years_backgr = 2016:2019, 
                      min_years = 1)


get_back <- function(x) {
  get_background_values(determinant = x, species = "Mytilus edulis", var_name = "VALUE_WW", 
                                      data = subset(data_all_new, YEAR %in% 1992:2019), 
                                      years_backgr = 2016:2019, 
                                      min_years = 1)
}
get_back("PFOS")
1:5 %>% map_dfr(~get_back("PFOS"))
1:5 %>% map_dfr(~get_back("PFOA"))


### Step 4. Make plot of differences among stations in increasing order for one compound, species and tissue

plot_increase_determ("NI", species = "Gadus morhua", variable = "VALUE_WW", 
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

