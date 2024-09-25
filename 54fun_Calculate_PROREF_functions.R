#
# Functions used by script 54  
#
#  . . . . . . . . . . . . .
# Depenndency tree
#  . . . . . . . . . . . . .
# get_background_values
#   find_tissue
#   get_lower_medians
#     get_rawdata
#   find_set_differences
#     find_set_difference
#       get_stationdata_by_rankrange
#         get_stationdata_by_rank
# get_conc_percentiles  
#   find_tissue
#  . . . . . . . . . . . . .

# 
# ** Utility **
# fact2char                     - change factor to character
# fact2char_df                  - change factor to character, for entire data frame
# ** Functions for picking background stations **
# get_rawdata                   - Picks raw data from the stations with enough data (at least 'min_years' with at least 'min_indiv_per_year' data per year)
# get_lower_medians             - Picks the stations with enough data, calculates annual medians or those stations,
# get_stationdata_by_rankrange  - As get_stationdata_by_rank, but returns station data (medians) for several data sets
# find_set_difference           - Find the statistical difference between 'data1' (data sets 1 to i-1) and data2 (data set i), where i = rank by median (low to high)
# find_set_differences          - Start with the cleanest station, add stations with increasing concentrations, test each station against the set of all cleanes ones
# get_background_values         - Sets up data, finds correct tissue and  runs 'get_lower_medians' 'get_background_values' 
# ** Functions for finding background levels from background stations **
# get_conc_percentiles          - finding background levels from background stations


library(stringr)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Utility functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Factor to character
fact2char <- function(x) {
  levels(x)[as.numeric(x)]
}

# Factor to character for data frame (i.e. for all 
fact2char_df <- function(df){
  for (i in 1:length(df)){
    if (class(df[[i]])[1] %in% "factor")
      df[[i]] <- fact2char(df[[i]])
  }
  df
}


#
# Find correct tissue
#
# 'before_underscores' is for use when determinants e.g. "HG__WW" not just "HG" 
# - if 'before_underscores' is TRUE, it will 

find_tissue <- function(determinant, species, before_underscores = FALSE){
  fish_species <- c("Gadus morhua", "Platichthys flesus", "Limanda limanda")
  if (before_underscores){
    determinant <- stringr::str_extract(determinant, "[^__]+")
  }
  if (species %in% fish_species & determinant %in% c("HG", "C/N")){
    tissue <- "Muskel"
  } else if (species %in% fish_species & determinant %in% c("BAP3O", "PA1O", "PYR1O")){
    tissue <- "Bile"
  } else if (species %in% fish_species & determinant %in% "ALAD"){
    tissue <- "Blood"
  } else if (species %in% fish_species & determinant != "HG"){
    tissue <- "Lever"
  } else if (species %in% c("Mytilus edulis", "Littorina littorea", "Nucella lapillus")){
    tissue <- "Whole soft body"
  } else {
    stop ("species must be 'Gadus morhua', Platichthys flesus', 'Limanda limanda, 'Mytilus edulis', 'Nucella littorea' or 'Littorina littorea'")
  }
  tissue
}
# find_tissue("CD", species = "Gadus morhua")
# find_tissue("HG", species = "Gadus morhua")
# find_tissue("CD", species = "Mytilus edulis")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for picking background stations ----
#    Principle: for a given compound, we sort stations from the cleanes to the dirtiest
#       We then add stations one by one, comparing the median of the last station to the median of the 
#       preceding stations.
#
# A bunch of functions, the last one ('differences_increasing_conc') uses all the preceding functions
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Picks raw data from the stations with enough data (at least 'min_years' with at least 'min_indiv_per_year' data oer year)
# The "focus variable" is renamed "Measurement"
#
# Input: names of parameter, species and tissue, plus name of the focus variable
# Output: data frame
#
get_rawdata <- function(par, sp, ti, 
                        variable_name = "VALUE_WW", 
                        data = data_all[sel_allplots, ],
                        min_years = 5, min_indiv_per_year = 10){
  sel_analysis <-  with(data, PARAM %in% par & LATIN_NAME %in% sp & TISSUE_NAME %in% ti) & is.finite(data[,variable_name])
  data_analysis <- data[sel_analysis,]
  tab <- xtabs(~YEAR + STATION_CODE, data_analysis, drop = TRUE)
  no_years <- apply(tab >= min_indiv_per_year, 2, sum)
  stations_over_minimum <- names(no_years)[no_years >= min_years]
  if (length(stations_over_minimum) > 0){
    data_analysis <- subset(data_analysis, STATION_CODE %in% stations_over_minimum)
    i <- which(colnames(data_analysis) %in% variable_name)
    colnames(data_analysis)[i] <- "Measurement"
    df <- data.frame(Variable = variable_name, 
                     data_analysis[,c("PARAM", "LATIN_NAME", "TISSUE_NAME","YEAR", "STATION_CODE", "Measurement","FLAG1")],
                     stringsAsFactors = FALSE)
    df <- fact2char_df(df)
  } else {
    cat("No stations have", min_years, "years of data (", par, sp, ti, variable_name, ")\n")
    df <- NULL
  }
  df
}

# debugonce(get_rawdata)
# x <- get_rawdata("HG", "Gadus morhua","Lever")
# x <- get_rawdata("CB118", "Gadus morhua","Lever")
# x <- get_rawdata("CB118", "Gadus morhua","Lever", variable_name = "conc_ww", data = cemp_comb3)
# str(x)

#
# for 'cemp_comb2' data (script 12b)
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Picks the stations with enough data, calculates annual medians or those stations,
#   and discards the 10% highest annual medians
#
# Input: names of parameter, species and tissue, plus name of the focus variable
# Output: a list of two:
#  1) a named list with one data frame per station
#  2) a vector of station-wise medians
#
# Depends on 
#   get_rawdata
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

get_lower_medians <- function(par, sp, ti, variable_name = "VALUE_WW", quantile = 0.9, data = data_all[sel_allplots, ],
                              min_years = 5, min_indiv_per_year = 10){
  
  # get raw data
  data_analysis <- get_rawdata(par = par, sp = sp, ti = ti, variable_name = variable_name, data = data, 
                               min_years = min_years, min_indiv_per_year = min_indiv_per_year)
  
  if (!is.null(data_analysis)){
    data_analysis$Species <- strsplit(data_analysis$LATIN_NAME[1], " ")[[1]][1]  # We keep only the first word of thw species
    
    # compute medians per year/station (we include 'Variable', 'PARAM' and 'Species' as well although it has only one value. Nice to have in the final data frame)
    data_medians <- data_analysis %>%
      group_by(Variable, PARAM, Species, STATION_CODE, YEAR) %>%
      dplyr::summarise(median = median(Measurement), n = n(), n_loq = sum(!grepl("<", FLAG1)), .groups = "drop")
    
    data_medians <- droplevels(data_medians)
    
    # change it to a list
    data_medians_list <- data_medians %>% split(.$STATION_CODE)   # see ?purrr::map2
    
    # for each station, discard the highest 10%:
    for (i in 1:length(data_medians_list)){
      # sort data by median
      data_medians_list[[i]] <- data_medians_list[[i]][order(data_medians_list[[i]]$median),]
      # find how many years we will pick (rounding down)
      no_of_years <- floor(nrow(data_medians_list[[i]])*quantile)
      # pick the 90% of years with lowest medians
      data_medians_list[[i]] <- data_medians_list[[i]][1:no_of_years,]
    }
    
    # In addition (for later use), we calculate the overall station median, i.e. station-wise median across years based on the per-year medians
    # This is later used to rank the stations by median value
    median_per_station <- data_medians_list %>% map_dbl(~median(.$median))
    
    result <- list(data_medians_list = data_medians_list, median_per_station = median_per_station)
  } else {
    result <- NULL
  }
  result
}

# debugonce(get_lower_medians)
# x <- get_lower_medians("PB", "Gadus morhua","Lever")
# str(x, 1)
# head(x$data_medians_list[[1]])

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Returns station data (medians) for data set number 'rank' (where ranked by increasing median)
#
# Input: the output from 'get_lower_medians', plus the rank that we want (i.e. 1 if we want the station with lowest values)
# Output: a list of two elements:
#   1) a character (name of the station)
#   2) a data frame (annual medians for that station)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

get_stationdata_by_rank <- function(object, rank){
  i <- which(rank(object$median_per_station, ties.method = "first") == rank)
  list(station = names(object$data_medians_list)[i],
       result = object$data_medians_list[[i]])
}

# get_stationdata_by_rank(x, 1)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# get_stationdata_by_rankrange
# As get_stationdata_by_rank, but returns station data (medians) for several data sets
#
# Input: the output from 'get_lower_medians', plus a vector of ranks that we want (i.e. 1:3 if we want the 3 stations with lowest values)
# Output: a list of two elements:
#   1) a character vector (name of the stations)
#   2) a single data frame (annual medians for stations)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

get_stationdata_by_rankrange <- function(object, ranks){
  result_list <- ranks %>% map(function(x) get_stationdata_by_rank(object, rank = x))
  stations <- result_list %>% map_chr(nth, 1)
  medians <- result_list %>% map_df(nth, 2)
  list(stations = stations, medians = medians)
}

# x2 <- get_stationdata_by_rankrange(x, 1)
# debugonce(get_stationdata_by_rankrange)
# x2 <- get_stationdata_by_rankrange(x, 6:7)
# str(x2, 1)
# View(x2$medians)

#o#o#o#o/#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Find the statistical difference between 'data1' (data sets 1 to i-1) and data2 (data set i), where i = rank by median (low to high)
# For instance, if i = 3, we compare the station with the 3rd "cleanest" (low concentrations) station with the 2 cleanest ones
# The medians of data sets 1 to i-1 are treated as one pooled data set in the analysis.
#
# The output contains which test has been performed, which variable that is uswed for concentration,
#   and test statistics. The result table also includes the maximum of the reduced data sets, "Max_reduced_data1" and "Upper_quantile2". Since the data sets
#   are already reduced by deleting the upper 10%, "Max_reduced_data1" is expected to be close to but not
#   identical to the upper quantile for the pooled data sets 1 to i-1.
#
# Input: the output from 'get_lower_medians', plus the number i
# Output: a data frame with 1 row
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

# Using Mann-Whitney test instead of parametric test
#
# Depands on
#   get_stationdata_by_rankrange

find_set_difference <- function(object, i){
  if (i > 1){
    data1 <- get_stationdata_by_rankrange(object, 1:(i-1))
    data2 <- get_stationdata_by_rankrange(object, i)
    data_for_test1 <- data.frame(set = "data1", median = data1$medians$median, n = data1$medians$n,
                                 stations = paste(data1$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test2 <- data.frame(set = "data2", median = data2$medians$median, n = data2$medians$n, 
                                 stations = paste(data2$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test <- rbind(data_for_test1, data_for_test2)
    # Parametric test (t-test)
    # testresult <- as.data.frame(summary(lm(median ~ set, data_for_test))$coef)[2,]
    # Non-parametric test (independent 2-group Mann-Whitney U Test)
    old.o <- options(warn = -1)
    testresult <- wilcox.test(median ~ set, data_for_test, exact = FALSE)
    options(old.o)
    txt <- paste(paste(data1$stations, collapse = ","), "vs.", data2$stations)
    result <- data.frame(Rank = i, Station = data2$stations, Test = txt, 
                         Variable = data1$medians$Variable[1],
                         Param = data1$medians$PARAM[1],
                         Species = data1$medians$Species[1],
                         Median1 = median(data1$medians$median), Median2 = median(data2$medians$median), 
                         Max_reduced_data1 = max(data1$medians$median), Upper_quantile2 = max(data2$medians$median),
                         W = testresult$statistic, P = testresult$p.value,
                         N2 = sum(data2$medians$n), 
                         Perc_over_loq2 = round(100*sum(data2$medians$n_loq)/sum(data2$medians$n),0), 
                         stringsAsFactors = FALSE)
    # colnames(result)[12:14] <- c("SE", "t", "P")      # note hard-coded column numbers
  } else if (i == 1){
    data2 <- get_stationdata_by_rankrange(object, i)
    result <- data.frame(Rank = i, Station = data2$stations, Test = "-", 
                         Variable = data2$medians$Variable[1],
                         Species = data2$medians$Species[1],
                         Param = data2$medians$PARAM[1],
                         Median1 = NA, Median2 = median(data2$medians$median), 
                         Max_reduced_data1 = NA, Upper_quantile2 = max(data2$medians$median),
                         W = NA, P = NA,
                         N2 = sum(data2$medians$n), 
                         Perc_over_loq2 = round(100*sum(data2$medians$n_loq)/sum(data2$medians$n),0), 
                         stringsAsFactors = FALSE)
  }
  result
}
# debugonce(find_set_difference)
# find_set_difference(x, 2)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Starting with the cleanest station and adding stations with increasing concetrations, we test each station against
#   the set of all cleanes ones
# So, first the cleanest station is compared with the second cleanest, then the 3rd station is compared with the two cleanest ones, etc.
#
# For explanation of output, see 'find_set_difference'
#
# Input: the output from 'get_lower_medians'
# Output: a data frame with n rows, where n = number of stations minus 1
#
# Depends on
#   find_set_differences
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

find_set_differences <- function(object){
  n <- length(object$data_medians_list)
  result <- 1:n %>% map_df(function(j) find_set_difference(object, j))
  result %>% mutate(Median_ratio = Median2/Median2[1])
}

# y <- find_set_differences(x)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# get_background_values 
#   Sets up data, finds correct tissue and  runs 'get_lower_medians' 'get_background_values' 
#   - new version: percentiles (for limits) is based on the full data
#
# Depends on  
#   find_tissue
#   get_lower_medians
#   find_set_differences
#   
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

get_background_values <- function(determinant, species, var_name, years_backgr = 1992:2016, 
                                  threshold_p = 0.10,
                                  data = subset(data_all, YEAR %in% years_backgr), ...){
  fish_species <- c("Gadus morhua", "Platichthys flesus", "Limanda limanda")
  data <- as.data.frame(data)
  if (species %in% fish_species & determinant != "ALAD"){ 
    min_indiv_per_year = 10
  } else if (species %in% fish_species){ 
    min_indiv_per_year = 10
  } else if (species %in% "Mytilus edulis"){
    min_indiv_per_year = 2
  } else if (species %in% c("Littorina littorea", "Nucella lapillus")){     # i.e., VDSI
    min_indiv_per_year = 1
  }
  tissue <- find_tissue(determinant = determinant, species = species, before_underscores = TRUE)
  sel_analysis <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & is.finite(data[,var_name]))
  df <- data[sel_analysis,] %>% as.data.frame()
  
  # Set less-thans to  a random number between 50 and 100% of the LOQ, using a uniform dsitribution)
  # sel <- df$FLAG1 %in% "<"
  # df$VALUE_WW[sel] <- runif(sum(sel), df$VALUE_WW[sel]*0.5, df$VALUE_WW[sel])
  # sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_DW)
  # df$VALUE_DW[sel] <- runif(sum(sel), df$VALUE_DW[sel]*0.5, df$VALUE_DW[sel])
  # sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_FB)
  # df$VALUE_FB[sel] <- runif(sum(sel), df$VALUE_FB[sel]*0.5, df$VALUE_FB[sel])
  
  unit <- unique(df$UNIT)
  if (length(unit) == 0)
    unit <- "no data"
  if (length(unit) > 1)
    unit <- ">1 unit"
  X <- get_lower_medians(par = determinant, sp = species, ti = tissue, 
                         variable = var_name, data = data, min_indiv_per_year = min_indiv_per_year, ...)
  if (!is.null(X) & !unit %in% c("no data", ">1 unit")){
    df_diff <- find_set_differences(X)
    i1 <- 1
    i2 <- which(df_diff$P < threshold_p)[1] - 1
    if (is.na(i2)){
      i2 <- nrow(df_diff)
    }
    stations <- df_diff$Station[i1:i2]
    sel_analysis <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & 
                            STATION_CODE %in% stations) &
      is.finite(data[,var_name])
    df <- data[sel_analysis,] %>% as.data.frame()
    N <- nrow(df)
    ci_q <- qt(p = c(0.95, 0.975), N)
    upper_CI_log <- mean(log(df[,var_name] + 0.001)) + ci_q*sd(log(df[,var_name] + 0.001))/sqrt(N)
    upper_CI <- exp(upper_CI_log) - 0.001
    upper_perc <- quantile(df[,var_name], c(0.5, 0.9, 0.95, 1))
    upper_perc <- quantile(df[,var_name], c(0.5, 0.9, 0.95, 1))
    result <- data.frame(PARAM = determinant, LATIN_NAME = species, TISSUE_NAME = tissue, Variable = var_name, UNIT = unit,
                         Years_start = head(years_backgr, 1),
                         Years_end = tail(years_backgr, 1),
                         Stations = paste(stations, collapse = ","),
                         N_stations = length(stations),
                         N = N, 
                         Median_lowest = upper_perc[1], 
                         Median_ratio = df_diff$Median_ratio[i2],
                         CI90 = upper_CI[1], CI95 = upper_CI[2], Q90 = upper_perc[2], Q95 = upper_perc[3], Max = upper_perc[4], 
                         stringsAsFactors = FALSE)
  } else {
    result <- data.frame(PARAM = determinant, LATIN_NAME = species, TISSUE_NAME = tissue, Variable = var_name, UNIT = unit,
                         Years_start = head(years_backgr, 1),
                         Years_end = tail(years_backgr, 1),
                         Stations = NA,
                         N_stations = NA,
                         N = NA,
                         Median_lowest = NA, Median_ratio = NA, Q90 = NA, Q95 = NA, Max = NA, 
                         stringsAsFactors = FALSE)
  }  
  df_diff <- df_diff %>%
    mutate(
      PARAM = determinant, LATIN_NAME = species, TISSUE_NAME = tissue, 
      Variable = var_name, UNIT = unit, .before = everything())
  list(
    result_one_line = result,
    differences = df_diff
  )
}

if (FALSE){
  # Usage
  # debugonce(get_background_values)
  one_case <- get_background_values(determinant = "DDEPP", species = "Gadus morhua", 
                                    var_name = "VALUE_WW", 
                                    years_backgr = 1992:2016,
                                    data = subset(data_all, YEAR %in% years_backgr))  
  
  str(one_case, 1)
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Functions for finding background levels from background stations ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##


get_conc_percentiles <- function(determinant,
                                 species,
                                 stationstring, 
                                 percentile = 0.95,
                                 var_name = "VALUE_WW", 
                                 data){
  data <- as.data.frame(data)
  tissue <- find_tissue(determinant = determinant, species = species, before_underscores = TRUE)
  sel_analysis <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & is.finite(data[,var_name]))
  stations <- strsplit(stationstring, split = ", ")[[1]] %>% gsub(" ", "", ., fixed = TRUE)
  sel_stations <-  with(data, STATION_CODE %in% stations)
  df <- data[sel_analysis & sel_stations,]
  quantile(df[[var_name]], probs = percentile)
  }

if (FALSE){
  # debugonce(get_conc_percentiles)
  get_conc_percentiles(determinant = "BDE47", 
                       species = "Mytilus edulis", 
                       stationstring = "98A2, 36A1", 
                       data = subset(data_all2, YEAR %in% years_backgr2))
  
  
}
