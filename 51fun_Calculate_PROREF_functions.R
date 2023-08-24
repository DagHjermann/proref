#
# Functions used by 13d
# Based on 13a except that it doesn't depend an 'data_all'
# 
# Depends on the script file '00_functions_ranklineplot.R'
#
# fact2char
# fact2char_df
# plot_perc                     - Function for plotting for a given par, sp, ti
# plot_perc_save                - Function for plotting for a given par, sp, ti and saving plot to file
# get_rawdata                   - Picks raw data from the stations with enough data (at least 'min_years' with at least 'min_indiv_per_year' data per year)
# get_lower_medians             - Picks the stations with enough data, calculates annual medians or those stations,
# get_stationdata_by_rank       - Returns station data (medians) for data set number 'rank' (where ranked by increasing median)
# get_stationdata_by_rankrange  - As get_stationdata_by_rank, but returns station data (medians) for several data sets
# find_set_difference           - Find the statistical difference between 'data1' (data sets 1 to i-1) and data2 (data set i), where i = rank by median (low to high)
# compare_groups                - Compare two groups of stations
# find_set_differences          - Start with the cleanest station, add stations with increasing concentrations, test each station against the set of all cleanes ones
# differences_increasing_conc   - Runs 'get_lower_medians' and feeds the result into 'find_set_differences'
# plot_increase                 - Plots median of the newly added station, and the median of all cleaner stations
# plot_increase_upper           - Plots the upper percentile of the newly added station, and the median of all cleaner stations

# Main function used in results is 'get_background_values_rep':
# get_background_values_rep 
# - get_background_values (called 21 times)
#   - finds min_indiv_per_year, tissue
#   - get_lower_medians (Picks the stations with enough data, calculates annual medians or those stations, and discards the 10% highest annual medians)
#   - find_set_differences (Starting with the cleanest station and adding stations with increasing concetrations)
#   - find_set_difference
#   - From differences, find first P-value lower than critical P (= 0.10) 
#   - Creates 'df_proref' (pool data of all background stations, from original data set)
#   - Returns Q95 (95 percentile), names of statoins etc.
# - combines Q95 etc. from the 21 replicates 


# Factor to character
fact2char <- function(x) levels(x)[as.numeric(x)]

# Factor to character for data frame (i.e. for all 
fact2char_df <- function(df){
  for (i in 1:length(df)){
    if (class(df[[i]])[1] %in% "factor")
      df[[i]] <- fact2char(df[[i]])
  }
  df
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. Functions for plotting ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# this stuff is commented out in order to be independent of 'data_all'
#
# Find stations used in 2014-2016
#tab <- xtabs(~STATION_CODE, subset(data_all, YEAR %in% 2014:2016), drop = TRUE)
#pick_stations <- names(tab)
#rm(tab)

# Get data since 1991 and including those stations
#sel_allplots <- with(data_all, YEAR >= 1991 & STATION_CODE %in% pick_stations)






#
# Function for plotting for a given par, sp, ti
# Including plotting station-wise quantile (based on raw data, not medians)
#
plot_perc <- function(par, sp, ti, quantile = 0.9, data = data_all[sel_allplots, ],
                      min_years = 10, min_indiv_per_year = 10
){
  sel_plot <-  with(data, PARAM %in% par & LATIN_NAME %in% sp & TISSUE_NAME %in% ti & !is.na(VALUE_WW))
  data_plot <- data[sel_plot,]
  tab <- xtabs(~YEAR + STATION_CODE, data_plot, drop = TRUE)
  no_years <- apply(tab >= min_indiv_per_year, 2, sum)
  stations_over_minimum <- names(no_years)[no_years >= min_years]
  data_plot <- subset(data_plot, STATION_CODE %in% stations_over_minimum)
  data_plot$Over_LOQ <- "Yes"
  data_plot$Over_LOQ[grepl("<", data_plot$FLAG1)] <- "No"
  
  if (nrow(data_plot) > 30){
    # 0.90 percentiles for each station
    df_quant <- data_plot %>%
      group_by(STATION_CODE) %>%
      dplyr::summarise(upper_quant = quantile(VALUE_WW, probs = quantile, na.rm = TRUE),
                       .groups = "drop")
    
    gg <- ggplot(data_plot, aes(YEAR, VALUE_WW, color = Over_LOQ)) + 
      geom_jitter(width = 0.3, size = rel(0.5)) +
      geom_hline(data = df_quant, aes(yintercept = upper_quant), linetype = 2) +
      geom_text(data = df_quant, aes(y = upper_quant, label = round(upper_quant, 3)), x = 2012, vjust = -0.3, col = "darkred", size = 3) +
      scale_y_log10() + 
      facet_wrap(~STATION_CODE) +
      theme_bw() +
      ggtitle(paste0(par, " in ", sp, " ", ti, " (Upper ", 100*quantile, "% percentiles)"))
  } else {
    gg <- NULL
  }
  gg
}

# Test
# debugonce(plot_perc)
# gg <- plot_perc("CB28", "Gadus morhua","Lever")
# gg


# Function for plotting for a given par, sp, ti and saving plot to file
plot_perc_save <- function(par, sp, ti, quantile = 0.9, data = data_all[sel_allplots, ]){
  gg <- plot_perc(par=par, sp=sp, ti=ti, quantile = quantile, data = data)
  ggsave(paste0("Figures_13/Q", quantile*1000, "_", par, "_", sp, "_", ti, ".png"), gg, width = 10, height = 7.5)
}


# Test
# plot_perc_save("CB180", "Gadus morhua","Lever")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Functions for picking background stations ----
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
      dplyr::summarise(median = median(Measurement), n = n(), n_loq = sum(!grepl("<", FLAG1)),
                       .groups = "drop")
    
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
# USing the normal approximation (exact = FALSE) by default

find_set_difference <- function(object, i, exact = FALSE){
  if (i > 1){
    data1 <- get_stationdata_by_rankrange(object, 1:(i-1))
    data2 <- get_stationdata_by_rankrange(object, i)
    data_for_test1 <- data.frame(set = "data1", median = data1$medians$median, 
                                 stations = paste(data1$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test2 <- data.frame(set = "data2", median = data2$medians$median, 
                                 stations = paste(data2$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test <- rbind(data_for_test1, data_for_test2)
    # Parametric test (t-test)
    # testresult <- as.data.frame(summary(lm(median ~ set, data_for_test))$coef)[2,]
    # Non-parametric test (independent 2-group Mann-Whitney U Test)
    old.o <- options(warn = -1)
    testresult <- wilcox.test(median ~ set, data_for_test, exact = exact)
    options(old.o)
    txt <- paste(paste(data1$stations, collapse = ","), "vs.", data2$stations)
    result <- data.frame(Rank = i, Station = data2$stations, Test = txt, 
                         Variable = data1$medians$Variable[1],
                         Param = data1$medians$PARAM[1],
                         Species = data1$medians$Species[1],
                         Median1 = median(data1$medians$median), Median2 = median(data2$medians$median), 
                         Max_medians1 = max(data1$medians$median), Max_medians2 = max(data2$medians$median),
                         W = testresult$statistic, P = testresult$p.value, stringsAsFactors = FALSE)
    # colnames(result)[12:14] <- c("SE", "t", "P")      # note hard-coded column numbers
  } else if (i == 1){
    data2 <- get_stationdata_by_rankrange(object, i)
    result <- data.frame(Rank = i, Station = data2$stations, Test = "-", 
                         Variable = data2$medians$Variable[1],
                         Species = data2$medians$Species[1],
                         Param = data2$medians$PARAM[1],
                         Median1 = NA, Median2 = median(data2$medians$median), 
                         Max_medians1 = NA, Max_medians2 = max(data2$medians$median),
                         W = NA, P = NA, stringsAsFactors = FALSE)
  }
  result
}

# debugonce(find_set_difference)
# find_set_difference(x, 2)


#
# Compare two groups of stations
# Note: parametric test (t-test)
#
compare_groups <- function(object, indices1, indices2){
  if (max(indices1,indices2) <= length(object$data_medians_list)){
    data1 <- get_stationdata_by_rankrange(object, indices1)
    data2 <- get_stationdata_by_rankrange(object, indices2)
    data_for_test1 <- data.frame(set = "data1", median = data1$medians$median, 
                                 stations = paste(data1$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test2 <- data.frame(set = "data2", median = data2$medians$median, 
                                 stations = paste(data2$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test <- rbind(data_for_test1, data_for_test2)
    testresult <- as.data.frame(summary(lm(median ~ set, data_for_test))$coef)[2,]
    txt <- paste(paste(data1$stations, collapse = ","), "vs.", 
                 paste(data2$stations, collapse = ",")
    )
    result <- data.frame(Indices1 = paste(indices1, collapse = ","), Indices2 = paste(indices2, collapse = ","), 
                         Stations = txt, 
                         Variable = data1$medians$Variable[1],
                         Param = data1$medians$PARAM[1],
                         Species = data1$medians$Species[1],
                         Median1 = median(data1$medians$median), Median2 = median(data2$medians$median), 
                         Max_reduced_data1 = max(data1$medians$median), 
                         Upper_quantile2 = max(data2$medians$median),
                         testresult, stringsAsFactors = FALSE)
    colnames(result)[ncol(result) + seq(-2,0)] <- c("SE", "t", "P")      # note hard-coded column numbers
  } else {
    result <- NULL
  }
  result
}

# X <- get_lower_medians("NI", sp = "Gadus morhua", ti = "Lever", variable = "VALUE_WW", data = data_all)
# compare_groups(X, 1:2, 3:4)
# compare_groups(X, 1:2, 10:11)


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
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

find_set_differences <- function(object){
  n <- length(object$data_medians_list)
  1:n %>% map_df(function(j) find_set_difference(object, j))
}

# y <- find_set_differences(x)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
# 
# Runs 'get_lower_medians' and feeds the result into 'find_set_differences'
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

# Used to make "increasing concentrations" table 
# Used by
#   plot_increase (and plot_increase_upper) 
#   differences_increasing_conc_mult
#   plot_rank
# 


differences_increasing_conc <- function(...){
  data_object <- get_lower_medians(...)
  if (!is.null(data_object)){
    result <- find_set_differences(data_object)
  } else {
    result <- NULL
  }
  result
}

#debugonce(differences_increasing_conc)
#debugonce(find_set_differences)
# y <- differences_increasing_conc("PB", "Gadus morhua","Lever","VALUE_WW")
# y <- differences_increasing_conc("CD", "Gadus morhua","Lever","VALUE_WW")
# plot(P ~ Rank, y, type = "b")

# test
# c("PB", "CD") %>% map_df(function(x) differences_increasing_conc(x, "Gadus morhua","Lever","VALUE_WW"))


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Function for making median plot ----
#
#    Principle: for a given compound, we sort stations from the cleanes to the dirtiest
#       We then add stations one by one, comparing the median of the last station to the median of the 
#       preceding stations.
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

#
# Plots median of the newly added station, and the median of all cleaner stations
#
# "#fdcc8a","#fc8d59","#d7301f"
# plot(1:3, 1:3, col = c("#FDCC8A", "#FC8D59", "D7301F"))


plot_increase <- function(par, sp, ti, variable_name, quantile = 0.9, data = data_all[sel_allplots, ],
                          min_years = 5, min_indiv_per_year = 10, threshold_p = c(0.1, 0.05, 0.01), cols = c("#FDCC8A", "#FC8D59", "#D7301F")){
  df <- differences_increasing_conc(par=par, sp=sp, ti=ti, variable_name=variable_name, quantile = quantile, data = data,
                                    min_years = min_years, min_indiv_per_year = min_indiv_per_year)
  # if (par == "AG") browser()
  # browser()
  if (!is.null(df)){
    plot(Median2 ~ Rank, df, type = "c", ylim = range(0, Median2),
         main = paste0(par, " in ", sp, " ", ti), ylab = variable_name, axes = FALSE)
    axis(1, labels = df$Station, at = df$Rank, las = 3)
    axis(2)
    box()
    points(Median2 ~ Rank, df[1,],                                               pch = 1, cex = 1.4)
    points(Median2 ~ Rank, subset(df, P > threshold_p[1]),                       pch = 1, cex = 1.4)
    points(Median2 ~ Rank, subset(df, P <= threshold_p[1] & P > threshold_p[2]), pch = 21, cex = 1.4, bg = cols[1])
    points(Median2 ~ Rank, subset(df, P <= threshold_p[2] & P > threshold_p[3]), pch = 21, cex = 1.4, bg = cols[2])
    points(Median2 ~ Rank, subset(df, P <= threshold_p[3]),                      pch = 21, cex = 1.4, bg = cols[3])
    lines(Median1 ~ Rank, df, type = "b", pch = 19, col = "blue")
  }
}

#
# Plots the upper percentile of the newly added station, and the median of all cleaner stations
#
plot_increase_upper <- function(par, sp, ti, variable_name, quantile = 0.9, data = data_all[sel_allplots, ],
                                min_years = 5, min_indiv_per_year = 10, threshold_p = 0.05){
  df <- differences_increasing_conc(par=par, sp=sp, ti=ti, variable_name=variable_name, quantile = quantile, data = data,
                                    min_years = min_years, min_indiv_per_year = min_indiv_per_year)
  if (!is.null(df)){
    plot(Upper_quantile2 ~ Rank, df, type = "l", lty = "dashed", ylim = range(0, Upper_quantile2),
         main = paste0(par, " in ", sp, " ", ti), ylab = variable_name, axes = FALSE)
    axis(1, labels = df$Station, at = df$Rank, las = 3)
    axis(2)
    box()
    lines(Median2 ~ Rank, df, type = "b", col = "black")
    points(Median2 ~ Rank, subset(df, P < threshold_p), pch = 20, col = "red")
    lines(Median1 ~ Rank, df, type = "b", col = "blue")
    lines(Max_reduced_data1 ~ Rank, df, type = "l", lty = "dashed", col = "blue")
  }
}

# plot_increase("PB", "Gadus morhua","Lever","VALUE_WW")
# plot_increase("HG", "Gadus morhua","Lever","VALUE_WW")
# plot_increase("PB", "Mytilus edulis", "Whole soft body", "VALUE_WW", min_indiv_per_year = 2)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Functions for easier plotting of multiple plots ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

# xtabs(~PARAM + TISSUE_NAME, subset(data_all, PARAM %in% c("ALAD", "PYR1O", "VDSI", "TBT", "TBTIN")))
# xtabs(~addNA(TISSUE_NAME), subset(data_all, LATIN_NAME %in% "Gadus morhua"))
# xtabs(~PARAM, subset(data_all, LATIN_NAME %in% "Gadus morhua" & TISSUE_NAME %in% "Bile"))
# xtabs(~PARAM, subset(data_all, LATIN_NAME %in% "Gadus morhua" & TISSUE_NAME %in% "Bladder"))



#
# Find correct tissue, and run plot_increase (for a single subplot)
#
find_tissue <- function(determinant, species){
  if (species == "Gadus morhua" & determinant %in% c("HG", "C/N")){
    tissue <- "Muskel"
  } else if (species %in% "Gadus morhua" & determinant %in% c("BAP3O", "PA1O", "PYR1O")){
    tissue <- "Bile"
  } else if (species %in% "Gadus morhua" & determinant %in% "ALAD"){
    tissue <- "Blood"
  } else if (species %in% "Gadus morhua" & determinant != "HG"){
    tissue <- "Lever"
  } else if (species %in% c("Mytilus edulis", "Littorina littorea", "Nucella lapillus", "Littorina/Nucella")){
    tissue <- "Whole soft body"
  } else {
    stop ("species must be 'Gadus morhua', 'Mytilus edulis', 'Nucella littorea', 'Littorina littorea', or 'Littorina/Nucella'")
  }
  tissue
}
# find_tissue("CD", species = "Gadus morhua")
# find_tissue("HG", species = "Gadus morhua")
# find_tissue("CD", species = "Mytilus edulis")

#
# Find correct tissue, and run plot_increase (for a single subplot)
#
plot_increase_determ <- function(determinant, species, variable, min_years = 5, ...){
  tissue <- find_tissue(determinant = determinant, species = species)
  plot_increase(determinant, species, tissue, variable, min_years = min_years, ...)
}
# plot_increase_determ(determ[1], species = "Gadus morhua", variable = "VALUE_WW")
# plot_increase_determ(determ[1], species = "Mytilus edulis", variable = "VALUE_WW", min_indiv_per_year = 2)


#
# Make file name given species, variable, determinantgroup
#
plot_make_filename <- function(species, variable, determinantgroup, file_suffix,
                               file_prefix = "Figures_13/13b2_"){
  if (species == "Gadus morhua"){
    file_name <- paste0(file_prefix, determinantgroup, "_gadus_")
  } else if (species == "Mytilus edulis"){
    file_name <- paste0(file_prefix, determinantgroup, "_mytil_")
  }
  if (variable == "VALUE_WW"){
    file_name <- paste0(file_name, "ww_", file_suffix)
  } else if (variable == "VALUE_DW"){
    file_name <- paste0(file_name, "dw_", file_suffix)
  } else if (variable == "VALUE_FB"){
    file_name <- paste0(file_name, "fb_", file_suffix)
  }
  if (variable %in% c("VALUE_WW", "conc_ww")){
    file_name <- paste0(file_name, "ww_", file_suffix)
  } else if (variable %in% c("VALUE_DW", "conc_dw")){
    file_name <- paste0(file_name, "dw_", file_suffix)
  } else if (variable %in% c("VALUE_FB", "conc_fb")){
    file_name <- paste0(file_name, "fb_", file_suffix)
  } else if (variable %in% c("VALUE_WW50", "conc_ww50")){
    file_name <- paste0(file_name, "wwl_", file_suffix)
  } else if (variable %in% c("VALUE_DW50", "conc_dw50")){
    file_name <- paste0(file_name, "dwl_", file_suffix)
  } else if (variable %in% c("VALUE_FB50", "conc_fb50")){
    file_name <- paste0(file_name, "fbl_", file_suffix)
  }
  file_name
}

# plot_make_filename(species = "Gadus morhua", variable = "VALUE_WW", determinantgroup = "metals", file_suffix = "medians.png")

#
# Make plot title (in outer margin) given species, variable, determinantgroup
#
plot_make_title <- function(species, variable, determinantgroup){
  if (variable %in% c("VALUE_WW", "conc_ww")){
    tit <- paste(determinantgroup, "on wet-weight basis,")
  } else if (variable %in% c("VALUE_DW", "conc_dw")){
    tit <- paste(determinantgroup, "on dry-weight basis,")
  } else if (variable %in% c("VALUE_FB", "conc_fb")){
    tit <- paste(determinantgroup, "on fat basis,")
  } else if (variable %in% c("VALUE_WW50", "conc_ww50")){
    tit <- paste(determinantgroup, "on wet-weight basis, length-adjusted,")
  } else if (variable %in% c("VALUE_DW50", "conc_dw50")){
    tit <- paste(determinantgroup, "on dry-weight basis, length-adjusted,")
  } else if (variable %in% c("VALUE_FB50", "conc_fb50")){
    tit <- paste(determinantgroup, "on fat basis, length-adjusted,")
  }
  if (species == "Gadus morhua"){
    tit <- paste(tit, "cod")
  } else if (species == "Mytilus edulis"){
    tit <- paste(tit, "blue mussel")
  }
  tit <- paste0(toupper(substring(tit,1,1)), substring(tit,2))
  tit
}
# plot_make_title(species = "Gadus morhua", variable = "VALUE_WW", determinantgroup = "metals")

#
# Make multiple "increase" plots (single plot with subplots)
#
plot_increase_mult <- function(determinands, species, variable, determinantgroup, subplots = c(2,5), plot = "file", ...){
  fn <- plot_make_filename(species = species, variable = variable, determinantgroup = determinantgroup, file_suffix = "medians.png")
  if (plot == "file"){
    png2(fn, subplots[2]*2.5, subplots[1]*2.5)
  } else if (plot == "window") {
    windows(subplots[2]*2.5, subplots[1]*2.5)
  }
  par(mfrow = subplots, mar = c(3,4,2,1), oma = c(0,0,3,0))
  determinands %>% walk(function(x) plot_increase_determ(x, species = species, variable = variable, ...))
  mtext(plot_make_title(species = species, variable = variable, determinantgroup = determinantgroup), outer = TRUE, line = 0.5)
  if (plot == "file")
    dev.off()
}

# plot_increase_mult(c("CD","HG"), species = "Gadus morhua", variable = "VALUE_WW", determinantgroup = "metals")
# plot_increase_mult(c("CD","HG"), species = "Mytilus edulis", variable = "VALUE_WW", determinantgroup = "metals", 
#                    min_years = 15, min_indiv_per_year = 2) 

#
# For rank plot, make combined data set for multiple determinants
#
differences_increasing_conc_mult <- function(determinants, species, variable, min_years = 5, delete = "none", ...){
  for (i in 1:length(determinants)){
    tissue <- find_tissue(determinant = determinants[i], species = species)
    df <- differences_increasing_conc(determinants[i], species, tissue, variable, min_years = min_years, ...)
    if (!is.null(df) & i == 1){
      result <- df
      determinants_included <- determinants[i]
    } else if (!is.null(df) & i > 1){
      result <- rbind(result, df)
      determinants_included <- c(determinants_included, determinants[i])
    }
  }
  # result
  result_broad <- result[,c("Param","Station","Median2")] %>% spread(Station, Median2)
  result_broad <- result_broad[rank(determinants_included),]
  if (substr(delete, 1, 3) %in% c("det","row")){
    result_broad <- result_broad[apply(is.na(result_broad), 1, sum) == 0,]
  } else if (substr(delete, 1, 3) %in% c("sta","col")){
    result_broad <- result_broad[apply(is.na(result_broad), 2, sum) == 0,]
  }
  result_broad
}

# incr_metals <- differences_increasing_conc_mult(determ, species = "Gadus morhua", variable = "VALUE_WW")
# debugonce(differences_increasing_conc_mult)
# incr_metals <- differences_increasing_conc_mult(determ, species = "Mytilus edulis", variable = "VALUE_WW"
# incr_metals <- differences_increasing_conc_mult(determ, species = "Mytilus edulis", variable = "VALUE_WW", min_indiv_per_year = 2)
# View(incr_metals)

#
# Make and save rank plot
#  - uses differences_increasing_conc_mult + plot_make_filename and plot_make_title (also uses plot.qual )
#  - used by plot_both
#
plot_rank <- function(determinands, species, variable, determinantgroup, rank_plotsize = c(1.2, 0.4), plot = "file", ...){
  df <- differences_increasing_conc_mult(determinands, species, variable, ...)
  fn <- plot_make_filename(species = species, variable = variable, determinantgroup = determinantgroup, file_suffix = "ranks.png")
  if (!is.null(df)){
    if (plot == "file"){
      png2(fn, nrow(df)*rank_plotsize[1], ncol(df)*rank_plotsize[2])
    } else if (plot == "window") {
      windows(nrow(df)*rank_plotsize[1], ncol(df)*rank_plotsize[2])
    }
    par(mar=numeric(4), oma = c(0,0,3,0), family='serif')
    plot.qual(df, rs.ln = 5)
    mtext(plot_make_title(species = species, variable = variable, determinantgroup = determinantgroup), outer = TRUE, line = 0.5)
    if (plot == "file")
      dev.off()
  }
}

# plot_rank(determ, species = "Gadus morhua", variable = "VALUE_WW", determinantgroup = "metals")
# plot_rank(determ, species = "Mytilus edulis", variable = "VALUE_WW", determinantgroup = "metals", 
#           min_years = 15, min_indiv_per_year = 2, rank_plotsize = c(1.5, 0.25))

#
# Runs both plot_increase_mult() and plot_rank()
#
plot_both <- function(determinands, species, variable, determinantgroup, min_years = 5, min_indiv_per_year = 10,
                      rank_plotsize = c(1.2, 0.4), plot_median = "file", plot_rank = "file"){
  plot_increase_mult(determinands = determinands, species = species, variable = variable, determinantgroup = determinantgroup,
                     min_years = min_years, min_indiv_per_year = min_indiv_per_year, plot = plot_median)
  plot_rank(determinands = determinands, species = species, variable = variable, determinantgroup = determinantgroup,
            min_years = min_years, min_indiv_per_year = min_indiv_per_year, rank_plotsize = rank_plotsize, plot = plot_rank)
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Make table for "lower median" concentrations
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##


# Make table for "lower median" concentrations by determinand (columns) and stations (rows)
# Sorted by concentrations for determinand no. 1 (here, PCB7)
table_increase_mult <- function(determ, species, variable, data, min_years, min_indiv_per_year){
  i <- 1
  tissue <- find_tissue(determ[i], species = "Mytilus edulis")
  df_first <- differences_increasing_conc(determ[i], sp = species, ti = tissue, variable_name = variable, 
                                          data = cemp_muss_alt, min_years = min_years, min_indiv_per_year = min_indiv_per_year)
  result <- matrix(NA, nrow = nrow(df_first), ncol = length(determ))
  rownames(result) <- df_first$Station
  colnames(result) <- determ
  result[,determ[i]] <- df_first$Median2
  df_helper <- data.frame(Station = df_first$Station, stringsAsFactors = FALSE)
  for (i in 2:length(determ)){
    # i <- 2
    df <- differences_increasing_conc(determ[i], sp = "Mytilus edulis", ti = tissue, variable_name = variable, 
                                      data = cemp_mus_alt, min_years = min_years, min_indiv_per_year = min_indiv_per_year)
    df_helper <- left_join(df_helper, df[,c("Station","Median2")], by = "Station", copy = TRUE)
    result[,determ[i]] <- df_helper$Median2
    df_helper$Median2 <- NULL
  }
  result
}

# table_increase_mult(determ, species = "Mytilus edulis", variable = "conc_ww",  
#   data = cemp_muss_alt, min_years = 16, min_indiv_per_year = 1)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##
#
# Functions for finding background levels and compare last year's level ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##

#
# get_background_stations
#
get_background_stations <- function(determinant, species, var_name, years_backgr = 2002:2016, ...){
  X <- get_lower_medians(par = determinant, sp = species, ti = find_tissue(determinant = determinant, species = species), 
                         variable = var_name, data = subset(data_all, YEAR %in% years_backgr), ...)
  df_diff <- find_set_differences(X)
  i1 <- 1
  i2 <- which(df_diff$P < 0.1)[1] - 1
  if (is.na(i2)){
    i2 <- nrow(df_diff)
  }
  df_diff$Station[i1:i2]
}

# test
# get_background_stations("NI", "Gadus morhua", "VALUE_WW")

# debugonce(get_background_stations)
# get_background_stations("CD", "Gadus morhua", "VALUE_WW")



# Plot table (without text colors)
plot_table <- function(tab, title = ""){
  # tab <- result
  # start plot
  par(mar = c(4,5,2,1))
  x <- seq(0, 1, length = ncol(tab))
  y <- seq(1, 0, length = nrow(tab))
  dx <- x[2]-x[1]
  dy <- y[1]-y[2]
  plot(c(-dx, 1+dx), c(-dy, 1+dy), type = "n", xlab = "", ylab = "", axes = FALSE, main = title)
  axis(1, at = x, labels = colnames(tab))
  axis(2, at = y, labels = rownames(tab), las = 1)
  # plot numbers
  for (i in 1:ncol(tab))
    text(x[i], y, round(tab[,i],3))
}

# res <- table_increase_mult(determ, species = "Mytilus edulis", variable = "conc_ww",  
#   data = cemp_muss_alt, min_years = 16, min_indiv_per_year = 1)
# plot_table(res, "PCB")

tableplot_increase_mult <- function(determ, species, variable, data, 
                                    determinantgroup = "", plot = "", min_years, min_indiv_per_year){
  res <- table_increase_mult(determ, species = "Mytilus edulis", variable = "conc_ww",  
                             data = cemp_muss_alt, min_years = min_years, min_indiv_per_year = min_indiv_per_year)
  plot_table(res, determinantgroup)
}

# tableplot_increase_mult(determ, species = "Mytilus edulis", variable = "conc_ww", determinantgroup = "PCB", 
#   plot = "", data = cemp_muss_alt, min_years = 16, min_indiv_per_year = 1)


#
# get_background_values - is based on get_background_stations
#   - new version
#     percentiles (for limits) is based on the full data
#

get_background_values <- function(determinant, species, var_name, years_backgr = 1992:2016, 
                                  data = subset(data_all, YEAR %in% years_backgr), 
                                  critical_pvalue = 0.1, replicate_no = 1, ...){
  data <- as.data.frame(data)
  # How many individuals per year do we need to accept a station/year? (Used in 'get_lower_medians'.)  
  if (species %in% "Gadus morhua" & determinant != "ALAD"){ 
    min_indiv_per_year = 10
  } else if (species %in% "Gadus morhua"){ 
    min_indiv_per_year = 10
  } else if (species %in% "Mytilus edulis"){
    min_indiv_per_year = 2
  } else if (species %in% c("Littorina littorea", "Nucella lapillus", "Littorina/Nucella")){     # i.e., VDSI
    min_indiv_per_year = 1
  }
  # Picks the right tissue (e.g., muscle for mercury)
  tissue <- find_tissue(determinant = determinant, species = species)
  # The data that will be analysed ('df', will be used as input to 'get_lower_medians'.)
  sel_analysis <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & is.finite(data[,var_name]))
  df <- data[sel_analysis,] %>% as.data.frame()
  
  # Set less-thans to  a random number between 50 and 100% of the LOQ, using a uniform dsitribution)
  sel <- df$FLAG1 %in% "<"
  df$VALUE_WW[sel] <- runif(sum(sel), df$VALUE_WW[sel]*0.5, df$VALUE_WW[sel])
  sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_DW)
  df$VALUE_DW[sel] <- runif(sum(sel), df$VALUE_DW[sel]*0.5, df$VALUE_DW[sel])
  sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_FB)
  df$VALUE_FB[sel] <- runif(sum(sel), df$VALUE_FB[sel]*0.5, df$VALUE_FB[sel])
  
  # Check if all data has the same unit
  unit <- unique(df$UNIT)
  if (length(unit) == 0)
    unit <- "no data"
  if (length(unit) > 1)
    unit <- ">1 unit"
  # Run 'get_lower_medians', the first main part of the analysis
  # (note: changed data=data to data=df; compare with OLD3)
  X <- get_lower_medians(par = determinant, sp = species, ti = tissue, 
                         variable = var_name, data = df, min_indiv_per_year = min_indiv_per_year, ...)  
  # If results are OK
  if (!is.null(X) & !unit %in% c("no data", ">1 unit")){
    # Statistics for all dat used in analysis
    N_stations_base <- length(X$data_medians_list)
    N_base <- X$data_medians_list %>% purrr::map_int(~sum(.$n)) %>% sum()
    N_overLOQ_base <- X$data_medians_list %>% purrr::map_int(~sum(.$n_loq)) %>% sum()
    # Run 'find_set_differences', the second main part of the analysis
    df_diff <- find_set_differences(X)
    # Find out which stations that will be deemed reference stations (called just 'stations')
    i1 <- 1
    i2 <- which(df_diff$P < critical_pvalue)[1] - 1      # Picks the first P lower than critical_pvalue
    if (is.na(i2)){
      i2 <- nrow(df_diff)
    }
    stations <- df_diff$Station[i1:i2]
    # From the reference stations, get percentiles (upper_perc) and other data
    sel_proref <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & 
                          STATION_CODE %in% stations) &
      is.finite(data[,var_name])
    
    df_proref <- data[sel_proref,] %>% as.data.frame()    # here we use the "original" data again
    N <- nrow(df_proref)
    N_overLOQ <- sum(is.na(df_proref$FLAG1))
    ci_q <- qt(p = c(0.95, 0.975), N)
    upper_CI_log <- mean(log(df_proref[,var_name] + 0.001)) + ci_q*sd(log(df_proref[,var_name] + 0.001))/sqrt(N)
    upper_CI <- exp(upper_CI_log) - 0.001
    upper_perc <- quantile(df_proref[,var_name], c(0.5, 0.9, 0.95, 1))
    result <- data.frame(PARAM = determinant, LATIN_NAME = species, TISSUE_NAME = tissue, Variable = var_name, UNIT = unit,
                         Years_start = head(years_backgr, 1),
                         Years_end = tail(years_backgr, 1),
                         N_stations_base = N_stations_base,
                         N_base = N_base,
                         N_overLOQ_base = N_overLOQ_base,
                         Stations_proref = paste(stations, collapse = ","),
                         N_stations_proref = length(stations),
                         N_proref = N, 
                         N_overLOQ_proref = N_overLOQ,
                         Median = upper_perc[1], 
                         CI90 = upper_CI[1], CI95 = upper_CI[2], Q90 = upper_perc[2], 
                         Q95 = upper_perc[3], Max = upper_perc[4], 
                         Repl = replicate_no,
                         stringsAsFactors = FALSE)
  # If results are not OK, return (mostly) missing values    
  } else {
    result <- data.frame(PARAM = determinant, LATIN_NAME = species, TISSUE_NAME = tissue, Variable = var_name, UNIT = unit,
                         Years_start = head(years_backgr, 1),
                         Years_end = tail(years_backgr, 1),
                         N_stations_base = NA,
                         N_base = NA,
                         N_overLOQ_base = NA,
                         Stations_proref = "",
                         N_stations_proref  = NA,
                         N_proref = NA,
                         N_overLOQ_proref = NA,
                         Median = NA, 
                         CI90 = NA, CI95 = NA, Q90 = NA, Q95 = NA, Max = NA, 
                         Repl = replicate_no,
                         stringsAsFactors = FALSE)
  }  
  result
}

get_data_step <- function(determinant, species, var_name, years_backgr = 1992:2016, 
                          data = subset(data_all, YEAR %in% years_backgr), 
                          critical_pvalue = 0.1, 
                          step, ...){
  data <- as.data.frame(data)
  if (species %in% "Gadus morhua" & determinant != "ALAD"){ 
    min_indiv_per_year = 10
  } else if (species %in% "Gadus morhua"){ 
    min_indiv_per_year = 10
  } else if (species %in% "Mytilus edulis"){
    min_indiv_per_year = 2
  } else if (species %in% c("Littorina littorea", "Nucella lapillus", "Littorina/Nucella")){     # i.e., VDSI
    min_indiv_per_year = 1
  }
  tissue <- find_tissue(determinant = determinant, species = species)
  sel_analysis <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & is.finite(data[,var_name]))
  df <- data[sel_analysis,] %>% as.data.frame()
  
  # Set less-thans to  a random number between 50 and 100% of the LOQ, using a uniform dsitribution)
  sel <- df$FLAG1 %in% "<"
  df$VALUE_WW[sel] <- runif(sum(sel), df$VALUE_WW[sel]*0.5, df$VALUE_WW[sel])
  sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_DW)
  df$VALUE_DW[sel] <- runif(sum(sel), df$VALUE_DW[sel]*0.5, df$VALUE_DW[sel])
  sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_FB)
  df$VALUE_FB[sel] <- runif(sum(sel), df$VALUE_FB[sel]*0.5, df$VALUE_FB[sel])
  
  unit <- unique(df$UNIT)
  if (length(unit) == 0)
    unit <- "no data"
  if (length(unit) > 1)
    unit <- ">1 unit"
  # UNDER: changed data=data to data=df (compare with OLD3)
  X <- get_lower_medians(par = determinant, sp = species, ti = tissue, 
                         variable = var_name, data = df, min_indiv_per_year = min_indiv_per_year, ...)  
  if (!is.null(X) & !unit %in% c("no data", ">1 unit")){
    df_diff <- find_set_differences(X)
    i1 <- 1
    # i2 <- which(df_diff$P < critical_pvalue)[1] - 1      # Picks the first P lower than critical_pvalue
    i2 <- step
    stations <- df_diff$Station[i1:i2]
    sel_proref <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & 
                          STATION_CODE %in% stations) &
      is.finite(data[,var_name])
    
    df_proref <- data[sel_proref,] %>% as.data.frame()    # here we use thee "original" data again
    result <- data.frame(
      Step = step,
      Stations = paste(stations, collapse = ","),
      df_proref, 
      stringsAsFactors = FALSE)
    result$LOQ = ifelse(is.na(result$FLAG1), "Above LOQ", "Below LOQ")
  } else {
    result <- NULL
  }  
  result
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Define function for replicated analysis ----
#
# 'get_background_values_rep' - function for getting 10 repeats of 'get_background_values'
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

get_background_values_rep <- function(determinant, species, var_name, years_backgr, data, rep = 10) { 
  cat(determinant, "\n")
  1:rep %>% 
    map_df(~get_background_values(determinant=determinant, species=species, var_name=var_name, data = data, 
                                  years_backgr = years_backgr,  
                                  replicate_no = .)) %>%
    arrange(Q95)
}

# test
# get_background_values_rep("HCHG", "Gadus morhua", "VALUE_WW", subset(data_all, YEAR %in% years_backgr), rep = 10)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Get 'base' data ----
# I.e., data on which stations/data PROREF had to choose from  

# 'get_background_values' 
#
# We get metadata for these two datasets:
#   base1: all data with said determinant, basis, species etc
#   base2; as base1, but for stations with enough data (at least 'min_years' with at least 
#     'min_indiv_per_year' data oer year)
# In addition, we get one more metadata parameter for proref stations as well, namely
#   'N_overLOQ' = number of values over LOQ for 'background' (proref) stations
#
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# based on 'get_background_values' start
#
get_base_metadata <- function(determinant, species, var_name, years_backgr = 1992:2016, 
                              data = subset(data_all, YEAR %in% years_backgr),
                              proref_stations){
  data <- as.data.frame(data)
  # How many individuals per year do we need to accept a station/year? (Used in 'get_lower_medians'.)  
  if (species %in% "Gadus morhua" & determinant != "ALAD"){ 
    min_indiv_per_year = 10
  } else if (species %in% "Gadus morhua"){ 
    min_indiv_per_year = 10
  } else if (species %in% "Mytilus edulis"){
    min_indiv_per_year = 2
  } else if (species %in% c("Littorina littorea", "Nucella lapillus", "Littorina/Nucella")){     # i.e., VDSI
    min_indiv_per_year = 1
  }
  # Picks the right tissue (e.g., muscle for mercury)
  tissue <- find_tissue(determinant = determinant, species = species)
  # The data that will be analysed ('df', will be used as input to 'get_lower_medians'.)
  sel_analysis <-  with(data, PARAM %in% determinant & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & is.finite(data[,var_name]))

  # This is the basis of 'base1': all data and stations:  
  df <- data[sel_analysis,] %>% as.data.frame()

  # Set less-thans to  a random number between 50 and 100% of the LOQ, using a uniform dsitribution)
  sel <- df$FLAG1 %in% "<"
  df$VALUE_WW[sel] <- runif(sum(sel), df$VALUE_WW[sel]*0.5, df$VALUE_WW[sel])
  sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_DW)
  df$VALUE_DW[sel] <- runif(sum(sel), df$VALUE_DW[sel]*0.5, df$VALUE_DW[sel])
  sel <- df$FLAG1 %in% "<" & !is.na(df$VALUE_FB)
  df$VALUE_FB[sel] <- runif(sum(sel), df$VALUE_FB[sel]*0.5, df$VALUE_FB[sel])
  
  # Check if all data has the same unit
  unit <- unique(df$UNIT)
  if (length(unit) == 0)
    unit <- "no data"
  if (length(unit) > 1)
    unit <- ">1 unit"
  
  # 'data_analysis' below is the basis of 'base2': 
  # Data from the stations with enough data (at least 'min_years' with at least 
  #  'min_indiv_per_year' data oer year)
  data_analysis <- get_rawdata(par = determinant, sp = species, ti = tissue, 
                               variable = var_name, data = df, min_indiv_per_year = min_indiv_per_year)  
  
  if (!is.null(data_analysis)){
    N_stations_base2 <- length(unique(data_analysis$STATION_CODE))
    N_base2 <- nrow(data_analysis)
    N_overLOQ_base2 <- sum(is.na(data_analysis$FLAG1))
  } else {
    N_stations_base2 <- 0
    N_base2 <- 0
    N_overLOQ_base2 <- 0
  }
  
  # Picking proref stations specifically
  proref_stations <- strsplit(proref_stations, split = ",")[[1]]
  data_analysis_proref <- data_analysis %>%
    filter(STATION_CODE %in% proref_stations)  

  # Return N_stations_base, N_base and N_overLOQ_base
  data.frame(
    PARAM  = determinant,
    LATIN_NAME = species,
    TISSUE_NAME = tissue,
    Basis = sub("VALUE_", "", var_name, fixed = TRUE),
    UNIT = unique(df$UNIT)[1],
    N_overLOQ = sum(is.na(data_analysis_proref$FLAG1)),
    N_stations_base1 = length(unique(df$STATION_CODE)),
    N_base1 = nrow(df),
    N_overLOQ_base1 = sum(!sel),
    N_stations_base2 = N_stations_base2,
    N_base2 = N_base2,
    N_overLOQ_base2 = N_overLOQ_base2,
    stringsAsFactors = FALSE
    )
  

}

# Test
if (FALSE){
  get_base_metadata("CD", "Gadus morhua", "VALUE_WW", years_backgr = years_backgr,
                    data = subset(data_all, YEAR %in% years_backgr),
                    proref_stations = "80B,67B,15B,23B")
  
  # debugonce(get_base_metadata)
  get_base_metadata("4-N-NP", "Gadus morhua", "VALUE_WW", years_backgr = years_backgr,
                    data = subset(data_all, YEAR %in% years_backgr),
                    proref_stations = "80B,43B2")

  get_base_metadata("4-N-OP", "Gadus morhua", "VALUE_DW", years_backgr = years_backgr,
                    data = subset(data_all, YEAR %in% years_backgr),
                    proref_stations = "80B,43B2")
  
}

