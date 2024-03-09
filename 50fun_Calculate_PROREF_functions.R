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
      dplyr::summarise(upper_quant = quantile(VALUE_WW, probs = quantile, na.rm = TRUE))
    
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

find_set_difference_OLD <- function(object, i){
  if (i > 1){
    data1 <- get_stationdata_by_rankrange(object, 1:(i-1))
    data2 <- get_stationdata_by_rankrange(object, i)
    data_for_test1 <- data.frame(set = "data1", median = data1$medians$median, 
                                 stations = paste(data1$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test2 <- data.frame(set = "data2", median = data2$medians$median, 
                                 stations = paste(data2$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test <- rbind(data_for_test1, data_for_test2)
    testresult <- as.data.frame(summary(lm(median ~ set, data_for_test))$coef)[2,]
    txt <- paste(paste(data1$stations, collapse = ","), "vs.", data2$stations)
    result <- data.frame(Rank = i, Station = data2$stations, Test = txt, 
                         Variable = data1$medians$Variable[1],
                         Param = data1$medians$PARAM[1],
                         Species = data1$medians$Species[1],
                         Median1 = median(data1$medians$median), Median2 = median(data2$medians$median), 
                         Max_reduced_data1 = max(data1$medians$median), Upper_quantile2 = max(data2$medians$median),
                         testresult, stringsAsFactors = FALSE)
    colnames(result)[12:14] <- c("SE", "t", "P")      # note hard-coded column numbers
  } else if (i == 1){
    data2 <- get_stationdata_by_rankrange(object, i)
    result <- data.frame(Rank = i, Station = data2$stations, Test = "-", 
                         Variable = data2$medians$Variable[1],
                         Species = data2$medians$Species[1],
                         Param = data2$medians$PARAM[1],
                         Median1 = NA, Median2 = median(data2$medians$median), 
                         Max_reduced_data1 = NA, Upper_quantile2 = max(data2$medians$median),
                         Estimate = NA, SE = NA, t = NA, P = NA, stringsAsFactors = FALSE)
  }
  result
}


# Using Mann-Whitney test instead of parametric test
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


# As find_set_difference, but just makes a plot
plot_set_difference <- function(object, i){
  if (i > 1){
    data1 <- get_stationdata_by_rankrange(object, 1:(i-1))
    data2 <- get_stationdata_by_rankrange(object, i)
    data_for_test1 <- data.frame(set = "data1", median = data1$medians$median, 
                                 stations = paste(data1$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test2 <- data.frame(set = "data2", median = data2$medians$median, 
                                 stations = paste(data2$stations, collapse = ","), stringsAsFactors = FALSE)
    data_for_test <- rbind(data_for_test1, data_for_test2)
    data_for_test$stations <- factor(data_for_test$stations, levels = c(data_for_test1$stations[1],data_for_test2$stations[1]))
    gg <- ggplot(data_for_test, aes(stations, median)) + geom_point()
  }
  gg
}

# debugonce(plot_set_difference)
# plot_set_difference(X, 5)


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
  result <- 1:n %>% map_df(function(j) find_set_difference(object, j))
  result %>% mutate(Median_ratio = Median2/Median2[1])
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
    text(df$Rank + 0.1, df$Median2, round(df$Median_ratio, 1), adj = 0)
  }
  # Return data frame of differences invisibly  
  invisible(df)
}

# plot_increase("PB", "Gadus morhua","Lever","VALUE_WW")
# plot_increase("HG", "Gadus morhua","Lever","VALUE_WW")
# plot_increase("PB", "Mytilus edulis", "Whole soft body", "VALUE_WW", min_indiv_per_year = 2)

#
# alternative version (for paper)
#
plot_increase_alt <- function(par, sp, ti, variable_name, quantile = 0.9, data = data_all[sel_allplots, ],
                          min_years = 5, min_indiv_per_year = 10, threshold_p = c(0.1, 0.05, 0.01), cols = c("#FDCC8A", "#FC8D59", "#D7301F"),
                          xlab = "Stations (ranked by median concentration)", 
                          # ylab = expression(Concentration~plain("in")~cod~liver~(mu*g/kg~w.w.)),
                          ylab = "Concentration in cod liver\n(\u03BCg/kg w.w.)"
                          ){
  df <- differences_increasing_conc(par=par, sp=sp, ti=ti, variable_name=variable_name, quantile = quantile, data = data,
                                    min_years = min_years, min_indiv_per_year = min_indiv_per_year)
  # if (par == "AG") browser()
  # browser()
  if (!is.null(df)){
    plot(Median2 ~ Rank, df, type = "c", ylim = range(0, Median2), xlab = "", ylab = "", axes = FALSE)
    df2 <- df %>%
      mutate(Median1 = ifelse(Rank == 1, Median2, Median1))
    lines(Median1 ~ Rank, df2, type = "b", pch = 19, col = "blue")
    axis(1, labels = df$Station, at = df$Rank, las = 1, cex = 0.9)
    axis(2, las = 1, cex = 0.9)
    box()
    points(Median2 ~ Rank, df[1,],                                               pch = 1, cex = 1.4)
    points(Median2 ~ Rank, subset(df, P > threshold_p[1]),                       pch = 1, cex = 1.4)
    points(Median2 ~ Rank, subset(df, P <= threshold_p[1] & P > threshold_p[2]), pch = 21, cex = 1.4, bg = cols[1])
    points(Median2 ~ Rank, subset(df, P <= threshold_p[2] & P > threshold_p[3]), pch = 21, cex = 1.4, bg = cols[2])
    points(Median2 ~ Rank, subset(df, P <= threshold_p[3]),                      pch = 21, cex = 1.4, bg = cols[3])
    mtext(xlab, 1, line = 2.5, cex = 1.3)
    mtext(ylab, 2, line = 3.2, cex = 1.3)
  }
}

#
# Get data for "increase" plot (based on plot_increase_alt above)
#
plot_increase_data <- function(par, sp, ti, variable_name, quantile = 0.9, data = data_all[sel_allplots, ],
                              min_years = 5, min_indiv_per_year = 10, threshold_p = c(0.1, 0.05, 0.01), cols = c("#FDCC8A", "#FC8D59", "#D7301F"),
                              xlab = "Stations (ranked by median concentration)", 
                              # ylab = expression(Concentration~plain("in")~cod~liver~(mu*g/kg~w.w.)),
                              ylab = "Concentration in cod liver\n(\u03BCg/kg w.w.)"
){
  df <- differences_increasing_conc(par=par, sp=sp, ti=ti, variable_name=variable_name, quantile = quantile, data = data,
                                    min_years = min_years, min_indiv_per_year = min_indiv_per_year)
  # if (par == "AG") browser()
  # browser()
  if (!is.null(df)){
    df2 <- df %>%
      mutate(Median1 = ifelse(Rank == 1, Median2, Median1))
  } else {
    df2 <- NULL
  }
  df2
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
find_tissue_OLD <- function(determinant, species){
  if (species == "Gadus morhua" & determinant == "HG"){
    tissue <- "Muskel"
  } else if (species == "Gadus morhua" & determinant != "HG"){
    tissue <- "Lever"
  } else if (species == "Mytilus edulis"){
    tissue <- "Whole soft body"
  } else {
    stop ("species must be 'Gadus morhua' or 'Mytilus edulis'")
  }
  tissue
}
# find_tissue("CD", species = "Gadus morhua")
# find_tissue("HG", species = "Gadus morhua")
# find_tissue("CD", species = "Mytilus edulis")



#
# Find correct tissue, and run plot_increase (for a single subplot)
#
find_tissue <- function(determinant, species){
  fish_species <- c("Gadus morhua", "Platichthys flesus", "Limanda limanda")
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
  X <- get_lower_medians(par = determinant, sp = species, ti = tissue, 
                         variable = var_name, data = data, min_indiv_per_year = min_indiv_per_year, ...)
  if (!is.null(X) & !unit %in% c("no data", ">1 unit")){
    df_diff <- find_set_differences(X)
    i1 <- 1
    i2 <- which(df_diff$P < 0.1)[1] - 1
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
# Funstions for writing to excel ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o##


write_excel_levels <- function(crit_col, filename, 
                               background_levels = df_background, raw_data = data_all, 
                               yr1 = 1981, yr2 = 2016, 
                               cols = c("#FFFFFF", "#EDEDED", "#AFAFAF", "#777777", "#4F4F4F", "#000000")){
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Sheet1")
  logfile <- file(paste0(filename, "_log.txt"), "w+")
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  
  cs_head <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white")
  
  cs_back <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over1 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over2 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over3 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over4 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over5 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  N <- nrow(background_levels)
  
  result1 <- background_levels
  cols <- c("Median", "CI90", "CI95", "Q90", "Q95", "Max")
  result1[,cols] <- round(result1[,cols], 3)
  result2 <- data.frame(matrix(NA, N, yr2 - yr1 +1))
  colnames(result2) <- paste0("Yr_", yr1:yr2)
  # result <- cbind(result1, result2)
  result <- data.frame(result1, STATION_CODE = "", result2, stringsAsFactors = FALSE)
  colno_station <- which(colnames(result) %in% "STATION_CODE")
  colno_yr1 <- which(colnames(result) %in% paste0("Yr_", yr1))
  
  rows <- createRow(sheet, rowIndex=1)
  
  
  for (i in 1:ncol(result)){
    cell <- createCell(rows, colIndex = i)[[1,1]]
    setCellValue(cell, colnames(result)[i])
    setCellStyle(cell, cs_head)
  }
  
  raw_data <- as.data.frame(raw_data)
  
  linecount <- 2
  pb = txtProgressBar(min = 0, max = N)
  
  #i2 <- round(seq(0, N, length = no_parts), 0)[-1]
  #i1 <- c(1, head(i2,-1)+1)
  #for (i in i1[part]:i2[part]){
  
  for (i in 1:N){
    # for (i in 3:8){
    cat("\n", i, "\n", file = logfile)
    df1 <- subset(raw_data, YEAR %in% yr1:yr2 &
                    PARAM %in% background_levels$PARAM[i] & 
                    LATIN_NAME %in% background_levels$LATIN_NAME[i] & 
                    TISSUE_NAME %in% background_levels$TISSUE_NAME[i] &
                    is.finite(VALUE_WW)) %>%
      group_by(STATION_CODE) %>%
      summarise(n = n())
    for (j in 1:nrow(df1)){
      cat(j, "", file = logfile)
      rows <- createRow(sheet, rowIndex = linecount)
      for (m in 1:ncol(background_levels)){
        cell <- createCell(rows, colIndex = m)[[1,1]]
        setCellValue(cell, background_levels[i,m])
      }
      cell <- createCell(rows, colIndex = colno_station)[[1,1]]
      setCellValue(cell, df1$STATION_CODE[j])
      df2 <- subset(raw_data, YEAR %in% yr1:yr2 &
                      PARAM %in% background_levels$PARAM[i] & 
                      LATIN_NAME %in% background_levels$LATIN_NAME[i] & 
                      TISSUE_NAME %in% background_levels$TISSUE_NAME[i] &
                      STATION_CODE %in% df1$STATION_CODE[j] &                           
                      is.finite(VALUE_WW)) %>%
        group_by(YEAR) %>%
        summarise(median_ww = round(median(VALUE_WW),3))
      # if (i == 105) browser()
      # if (df1$STATION_CODE[j] %in% "I301") browser()
      # browser()
      df2_flag <- subset(raw_data, YEAR %in% yr1:yr2 &
                           PARAM %in% background_levels$PARAM[i] & 
                           LATIN_NAME %in% background_levels$LATIN_NAME[i] & 
                           TISSUE_NAME %in% background_levels$TISSUE_NAME[i] &
                           STATION_CODE %in% df1$STATION_CODE[j] &                           
                           is.finite(VALUE_WW) &
                           !is.na(FLAG1)) %>%
        group_by(YEAR) %>%
        summarise(median_ww_flag = round(median(VALUE_WW),3))
      df2 <- left_join(df2, df2_flag[,c("YEAR","median_ww_flag")], by = "YEAR")
      df2$median_print <- ifelse(df2$median_ww > df2$median_ww_flag | is.na(df2$median_ww_flag), df2$median_ww, paste0("<", df2$median_ww))
      for (k in 1:nrow(df2)){
        col_ind <- colno_yr1 + df2$YEAR[k] - yr1
        cell <- createCell(rows, colIndex = col_ind)[[1,1]]
        value <- round(df2$median_ww[k], 3)
        setCellValue(cell, df2$median_print[k])
        if (!is.na(background_levels[i, crit_col])){
          if (value <= background_levels[i, crit_col]){
            setCellStyle(cell, cs_back)
          } else if (value > background_levels[i, crit_col] & value <= 2*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over1)
          } else if (value > 2*background_levels[i, crit_col] & value <= 5*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over2)
          } else if (value > 5*background_levels[i, crit_col] & value <= 10*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over3)
          } else if (value > 10*background_levels[i, crit_col] & value <= 20*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over4)
          } else if (value > 20*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over5)
          }
        }
      }
      linecount <- linecount + 1
    }
    setTxtProgressBar(pb, i)
  }
  saveWorkbook(wb, filename)
  cat("\n\n")
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  close(logfile)
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Include info on pooled samples in the format "12(11-3)" (12 samples, of which 11 are pooled, with a maximum of 3 individuals per sample)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


info_pooling <- function(parameter, species, tissue, station, year, raw_data, table_samples_specimens){
  df <- subset(raw_data, PARAM %in% parameter & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & 
                 YEAR == year & STATION_CODE %in% station) %>%
    as.data.frame()
  sel <- table_samples_specimens[,"SAMPLE_ID"] %in% df[,"SAMPLE_ID2"]
  tab <- table(table_samples_specimens[sel, "SAMPLE_ID"])
  N <- nrow(df)           # number of samples
  N_pool <- sum(tab > 1)  # number of pooled samples
  Max_pool <- max(tab)    # max number of individuals per pooled sample
  if (N_pool > 0){
    result <- paste0(N, "(", N_pool, "-", Max_pool, ")")
  } else if (N > 0){
    result <- as.character(N)
  } else {
    result <- ""
  }
  result
}

#  data_bluemussel_samplesize

# works only for fish
info_pooling_OLD <- function(parameter, species, tissue, station, year, raw_data, table_samples_specimens){
  df <- subset(raw_data, PARAM %in% parameter & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & 
                 YEAR == year & STATION_CODE %in% station) %>%
    as.data.frame()
  sel <- table_samples_specimens[,"SAMPLE_ID"] %in% df[,"SAMPLE_ID2"]
  tab <- table(table_samples_specimens[sel, "SAMPLE_ID"])
  N <- nrow(df)           # number of samples
  N_pool <- sum(tab > 1)  # number of pooled samples
  Max_pool <- max(tab)    # max number of individuals per pooled sample
  if (N_pool > 0){
    result <- paste0(N, "(", N_pool, "-", Max_pool, ")")
  } else if (N > 0){
    result <- as.character(N)
  } else {
    result <- ""
  }
  result
}


info_pooling <- function(parameter, species, tissue, station, year, raw_data, table_samples_specimens, 
                         table_samples_bluemussel = data_bluemussel_samplesize){
  df <- subset(raw_data, PARAM %in% parameter & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & 
                 YEAR == year & STATION_CODE %in% station) %>%
    as.data.frame()
  if (!species %in% "Mytilus edulis"){
    sel <- table_samples_specimens[,"SAMPLE_ID"] %in% df[,"SAMPLE_ID2"]
    tab <- table(table_samples_specimens[sel, "SAMPLE_ID"])  
    N <- nrow(df)           # number of samples
    N_pool <- sum(tab > 1)  # number of pooled samples
    Max_pool <- max(tab)    # max number of individuals per pooled sample
  } else {
    sel <-  table_samples_bluemussel[,"STATION_CODE"] %in% station & table_samples_bluemussel[,"SUBNO"] %in% df[,"SAMPLE_NO"] 
    df2 <- table_samples_bluemussel[sel,]
    N <- nrow(df2)                # number of samples
    N_pool <- sum(df2$NOIMP > 1)  # number of pooled samples
    Max_pool <- max(df2$NOIMP)    # max number of individuals per pooled sample
  }
  if (N_pool > 0){
    result <- paste0(N, "(", N_pool, "-", Max_pool, ")")
  } else if (N > 0){
    result <- as.character(N)
  } else {
    result <- ""
  }
  result
}



#
# Test (see script '13b3_Calc_background_Write_to_Excel.R')
#
# test fish with pooled samples
# info_pooling(parameter = "TBEP", species = "Gadus morhua", tissue = "Lever", station = "71B", year = 2016,
#              raw_data = data_all, table_samples_specimens = df_samplesspec_2016)
# "15(6-2)"

# test fish without pooled samples
# info_pooling(parameter = "TBEP", species = "Gadus morhua", tissue = "Lever", station = "23B", year = 2016,
#              raw_data = data_all, table_samples_specimens = df_samplesspec_2016)
# "15"

# test blue mussel
# info_pooling(parameter = "CB28", species = "Mytilus edulis", tissue = "Whole soft body", station = "10A2", year = 2016,
#              raw_data = data_all, table_samples_specimens = df_samplesspec_2016)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
# Function for printing D.d.i. - detectable data information
# 7 [0.11 - 0.29] means 7 measurements over LOQ, an these measurements varied from 0.11 to 0.29
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

info_ddi <- function(parameter, species, tissue, station, year, raw_data){
  df <- subset(raw_data, PARAM %in% parameter & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & 
                 YEAR == year & STATION_CODE %in% station) %>%
    as.data.frame()
  N_over_LOQ <- sum(is.na(df[,"FLAG1"]))
  if (N_over_LOQ > 0){
    range_over_LOQ <- range(df[,"VALUE_WW"][is.na(df$FLAG1)])
    result <- paste0(N_over_LOQ, "[", range_over_LOQ[1], " - ", range_over_LOQ[2], "]")
  } else if (nrow(df) > 0 & N_over_LOQ == 0){
    result <- paste0(N_over_LOQ, "[ -  ]")
  } else {
    result <- ""
  }
  result
}

#
# Test (see script '13b3_Calc_background_Write_to_Excel.R')
#
# info_ddi(parameter = "BDE99", species = "Gadus morhua", tissue = "Lever", station = "13B", year = 2016, raw_data = data_all)
# "7[0.11 - 0.29]"      (i.e. ca half are over LOQ)
# info_ddi(parameter = "BDE", species = "Gadus morhua", tissue = "Lever", station = "13B", year = 2016, raw_data = data_all)
# "15[4.43 - 21.51]"    (i.e. all are over LOQ)
# info_ddi(parameter = "4_N-NP", species = "Gadus morhua", tissue = "Lever", station = "13B", year = 2016, raw_data = data_all)
# ""                    (i.e. none are over LOQ)

#
# For use with data on "data_all2" format (a single value column, an Basis as a key column)
#
info_ddi2 <- function(param, species, tissue, station, basis, year, raw_data, value_column = "Value"){
  df <- subset(raw_data, PARAM %in% param & LATIN_NAME %in% species & TISSUE_NAME %in% tissue & Basis %in% basis &
                 YEAR == year & STATION_CODE %in% station) %>%
    as.data.frame()
  N_over_LOQ <- sum(is.na(df[,"FLAG1"]))
  if (N_over_LOQ > 0){
    range_over_LOQ <- range(df[,value_column][is.na(df$FLAG1)])
    result <- paste0(N_over_LOQ, "[", range_over_LOQ[1], " - ", range_over_LOQ[2], "]")
  } else if (nrow(df) > 0 & N_over_LOQ == 0){
    result <- paste0(N_over_LOQ, "[ -  ]")
  } else {
    result <- ""
  }
  result
}




# which(df_background$PARAM %in% "HG") 
# 108, 109

#
# Also make EQS symbols
#
write_excel_levels2 <- function(crit_col, filename, 
                                background_levels = df_background, raw_data = data_all, table_samples_specimens = df_samplesspec_2016, EQS_limits = EQS,
                                yr1 = 1981, yr2 = 2016, 
                                cols = c("#FFFFFF", "#EDEDED", "#AFAFAF", "#777777", "#4F4F4F", "#000000")){
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Sheet1")
  
  logfile <- file(paste0(filename, "_log.txt"), "w+")                # Print i (index in 'background_levels' file) and j (station nr. j) to text file
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  
  # Style for column headings
  cs_head <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white")
  
  # Style for cells with medians < reference level
  cs_back <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over reference level, but below 2 x reference level
  cs_over1 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over 2 x reference level, but below 5 x reference level
  cs_over2 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Etc.    
  cs_over3 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over4 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over5 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_EQS_green <- CellStyle(wb) +
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#4EB265") + 
    Fill(backgroundColor="white", foregroundColor="white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  cs_EQS_red <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#DC050C") + 
    Fill(backgroundColor = "white", foregroundColor = "white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  N <- nrow(background_levels)
  
  result1 <- background_levels
  cols <- c("Median", "CI90", "CI95", "Q90", "Q95", "Max")
  result1[,cols] <- round(result1[,cols], 3)
  nyr <- yr2 - yr1 +1
  result2 <- data.frame(matrix(NA, N, 2*nyr))
  txt_yr <- rep(yr1:yr2, each = 2)
  txt_yr <- substr(txt_yr, 3, 4)
  colnames(result2) <- paste0(rep(c("V","Q"),nyr), txt_yr)
  # result <- cbind(result1, result2)
  result <- data.frame(result1, STATION_CODE = "", result2, stringsAsFactors = FALSE)
  colno_station <- which(colnames(result) %in% "STATION_CODE")
  
  colno_Q1 <- which(colnames(result) %in% paste0("Q", txt_yr[1]))   # number of the first 'Q' column (EQS symbol). Is used below to find where to start counting years
  
  # set width of V columns
  cols_V <- colnames(result2)[seq(1, by = 2, length = nyr)]         # name of all 'V' columns 
  colno_V <- which(colnames(result) %in% cols_V)                    # number of all 'V' columns
  setColumnWidth(sheet, colno_V, 6.8)                               # set width o
  
  # set width of Q columns
  cols_Q <- colnames(result2)[seq(2, by = 2, length = nyr)]         # name of all 'Q' columns
  colno_Q <- which(colnames(result) %in% cols_Q)                    # number of all 'Q' columns
  setColumnWidth(sheet, colno_Q, 4)                                 # set width
  
  last_level_column <- ncol(result)                                 # column number of the last column with level data
  
  rows <- createRow(sheet, rowIndex=1)
  
  for (i in 1:ncol(result)){
    cell <- createCell(rows, colIndex = i)[[1,1]]
    setCellValue(cell, colnames(result)[i])
    setCellStyle(cell, cs_head)
  }
  extra_headers <- paste0(c("Count", "DDI"), "_", yr2)
  for (i in 1:2){
    cell <- createCell(rows, colIndex = last_level_column + i)[[1,1]]
    setCellValue(cell, extra_headers[i])
    setCellStyle(cell, cs_head)
  }
  
  raw_data <- as.data.frame(raw_data)
  
  linecount <- 2
  pb = txtProgressBar(min = 0, max = N)
  
  #i2 <- round(seq(0, N, length = no_parts), 0)[-1]
  #i1 <- c(1, head(i2,-1)+1)
  #for (i in i1[part]:i2[part]){
  
  for (i in 1:N){  # all
    # for (i in 108:109){  # test with HG data
    # for (i in 3:8){
    cat("\n", i, "\n", file = logfile)
    row_eqs <- which(EQS_limits$PARAM %in% background_levels$PARAM[i])[1]
    if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "UG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]
    } else if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "MG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]/1000
    } else {
      EQS <- NA
    }
    df1 <- subset(raw_data, YEAR %in% yr1:yr2 &
                    PARAM %in% background_levels$PARAM[i] & 
                    LATIN_NAME %in% background_levels$LATIN_NAME[i] & 
                    TISSUE_NAME %in% background_levels$TISSUE_NAME[i] &
                    is.finite(VALUE_WW)) %>%
      group_by(STATION_CODE) %>%
      summarise(n = n())
    for (j in 1:nrow(df1)){
      cat(j, "", file = logfile)
      rows <- createRow(sheet, rowIndex = linecount)
      for (m in 1:ncol(background_levels)){
        cell <- createCell(rows, colIndex = m)[[1,1]]
        setCellValue(cell, background_levels[i,m])
      }
      cell <- createCell(rows, colIndex = colno_station)[[1,1]]
      setCellValue(cell, df1$STATION_CODE[j])
      df2 <- subset(raw_data, YEAR %in% yr1:yr2 &
                      PARAM %in% background_levels$PARAM[i] & 
                      LATIN_NAME %in% background_levels$LATIN_NAME[i] & 
                      TISSUE_NAME %in% background_levels$TISSUE_NAME[i] &
                      STATION_CODE %in% df1$STATION_CODE[j] &                           
                      is.finite(VALUE_WW)) %>%
        group_by(YEAR) %>%
        summarise(median_ww = round(median(VALUE_WW),3))
      # if (i == 105) browser()
      # if (df1$STATION_CODE[j] %in% "I301") browser()
      # browser()
      df2_flag <- subset(raw_data, YEAR %in% yr1:yr2 &
                           PARAM %in% background_levels$PARAM[i] & 
                           LATIN_NAME %in% background_levels$LATIN_NAME[i] & 
                           TISSUE_NAME %in% background_levels$TISSUE_NAME[i] &
                           STATION_CODE %in% df1$STATION_CODE[j] &                           
                           is.finite(VALUE_WW) &
                           !is.na(FLAG1)) %>%
        group_by(YEAR) %>%
        summarise(median_ww_flag = round(median(VALUE_WW),3))
      df2 <- left_join(df2, df2_flag[,c("YEAR","median_ww_flag")], by = "YEAR")
      # Add "<"
      df2$median_print <- ifelse(df2$median_ww > df2$median_ww_flag | is.na(df2$median_ww_flag), as.character(df2$median_ww), paste0("<", df2$median_ww))
      # If you want, change decimal sign from "." to ","
      # df2$median_print <- sub(".", ",", df2$median_print, fixed = TRUE)
      for (k in 1:nrow(df2)){
        col_ind2 <- colno_Q1 + 2*(df2$YEAR[k] - yr1)               # find which column number to put the EQS symbol in
        col_ind1 <- col_ind2 - 1                                   # find which column number to put the median in
        value <- round(df2$median_ww[k], 3)                        # median value
        
        # Print median value
        cell <- createCell(rows, colIndex = col_ind1)[[1,1]]
        setCellValue(cell, df2$median_print[k])
        
        # Format median value
        if (!is.na(background_levels[i, crit_col])){
          if (value <= background_levels[i, crit_col]){
            setCellStyle(cell, cs_back)
          } else if (value > background_levels[i, crit_col] & value <= 2*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over1)
          } else if (value > 2*background_levels[i, crit_col] & value <= 5*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over2)
          } else if (value > 5*background_levels[i, crit_col] & value <= 10*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over3)
          } else if (value > 10*background_levels[i, crit_col] & value <= 20*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over4)
          } else if (value > 20*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over5)
          }
        }
        # EQS symbol - we use letter 'n', in Webdings font that becomes a 'bullet'
        cell <- createCell(rows, colIndex = col_ind2)[[1,1]]
        if (!is.na(EQS)){
          setCellValue(cell, "n")
          if (value <= EQS){
            setCellStyle(cell, cs_EQS_green)
          } else {
            setCellStyle(cell, cs_EQS_red)
          }
        }
      }
      # browser()
      # Add a cell with oinformation about pooling
      txt_pool <- info_pooling(parameter = background_levels$PARAM[i], species = background_levels$LATIN_NAME[i], 
                               tissue = background_levels$TISSUE_NAME[i], station = df1$STATION_CODE[j], year = yr2,
                               raw_data = raw_data, table_samples_specimens = table_samples_specimens)
      cell <- createCell(rows, colIndex = last_level_column + 1)[[1,1]]
      setCellValue(cell, txt_pool)
      # Add another cell with information about D.d.i. (values above LOQ)
      txt_ddi <- info_ddi(parameter = background_levels$PARAM[i], species = background_levels$LATIN_NAME[i], 
                          tissue = background_levels$TISSUE_NAME[i], station = df1$STATION_CODE[j], year = yr2,
                          raw_data = raw_data)
      cell <- createCell(rows, colIndex = last_level_column + 2)[[1,1]]
      setCellValue(cell, txt_ddi)
      linecount <- linecount + 1
    }
    setTxtProgressBar(pb, i)
  }
  saveWorkbook(wb, filename)
  cat("\n\n")
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  close(logfile)
}



#
#
# EQS symbols
# Columns are added in correct sequence / correct naming for finished Excel file
# 
# OLD version: variable is fixed to be VALUE_WW
#
#
write_excel_levels3_OLD <- function(crit_col, filename, 
                                    background_levels = df_background, raw_data = data_all, table_samples_specimens = df_samplesspec_2016, EQS_limits = EQS,
                                    # trends_long, trends_10yr,
                                    varnames,
                                    yr1 = 1980, yr2 = 2016, 
                                    cols = c("#FFFFFF", "#EDEDED", "#AFAFAF", "#777777", "#4F4F4F", "#000000")){
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Sheet1")
  
  logfile <- file(paste0(filename, "_log.txt"), "w+")                # Print i (index in 'background_levels' file) and j (station nr. j) to text file
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  
  # Style for column headings
  cs_head <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white")
  
  # Style for cells with medians < reference level
  cs_back <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over reference level, but below 2 x reference level
  cs_over1 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over 2 x reference level, but below 5 x reference level
  cs_over2 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Etc.    
  cs_over3 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over4 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over5 <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_EQS_green <- CellStyle(wb) +
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#4EB265") + 
    Fill(backgroundColor="white", foregroundColor="white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  cs_EQS_red <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#DC050C") + 
    Fill(backgroundColor = "white", foregroundColor = "white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  N <- nrow(background_levels)
  
  # Set column names
  rows <- createRow(sheet, rowIndex=1)
  for (i in 1:length(varnames)){
    cell <- createCell(rows, colIndex = i)[[1,1]]
    setCellValue(cell, varnames[i])
    setCellStyle(cell, cs_head)
  }
  
  df_var <- data.frame(
    Name1 = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Variable", "UNIT", "Stations", "N_stations", "N", "Median", "Q95"),
    Name2 = c("Parameter Code", "Species", "Tissue", "Basis", "Unit", "Backgr_stations", "Backgr_Nstat", "Backgr_N", "Median", "Q95"),
    stringsAsFactors = FALSE
  )
  
  colno_station <- which(varnames %in% "Station Code")
  
  nyr <- yr2 - yr1 + 1
  txt_yr <- rep(yr1:yr2, each = 2)
  txt_yr <- substr(txt_yr, 3, 4)
  colno_Q1 <- which(varnames %in% paste0("Q", txt_yr[1]))   # number of the first 'Q' column (EQS symbol). Is used below to find where to start counting years
  last_level_column <- which(varnames %in% paste0("V", txt_yr[2]))                                  # column number of the last column with level data
  
  # Take care these have correct names
  # browser()
  colno_pool <- which(varnames %in% "Ant pr?ver 2016")   # number of pooling column
  colno_ddi  <- which(varnames %in% "N>LOQ(min-maks)")   # number of ddi column
  
  colno_trends_long  <- which(varnames %in% "Trend p(long)")   # numbers of the first column for long trends
  colno_trends_10yr  <- which(varnames %in% "Trend p(short)")  # numbers of the first column for long trends
  
  # set width of V columns
  cols_V <- varnames[seq(1, by = 2, length = nyr)]         # name of all 'V' columns 
  colno_V <- which(varnames %in% cols_V)                   # number of all 'V' columns
  setColumnWidth(sheet, colno_V, 6.8)                      # set width o
  
  # set width of Q columns
  cols_Q <- varnames[seq(2, by = 2, length = nyr)]         # name of all 'Q' columns
  colno_Q <- which(varnames %in% cols_Q)                   # number of all 'Q' columns
  setColumnWidth(sheet, colno_Q, 4)                        # set width
  
  raw_data <- as.data.frame(raw_data)
  
  linecount <- 2
  pb = txtProgressBar(min = 0, max = N)
  
  #i2 <- round(seq(0, N, length = no_parts), 0)[-1]
  #i1 <- c(1, head(i2,-1)+1)
  #for (i in i1[part]:i2[part]){
  
  for (i in 1:N){  # all
    # for (i in 108:109){  # test with HG data
    # for (i in 3:8){
    cat("\n", i, "\n", file = logfile)
    row_eqs <- which(EQS_limits$PARAM %in% background_levels[i, "PARAM"])[1]
    if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "UG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]
    } else if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "MG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]/1000
    } else {
      EQS <- NA
    }
    df1 <- subset(raw_data, YEAR %in% yr1:yr2 &
                    PARAM %in% background_levels[i, "PARAM"] & 
                    LATIN_NAME %in% background_levels[i, "LATIN_NAME"] & 
                    TISSUE_NAME %in% background_levels[i, "TISSUE_NAME"] &
                    is.finite(VALUE_WW)) %>%
      group_by(STATION_CODE) %>%
      summarise(n = n())
    for (j in 1:nrow(df1)){
      cat(j, "", file = logfile)
      rows <- createRow(sheet, rowIndex = linecount)
      # browser()
      for (m in 1:nrow(df_var)){
        colno <- which(varnames %in% df_var$Name2[m])
        cell <- createCell(rows, colIndex = colno)[[1,1]]
        setCellValue(cell, background_levels[i, df_var$Name1[m]])
      }
      cell <- createCell(rows, colIndex = colno_station)[[1,1]]
      setCellValue(cell, df1$STATION_CODE[j])
      df2_sel <- raw_data[,"YEAR"] %in% yr1:yr2 &
        raw_data[,"PARAM"] %in% background_levels[i, "PARAM"] & 
        raw_data[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
        raw_data[,"TISSUE_NAME"]%in% background_levels[i, "TISSUE_NAME"] &
        raw_data[,"STATION_CODE"] %in% df1$STATION_CODE[j] &                           
        is.finite(raw_data[,"VALUE_WW"])
      df2 <- raw_data[df2_sel,] %>%
        group_by(YEAR) %>%
        summarise(median_ww = round(median(VALUE_WW),3))
      # if (i == 105) browser()
      # if (df1$STATION_CODE[j] %in% "I301") browser()
      # browser()
      df2_flag_sel <- df2_sel & !is.na(raw_data[,"FLAG1"])
      df2_flag <- raw_data[df2_flag_sel,] %>%
        group_by(YEAR) %>%
        summarise(median_ww_flag = round(median(VALUE_WW),3))
      df2 <- left_join(df2, df2_flag[,c("YEAR","median_ww_flag")], by = "YEAR")
      # Add "<"
      df2$median_print <- ifelse(df2$median_ww > df2$median_ww_flag | is.na(df2$median_ww_flag), as.character(df2$median_ww), paste0("<", df2$median_ww))
      # If you want, change decimal sign from "." to ","
      # df2$median_print <- sub(".", ",", df2$median_print, fixed = TRUE)
      for (k in 1:nrow(df2)){
        col_ind2 <- colno_Q1 + 2*(df2$YEAR[k] - yr1)               # find which column number to put the EQS symbol in
        col_ind1 <- col_ind2 - 1                                   # find which column number to put the median in
        value <- round(df2$median_ww[k], 3)                        # median value
        
        # Print median value
        cell <- createCell(rows, colIndex = col_ind1)[[1,1]]
        setCellValue(cell, df2$median_print[k])
        
        # Format median value
        if (!is.na(background_levels[i, crit_col])){
          if (value <= background_levels[i, crit_col]){
            setCellStyle(cell, cs_back)
          } else if (value > background_levels[i, crit_col] & value <= 2*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over1)
          } else if (value > 2*background_levels[i, crit_col] & value <= 5*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over2)
          } else if (value > 5*background_levels[i, crit_col] & value <= 10*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over3)
          } else if (value > 10*background_levels[i, crit_col] & value <= 20*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over4)
          } else if (value > 20*background_levels[i, crit_col]) {
            setCellStyle(cell, cs_over5)
          }
        }
        # EQS symbol - we use letter 'n', in Webdings font that becomes a 'bullet'
        cell <- createCell(rows, colIndex = col_ind2)[[1,1]]
        if (!is.na(EQS)){
          setCellValue(cell, "n")
          if (value <= EQS){
            setCellStyle(cell, cs_EQS_green)
          } else {
            setCellStyle(cell, cs_EQS_red)
          }
        }
      }
      # browser()
      # Add a cell with information about pooling
      txt_pool <- info_pooling(parameter = background_levels[i, "PARAM"], species = background_levels[i, "LATIN_NAME"], 
                               tissue = background_levels[i, "TISSUE_NAME"], station = df1$STATION_CODE[j], year = yr2,
                               raw_data = raw_data, table_samples_specimens = table_samples_specimens)
      cell <- createCell(rows, colIndex = colno_pool)[[1,1]]
      setCellValue(cell, txt_pool)
      # Add another cell with information about D.d.i. (values above LOQ)
      txt_ddi <- info_ddi(parameter = background_levels[i, "PARAM"], species = background_levels[i, "LATIN_NAME"], 
                          tissue = background_levels[i, "TISSUE_NAME"], station = df1$STATION_CODE[j], year = yr2,
                          raw_data = raw_data)
      cell <- createCell(rows, colIndex = colno_ddi)[[1,1]]
      setCellValue(cell, txt_ddi)
      # sel1 <- trends_long[,"PARAM"] %in% background_levels[i, "PARAM"] & 
      #   trends_long[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
      #   trends_long[,"TISSUE_NAME"] %in% background_levels[i, "TISSUE_NAME"] & 
      #   trends_long[,"STATION_CODE"] %in% df1$STATION_CODE[j]        
      # trends_long_results <- with(trends_long[sel1,], 
      #   c(P_change, 
      #     100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1),
      #     Year1, Year2, N))
      # sel2 <- trends_10yr[,"PARAM"] %in% background_levels[i, "PARAM"] & 
      #   trends_10yr[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
      #   trends_10yr[,"TISSUE_NAME"] %in% background_levels[i, "TISSUE_NAME"] & 
      #   trends_10yr[,"STATION_CODE"] %in% df1$STATION_CODE[j]        
      # trends_10yr_results <- with(trends_10yr[sel2,], 
      #   c(P_change, 
      #     100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1),
      #     Year1, Year2, N))
      # cell <- createCell(rows, colIndex = colno_trends_long + seq(0,4))[[1,1]]
      # setCellValue(cell, trends_long_results)
      # cell <- createCell(rows, colIndex = colno_trends_10yr + seq(0,4))[[1,1]]
      # setCellValue(cell, trends_10yr_results)
      # block <- CellBlock(sheet, startRow = linecount, startColumn = colno_trends_long, noRows = 1, noColumns = 5)
      # CB.setRowData(block, trends_long_results, 1)
      # block <- CellBlock(sheet, startRow = linecount, startColumn = colno_trends_10yr, noRows = 1, noColumns = 5)
      # CB.setRowData(block, trends_10yr_results, 1)
      
      # if (sum(sel1) > 0)
      #   for (i in 1:5){
      #     cell <- createCell(rows, colIndex = colno_trends_long + i - 1)[[1,1]]
      #     setCellValue(cell, trends_long_results[i])
      #     }
      # if (sum(sel2) > 0)
      #   for (i in 1:5){
      #     cell <- createCell(rows, colIndex = colno_trends_10yr + i - 1)[[1,1]]
      #     setCellValue(cell, trends_10yr_results[i])
      #     }
      
      linecount <- linecount + 1
    }
    setTxtProgressBar(pb, i)
  }
  saveWorkbook(wb, filename)
  cat("\n\n")
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  close(logfile)
  
}


#
#
#
# EQS symbols
# Columns are added in correct sequence / correct naming for finished Excel file
# Works for a given variable_name, e.g. VALUE_WW 
#
#
#
write_excel_levels3 <- function(crit_col, variable_name, filename, 
                                background_levels = df_background, raw_data = data_all, table_samples_specimens = df_samplesspec_2016, EQS_limits = EQS,
                                # trends_long, trends_10yr,
                                varnames,
                                yr1 = 1980, yr2 = 2016, 
                                cols = c("#FFFFFF", "#EDEDED", "#AFAFAF", "#777777", "#4F4F4F", "#000000")){
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Sheet1")
  
  logfile <- file(paste0(filename, "_log.txt"), "w+")                # Print i (index in 'background_levels' file) and j (station nr. j) to text file
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  
  # For now, we solve the task of specifying which variable to use by defining a new variable: VALUE_MEAS
  raw_data$VALUE_MEAS <- raw_data[,variable_name]
  
  # Style for column headings
  cs_head <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white")
  
  # Style for cells with medians < reference level
  cs_back <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over reference level, but below 2 x reference level
  cs_over1 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over 2 x reference level, but below 5 x reference level
  cs_over2 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Etc.    
  cs_over3 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over4 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over5 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_EQS_green <- CellStyle(wb) +
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#4EB265") + 
    Fill(backgroundColor="white", foregroundColor="white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  cs_EQS_red <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#DC050C") + 
    Fill(backgroundColor = "white", foregroundColor = "white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  N <- nrow(background_levels)
  
  # Set column names
  rows <- createRow(sheet, rowIndex=1)
  for (i in 1:length(varnames)){
    cell <- createCell(rows, colIndex = i)[[1,1]]
    setCellValue(cell, varnames[i])
    setCellStyle(cell, cs_head)
  }
  
  df_var <- data.frame(
    Name1 = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Variable", "UNIT", "Stations", "N_stations", "N", "Median", "Q95"),
    Name2 = c("Parameter Code", "Species", "Tissue", "Basis", "Unit", "Backgr_stations", "Backgr_Nstat", "Backgr_N", "Median", "Q95"),
    stringsAsFactors = FALSE
  )
  
  colno_station <- which(varnames %in% "Station Code")
  
  nyr <- yr2 - yr1 + 1
  txt_yr <- rep(yr1:yr2, each = 2)
  txt_yr <- substr(txt_yr, 3, 4)
  colno_Q1 <- which(varnames %in% paste0("Q", txt_yr[1]))   # number of the first 'Q' column (EQS symbol). Is used below to find where to start counting years
  last_level_column <- which(varnames %in% paste0("V", txt_yr[2]))                                  # column number of the last column with level data
  
  # Take care these have correct names
  # browser()
  colno_pool <- which(varnames %in% "Ant pr?ver 2016")   # number of pooling column
  colno_ddi  <- which(varnames %in% "N>LOQ(min-maks)")   # number of ddi column
  colno_sd2015  <- which(varnames %in% "SD 2015")   # number of column for SD 
  colno_sd2016  <- which(varnames %in% "SD 2016")   # number of column for SD 
  
  colno_trends_long  <- which(varnames %in% "Trend p(long)")   # numbers of the first column for long trends
  colno_trends_10yr  <- which(varnames %in% "Trend p(short)")  # numbers of the first column for long trends
  
  # set width of V columns
  cols_V <- varnames[seq(1, by = 2, length = nyr)]         # name of all 'V' columns 
  colno_V <- which(varnames %in% cols_V)                   # number of all 'V' columns
  setColumnWidth(sheet, colno_V, 6.8)                      # set width o
  
  # set width of Q columns
  cols_Q <- varnames[seq(2, by = 2, length = nyr)]         # name of all 'Q' columns
  colno_Q <- which(varnames %in% cols_Q)                   # number of all 'Q' columns
  setColumnWidth(sheet, colno_Q, 4)                        # set width
  
  raw_data <- as.data.frame(raw_data)
  
  linecount <- 2
  pb = txtProgressBar(min = 0, max = N)
  
  #i2 <- round(seq(0, N, length = no_parts), 0)[-1]
  #i1 <- c(1, head(i2,-1)+1)
  #for (i in i1[part]:i2[part]){
  
  for (i in 1:N){  # all
    # for (i in 108:109){  # test with HG data
    # for (i in 57:58){
    cat("\n", i, "\n", file = logfile)
    row_eqs <- which(EQS_limits$PARAM %in% background_levels[i, "PARAM"])[1]
    if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "UG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]
    } else if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "MG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]/1000
    } else {
      EQS <- NA
    }
    df1 <- subset(raw_data, YEAR %in% yr1:yr2 &
                    PARAM %in% background_levels[i, "PARAM"] & 
                    LATIN_NAME %in% background_levels[i, "LATIN_NAME"] & 
                    TISSUE_NAME %in% background_levels[i, "TISSUE_NAME"] &
                    is.finite(VALUE_MEAS)) %>%
      group_by(STATION_CODE) %>%
      summarise(n = n())
    if (nrow(df1) > 0){
      for (j in 1:nrow(df1)){
        cat(j, "", file = logfile)
        rows <- createRow(sheet, rowIndex = linecount)
        # browser()
        for (m in 1:nrow(df_var)){
          colno <- which(varnames %in% df_var$Name2[m])
          cell <- createCell(rows, colIndex = colno)[[1,1]]
          setCellValue(cell, background_levels[i, df_var$Name1[m]])
        }
        # browser()
        cell <- createCell(rows, colIndex = colno_station)[[1,1]]
        setCellValue(cell, df1$STATION_CODE[j])
        df2_sel <- raw_data[,"YEAR"] %in% yr1:yr2 &
          raw_data[,"PARAM"] %in% background_levels[i, "PARAM"] & 
          raw_data[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
          raw_data[,"TISSUE_NAME"]%in% background_levels[i, "TISSUE_NAME"] &
          raw_data[,"STATION_CODE"] %in% df1$STATION_CODE[j] &                           
          is.finite(raw_data[,"VALUE_MEAS"])
        df2 <- raw_data[df2_sel,] %>%
          group_by(YEAR) %>%
          summarise(median_value = round(median(VALUE_MEAS),3))
        # subset(raw_data[df2_sel, ], select = c(YEAR, PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, VALUE_MEAS, FLAG1))
        # if (i == 105) browser()
        # if (df1$STATION_CODE[j] %in% "I301") browser()
        # browser()
        df2_flag_sel <- df2_sel & !is.na(raw_data[,"FLAG1"])
        df2_flag <- raw_data[df2_flag_sel,] %>%
          group_by(YEAR) %>%
          summarise(median_value_flag = round(median(VALUE_MEAS),3))
        df2 <- left_join(df2, df2_flag[,c("YEAR","median_value_flag")], by = "YEAR")
        # Add "<"
        # browser()
        df2$median_print <- ifelse(df2$median_value > df2$median_value_flag | is.na(df2$median_value_flag), df2$median_value, paste0("<", df2$median_value))
        # If you want, change decimal sign from "." to "," for all values:|
        # df2$median_print <- sub(".", ",", df2$median_print, fixed = TRUE)
        
        # Change decimal sign from "." to "," for those that don't have the < attached 
        # df2$median_print <- ifelse(df2$median_value > df2$median_value_flag | is.na(df2$median_value_flag), sub(".", ",", df2$median_print, fixed = TRUE), df2$median_print)
        
        df2_sd <- raw_data[df2_sel,] %>%
          group_by(YEAR) %>%
          summarise(SD = round(sd(VALUE_MEAS, na.rm = TRUE),3)) %>%
          as.data.frame()
        
        for (k in 1:nrow(df2)){
          col_ind2 <- colno_Q1 + 2*(df2$YEAR[k] - yr1)               # find which column number to put the EQS symbol in
          col_ind1 <- col_ind2 - 1                                   # find which column number to put the median in
          value <- round(df2$median_value[k], 3)                     # median value
          
          # Print median value
          cell <- createCell(rows, colIndex = col_ind1)[[1,1]]
          setCellValue(cell, df2$median_print[k])
          
          # Format median value
          if (!is.na(background_levels[i, crit_col])){
            if (value <= background_levels[i, crit_col]){
              setCellStyle(cell, cs_back)
            } else if (value > background_levels[i, crit_col] & value <= 2*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over1)
            } else if (value > 2*background_levels[i, crit_col] & value <= 5*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over2)
            } else if (value > 5*background_levels[i, crit_col] & value <= 10*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over3)
            } else if (value > 10*background_levels[i, crit_col] & value <= 20*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over4)
            } else if (value > 20*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over5)
            }
          }
          # EQS symbol - we use letter 'n', in Webdings font that becomes a 'bullet'
          cell <- createCell(rows, colIndex = col_ind2)[[1,1]]
          if (!is.na(EQS)){
            setCellValue(cell, "n")
            if (value <= EQS){
              setCellStyle(cell, cs_EQS_green)
            } else {
              setCellStyle(cell, cs_EQS_red)
            }
          }
        }
        # browser()
        # Add a cell with information about pooling
        txt_pool <- info_pooling(parameter = background_levels[i, "PARAM"], species = background_levels[i, "LATIN_NAME"], 
                                 tissue = background_levels[i, "TISSUE_NAME"], station = df1$STATION_CODE[j], year = yr2,
                                 raw_data = raw_data, table_samples_specimens = table_samples_specimens)
        cell <- createCell(rows, colIndex = colno_pool)[[1,1]]
        setCellValue(cell, txt_pool)
        # Add another cell with information about D.d.i. (values above LOQ)
        txt_ddi <- info_ddi(parameter = background_levels[i, "PARAM"], species = background_levels[i, "LATIN_NAME"], 
                            tissue = background_levels[i, "TISSUE_NAME"], station = df1$STATION_CODE[j], year = yr2,
                            raw_data = raw_data)
        cell <- createCell(rows, colIndex = colno_ddi)[[1,1]]
        setCellValue(cell, txt_ddi)
        # Add SD 2015 and 2016
        sel <- df2_sd[,"YEAR"] %in% 2015
        if (sum(sel) == 1){
          txt_sd <- df2_sd[sel, "SD"]
          cell <- createCell(rows, colIndex = colno_sd2015)[[1,1]]
          setCellValue(cell, txt_sd)
        }
        sel <- df2_sd[,"YEAR"] %in% 2016
        if (sum(sel) == 1){
          txt_sd <- df2_sd[sel, "SD"]
          cell <- createCell(rows, colIndex = colno_sd2016)[[1,1]]
          setCellValue(cell, txt_sd)
        }
        # sel1 <- trends_long[,"PARAM"] %in% background_levels[i, "PARAM"] & 
        #   trends_long[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
        #   trends_long[,"TISSUE_NAME"] %in% background_levels[i, "TISSUE_NAME"] & 
        #   trends_long[,"STATION_CODE"] %in% df1$STATION_CODE[j]        
        # trends_long_results <- with(trends_long[sel1,], 
        #   c(P_change, 
        #     100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1),
        #     Year1, Year2, N))
        # sel2 <- trends_10yr[,"PARAM"] %in% background_levels[i, "PARAM"] & 
        #   trends_10yr[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
        #   trends_10yr[,"TISSUE_NAME"] %in% background_levels[i, "TISSUE_NAME"] & 
        #   trends_10yr[,"STATION_CODE"] %in% df1$STATION_CODE[j]        
        # trends_10yr_results <- with(trends_10yr[sel2,], 
        #   c(P_change, 
        #     100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1),
        #     Year1, Year2, N))
        # cell <- createCell(rows, colIndex = colno_trends_long + seq(0,4))[[1,1]]
        # setCellValue(cell, trends_long_results)
        # cell <- createCell(rows, colIndex = colno_trends_10yr + seq(0,4))[[1,1]]
        # setCellValue(cell, trends_10yr_results)
        # block <- CellBlock(sheet, startRow = linecount, startColumn = colno_trends_long, noRows = 1, noColumns = 5)
        # CB.setRowData(block, trends_long_results, 1)
        # block <- CellBlock(sheet, startRow = linecount, startColumn = colno_trends_10yr, noRows = 1, noColumns = 5)
        # CB.setRowData(block, trends_10yr_results, 1)
        
        # if (sum(sel1) > 0)
        #   for (i in 1:5){
        #     cell <- createCell(rows, colIndex = colno_trends_long + i - 1)[[1,1]]
        #     setCellValue(cell, trends_long_results[i])
        #     }
        # if (sum(sel2) > 0)
        #   for (i in 1:5){
        #     cell <- createCell(rows, colIndex = colno_trends_10yr + i - 1)[[1,1]]
        #     setCellValue(cell, trends_10yr_results[i])
        #     }
        
        linecount <- linecount + 1
      }
    }
    setTxtProgressBar(pb, i)
  }
  saveWorkbook(wb, filename)
  cat("\n\n")
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  close(logfile)
  
}

#
# Note: works by default with VALUE_WW
#
write_excel_levels3_identifiers_only <- function(variable_name, background_levels = df_background, raw_data = data_all,
                                                 yr1 = 1980, yr2 = 2016){
  
  N <- nrow(background_levels)
  raw_data <- as.data.frame(raw_data)
  linecount <- 1
  
  sel <- is.finite(raw_data[,variable_name])
  for (i in 1:N){  # all
    df1 <- subset(raw_data[sel,], YEAR %in% yr1:yr2 &
                    PARAM %in% background_levels[i, "PARAM"] & 
                    LATIN_NAME %in% background_levels[i, "LATIN_NAME"] & 
                    TISSUE_NAME %in% background_levels[i, "TISSUE_NAME"]) %>%
      group_by(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE) %>%
      summarise(n = n())
    if (nrow(df1) > 0){
      if (linecount == 1){
        result <- df1
      } else {
        result <- rbind(result, df1)
      }
      linecount <- linecount + 1
    }
  }
  result
}


#
#
# Difference from no 3: uses variable_name (e.g. VALUE_WW) given for each line in df_background
#
# EQS symbols
# Columns are added in correct sequence / correct naming for finished Excel file
#
#
#
write_excel_levels4 <- function(crit_col, filename, 
                                background_levels = df_background, raw_data = data_all, table_samples_specimens = df_samplesspec_2016, EQS_limits = EQS,
                                # trends_long, trends_10yr,
                                varnames,
                                yr1 = 1980, yr2 = 2016, 
                                cols = c("#FFFFFF", "#EDEDED", "#AFAFAF", "#777777", "#4F4F4F", "#000000")){
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Sheet1")
  
  logfile <- file(paste0(filename, "_log.txt"), "w+")                # Print i (index in 'background_levels' file) and j (station nr. j) to text file
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  
  # For now, we solve the task of specifying which variable to use by defining a new variable: VALUE_MEAS
  # raw_data$VALUE_MEAS <- raw_data[,variable_name]
  
  # Style for column headings
  cs_head <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white")
  
  # Style for cells with medians < reference level
  cs_back <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over reference level, but below 2 x reference level
  cs_over1 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over 2 x reference level, but below 5 x reference level
  cs_over2 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Etc.    
  cs_over3 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over4 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over5 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=TRUE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_EQS_green <- CellStyle(wb) +
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#4EB265") + 
    Fill(backgroundColor="white", foregroundColor="white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  cs_EQS_red <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Webdings", color = "#DC050C") + 
    Fill(backgroundColor = "white", foregroundColor = "white",
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_LEFT")
  
  N <- nrow(background_levels)
  
  # Set column names
  rows <- createRow(sheet, rowIndex=1)
  for (i in 1:length(varnames)){
    cell <- createCell(rows, colIndex = i)[[1,1]]
    setCellValue(cell, varnames[i])
    setCellStyle(cell, cs_head)
  }
  
  df_var <- data.frame(
    Name1 = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Variable", "UNIT", "Stations", "N_stations", "N", "Median", "Q95"),
    Name2 = c("Parameter Code", "Species", "Tissue", "Basis", "Unit", "Backgr_stations", "Backgr_Nstat", "Backgr_N", "Median", "Q95"),
    stringsAsFactors = FALSE
  )
  
  colno_station <- which(varnames %in% "Station Code")
  
  nyr <- yr2 - yr1 + 1
  txt_yr <- rep(yr1:yr2, each = 2)
  txt_yr <- substr(txt_yr, 3, 4)
  colno_Q1 <- which(varnames %in% paste0("Q", txt_yr[1]))   # number of the first 'Q' column (EQS symbol). Is used below to find where to start counting years
  last_level_column <- which(varnames %in% paste0("V", txt_yr[2]))                                  # column number of the last column with level data
  
  # Take care these have correct names
  # browser()
  colno_pool <- which(varnames %in% "Ant pr?ver 2016")   # number of pooling column
  colno_ddi  <- which(varnames %in% "N>LOQ(min-maks)")   # number of ddi column
  colno_sd2015  <- which(varnames %in% "SD 2015")   # number of column for SD 
  colno_sd2016  <- which(varnames %in% "SD 2016")   # number of column for SD 
  
  colno_trends_long  <- which(varnames %in% "Trend p(long)")   # numbers of the first column for long trends
  colno_trends_10yr  <- which(varnames %in% "Trend p(short)")  # numbers of the first column for long trends
  
  # set width of V columns
  cols_V <- varnames[seq(1, by = 2, length = nyr)]         # name of all 'V' columns 
  colno_V <- which(varnames %in% cols_V)                   # number of all 'V' columns
  setColumnWidth(sheet, colno_V, 6.8)                      # set width o
  
  # set width of Q columns
  cols_Q <- varnames[seq(2, by = 2, length = nyr)]         # name of all 'Q' columns
  colno_Q <- which(varnames %in% cols_Q)                   # number of all 'Q' columns
  setColumnWidth(sheet, colno_Q, 4)                        # set width
  
  raw_data <- as.data.frame(raw_data)
  
  linecount <- 2
  pb = txtProgressBar(min = 0, max = N)
  
  #i2 <- round(seq(0, N, length = no_parts), 0)[-1]
  #i1 <- c(1, head(i2,-1)+1)
  #for (i in i1[part]:i2[part]){
  
  for (i in 1:N){  # all
    # for (i in 108:109){  # test with HG data
    # for (i in 57:58){
    cat("\n", i, "\n", file = logfile)
    
    # raw_data$VALUE_MEAS <- raw_data[,variable_name]
    variable_name <- background_levels[i, "Variable"]
    raw_data$VALUE_MEAS <- raw_data[,variable_name]
    
    row_eqs <- which(EQS_limits$PARAM %in% background_levels[i, "PARAM"])[1]
    if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "UG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]
    } else if (!is.na(row_eqs) & background_levels$UNIT[i] %in% "MG_P_KG"){
      EQS <- EQS_limits$Limit[row_eqs]/1000
    } else {
      EQS <- NA
    }
    data_selection <- with(raw_data, YEAR %in% yr1:yr2 &
                             PARAM %in% background_levels[i, "PARAM"] & 
                             LATIN_NAME %in% background_levels[i, "LATIN_NAME"] & 
                             TISSUE_NAME %in% background_levels[i, "TISSUE_NAME"]  &
                             is.finite(raw_data[,"VALUE_MEAS"])
    )
    df1 <- raw_data[data_selection,] %>%
      group_by(STATION_CODE) %>%
      summarise(n = n())
    if (nrow(df1) > 0){
      for (j in 1:nrow(df1)){
        cat(j, "", file = logfile)
        rows <- createRow(sheet, rowIndex = linecount)
        # browser()
        for (m in 1:nrow(df_var)){
          colno <- which(varnames %in% df_var$Name2[m])
          cell <- createCell(rows, colIndex = colno)[[1,1]]
          setCellValue(cell, background_levels[i, df_var$Name1[m]])
        }
        # browser()
        cell <- createCell(rows, colIndex = colno_station)[[1,1]]
        setCellValue(cell, df1$STATION_CODE[j])
        df2_sel <- raw_data[,"YEAR"] %in% yr1:yr2 &
          raw_data[,"PARAM"] %in% background_levels[i, "PARAM"] & 
          raw_data[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
          raw_data[,"TISSUE_NAME"]%in% background_levels[i, "TISSUE_NAME"] &
          raw_data[,"STATION_CODE"] %in% df1$STATION_CODE[j] &                           
          is.finite(raw_data[,"VALUE_MEAS"])
        df2 <- raw_data[df2_sel,] %>%
          group_by(YEAR) %>%
          summarise(median_value = round(median(VALUE_MEAS),3))
        # subset(raw_data[df2_sel, ], select = c(YEAR, PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, VALUE_MEAS, FLAG1))
        # if (i == 105) browser()
        # if (df1$STATION_CODE[j] %in% "I301") browser()
        # browser()
        df2_flag_sel <- df2_sel & !is.na(raw_data[,"FLAG1"])
        df2_flag <- raw_data[df2_flag_sel,] %>%
          group_by(YEAR) %>%
          summarise(median_value_flag = round(median(VALUE_MEAS),3))
        df2 <- left_join(df2, df2_flag[,c("YEAR","median_value_flag")], by = "YEAR")
        # Add "<"
        # browser()
        df2$median_print <- ifelse(df2$median_value > df2$median_value_flag | is.na(df2$median_value_flag), df2$median_value, paste0("<", df2$median_value))
        # If you want, change decimal sign from "." to "," for all values:|
        # df2$median_print <- sub(".", ",", df2$median_print, fixed = TRUE)
        
        # Change decimal sign from "." to "," for those that don't have the < attached 
        # df2$median_print <- ifelse(df2$median_value > df2$median_value_flag | is.na(df2$median_value_flag), sub(".", ",", df2$median_print, fixed = TRUE), df2$median_print)
        
        df2_sd <- raw_data[df2_sel,] %>%
          group_by(YEAR) %>%
          summarise(SD = round(sd(VALUE_MEAS, na.rm = TRUE),3)) %>%
          as.data.frame()
        
        for (k in 1:nrow(df2)){
          col_ind2 <- colno_Q1 + 2*(df2$YEAR[k] - yr1)               # find which column number to put the EQS symbol in
          col_ind1 <- col_ind2 - 1                                   # find which column number to put the median in
          value <- round(df2$median_value[k], 3)                     # median value
          
          # Print median value
          cell <- createCell(rows, colIndex = col_ind1)[[1,1]]
          setCellValue(cell, df2$median_print[k])
          
          # Format median value
          if (!is.na(background_levels[i, crit_col])){
            if (value <= background_levels[i, crit_col]){
              setCellStyle(cell, cs_back)
            } else if (value > background_levels[i, crit_col] & value <= 2*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over1)
            } else if (value > 2*background_levels[i, crit_col] & value <= 5*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over2)
            } else if (value > 5*background_levels[i, crit_col] & value <= 10*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over3)
            } else if (value > 10*background_levels[i, crit_col] & value <= 20*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over4)
            } else if (value > 20*background_levels[i, crit_col]) {
              setCellStyle(cell, cs_over5)
            }
          }
          # EQS symbol - we use letter 'n', in Webdings font that becomes a 'bullet'
          cell <- createCell(rows, colIndex = col_ind2)[[1,1]]
          if (!is.na(EQS)){
            setCellValue(cell, "n")
            if (value <= EQS){
              setCellStyle(cell, cs_EQS_green)
            } else {
              setCellStyle(cell, cs_EQS_red)
            }
          }
        }
        # browser()
        # Add a cell with information about pooling
        txt_pool <- info_pooling(parameter = background_levels[i, "PARAM"], species = background_levels[i, "LATIN_NAME"], 
                                 tissue = background_levels[i, "TISSUE_NAME"], station = df1$STATION_CODE[j], year = yr2,
                                 raw_data = raw_data, table_samples_specimens = table_samples_specimens)
        cell <- createCell(rows, colIndex = colno_pool)[[1,1]]
        setCellValue(cell, txt_pool)
        # Add another cell with information about D.d.i. (values above LOQ)
        txt_ddi <- info_ddi(parameter = background_levels[i, "PARAM"], species = background_levels[i, "LATIN_NAME"], 
                            tissue = background_levels[i, "TISSUE_NAME"], station = df1$STATION_CODE[j], year = yr2,
                            raw_data = raw_data)
        cell <- createCell(rows, colIndex = colno_ddi)[[1,1]]
        setCellValue(cell, txt_ddi)
        # Add SD 2015 and 2016
        sel <- df2_sd[,"YEAR"] %in% 2015
        if (sum(sel) == 1){
          txt_sd <- df2_sd[sel, "SD"]
          cell <- createCell(rows, colIndex = colno_sd2015)[[1,1]]
          setCellValue(cell, txt_sd)
        }
        sel <- df2_sd[,"YEAR"] %in% 2016
        if (sum(sel) == 1){
          txt_sd <- df2_sd[sel, "SD"]
          cell <- createCell(rows, colIndex = colno_sd2016)[[1,1]]
          setCellValue(cell, txt_sd)
        }
        # sel1 <- trends_long[,"PARAM"] %in% background_levels[i, "PARAM"] & 
        #   trends_long[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
        #   trends_long[,"TISSUE_NAME"] %in% background_levels[i, "TISSUE_NAME"] & 
        #   trends_long[,"STATION_CODE"] %in% df1$STATION_CODE[j]        
        # trends_long_results <- with(trends_long[sel1,], 
        #   c(P_change, 
        #     100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1),
        #     Year1, Year2, N))
        # sel2 <- trends_10yr[,"PARAM"] %in% background_levels[i, "PARAM"] & 
        #   trends_10yr[,"LATIN_NAME"] %in% background_levels[i, "LATIN_NAME"] & 
        #   trends_10yr[,"TISSUE_NAME"] %in% background_levels[i, "TISSUE_NAME"] & 
        #   trends_10yr[,"STATION_CODE"] %in% df1$STATION_CODE[j]        
        # trends_10yr_results <- with(trends_10yr[sel2,], 
        #   c(P_change, 
        #     100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1),
        #     Year1, Year2, N))
        # cell <- createCell(rows, colIndex = colno_trends_long + seq(0,4))[[1,1]]
        # setCellValue(cell, trends_long_results)
        # cell <- createCell(rows, colIndex = colno_trends_10yr + seq(0,4))[[1,1]]
        # setCellValue(cell, trends_10yr_results)
        # block <- CellBlock(sheet, startRow = linecount, startColumn = colno_trends_long, noRows = 1, noColumns = 5)
        # CB.setRowData(block, trends_long_results, 1)
        # block <- CellBlock(sheet, startRow = linecount, startColumn = colno_trends_10yr, noRows = 1, noColumns = 5)
        # CB.setRowData(block, trends_10yr_results, 1)
        
        # if (sum(sel1) > 0)
        #   for (i in 1:5){
        #     cell <- createCell(rows, colIndex = colno_trends_long + i - 1)[[1,1]]
        #     setCellValue(cell, trends_long_results[i])
        #     }
        # if (sum(sel2) > 0)
        #   for (i in 1:5){
        #     cell <- createCell(rows, colIndex = colno_trends_10yr + i - 1)[[1,1]]
        #     setCellValue(cell, trends_10yr_results[i])
        #     }
        
        linecount <- linecount + 1
      }
    }
    setTxtProgressBar(pb, i)
  }
  saveWorkbook(wb, filename)
  cat("\n\n")
  cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  close(logfile)
  
}

#
# Note: takes varuable name (e.g. VALUE_WW) from 'background_levels'
#

write_excel_levels4_identifiers_only <- function(background_levels = df_background, raw_data = data_all,
                                                 yr1 = 1980, yr2 = 2016){
  
  N <- nrow(background_levels)
  raw_data <- as.data.frame(raw_data)
  linecount <- 1
  
  for (i in 1:N){  # all
    variable_name <- background_levels[i, "Variable"]
    sel <- is.finite(raw_data[,variable_name])
    df1 <- subset(raw_data[sel,], YEAR %in% yr1:yr2 &
                    PARAM %in% background_levels[i, "PARAM"] & 
                    LATIN_NAME %in% background_levels[i, "LATIN_NAME"] & 
                    TISSUE_NAME %in% background_levels[i, "TISSUE_NAME"]) %>%
      group_by(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE) %>%
      summarise(n = n())
    if (nrow(df1) > 0){
      df1 <- data.frame(df1, Variable = variable_name, stringsAsFactors = FALSE)
      if (linecount == 1){
        result <- df1
      } else {
        result <- rbind(result, df1)
      }
      linecount <- linecount + 1
    }
  }
  result
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Function for rank line plot (plot.qual) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# https://beckmw.wordpress.com/2013/04/01/a-nifty-line-plot-to-visualize-multivariate-time-series/
# https://gist.githubusercontent.com/fawda123/5281518/raw/216bb632c728f4df0ec45b53d766e971158a126e/plot_qual.r
#

# Input:
# x	        data frame or matrix with input data, first column is time step
# x.locs    minimum and maximum x coordinates for plotting region, from 0-1
# y.locs    minimum and maximum y coordinates for plotting region, from 0-1
# steps	    character string of time steps to include in plot, default all
# sp.names  character string of species connections to display, default all
# dt.tx     logical value indicating if time steps are indicated in the plot
# rsc       logical value indicating if line widths are scaled proportionally to their value
# ln.st	    numeric value for distance of lines from text labels, distance is determined automatically if not provided
# rs.ln	    two-element numeric vector for rescaling line widths, defaults to one value if one element is supplied, default 3 to 15
# ln.cl	    character string indicating color of lines, can use multiple colors or input to brewer.pal, default 'RdYlGn'
# alpha.val	numeric value (0-1) indicating transparency of connections, default 0.7
# leg       logical value for plotting legend, default T, values are for the original data frame, no change for species or step subsets
# rnks      logical value for showing only the ranks in the text labels
# ...       additional arguments to plot


plot.qual<-function(x,x.locs=c(0.01,0.99),y.locs=c(0,1),steps=NULL,sp.names=NULL,dt.tx=T,rsc=T,
                    ln.st=NULL,rs.ln=c(3,15),ln.cl='RdYlGn',alpha=0.7,leg=T,rnks=FALSE,...){
  
  require(RColorBrewer)
  require(scales)
  
  if(length(x.locs) != 2 | length(y.locs) != 2) 
    stop('x and y dimensions must be two-element vectors')
  
  if(x.locs[1]<0 | x.locs[2]>1 | y.locs[1]<0 | y.locs[2]>1) 
    stop('x and y dimensions must in range of 0--1')
  
  dim.x<-c(0,1) #plot dims x
  dim.y<-c(0,1) #plot dims y
  wrn.val<-F
  
  x[,1]<-as.character(x[,1]) 
  tot.sp<-ncol(x)-1
  sp.col<-2:ncol(x)
  
  #rescale if T, sort legend for later
  sp.orig<-x[,sp.col]
  if(length(rs.ln)==1) rsc<-F
  if(rsc) x[,sp.col]<-rescale(as.matrix(x[,sp.col]),rs.ln)
  if(rsc==F & leg) leg<-F #no legend if line widths aren't rescaled
  
  #reorder species columns, add rank as integer
  first.ord<-order(x[1,sp.col],decreasing=T)
  x[,sp.col]<-x[,sp.col][,first.ord]
  names(x)[sp.col]<-names(x)[sp.col][first.ord]
  names(x)[sp.col]<-paste(1:tot.sp,names(x)[sp.col],sep='. ')
  
  #list of spp by date, arranged in decreasing order for each date
  dt.dat.srt<-vector('list',nrow(x))
  names(dt.dat.srt)<-x[,1]
  for(val in 1:nrow(x)){
    tmp<-t(x[val,sp.col])
    tmp<-tmp[order(tmp,decreasing=T),,drop=F]
    dt.dat.srt[[val]]<-tmp
  }
  
  #initiate plot object
  plot(dim.x,dim.y,type='n',axes=F,xlab='',ylab='',...) 
  
  #subset for steps, if provided
  if(!is.null(steps)) dt.dat.srt<-dt.dat.srt[names(dt.dat.srt) %in% steps]
  
  #plot legend
  if(leg){
    y.locs[1]<-0.05*diff(y.locs)+y.locs[1]
    leg.txt<-format(round(seq(min(sp.orig),max(sp.orig),length=5),2),nsmall=2,digits=2)
    leg.wds<-seq(rs.ln[1],rs.ln[2],length=5)
    legend('bottom',(y.locs[1]-y.olds)/2,col=alpha('black',alpha),lwd=leg.wds,legend=leg.txt,bty='n',
           horiz=T)
  }	
  
  #x locations
  x.vals<-rep(seq(x.locs[1],x.locs[2],length=length(dt.dat.srt)),each=tot.sp)
  x.vals<-split(x.vals,x.vals)
  
  #y locations, rearranged in loop, exception if dates are plotted
  if(dt.tx) y.vals<-rev(seq(y.locs[1],y.locs[2],length=tot.sp+1))[-1]
  else y.vals<-rev(seq(y.locs[1],y.locs[2],length=tot.sp))
  
  #get line colors
  if(length(ln.cl)==1)
    if(ln.cl %in% row.names(brewer.pal.info)){
      pal.num<-brewer.pal.info[row.names(brewer.pal.info) == ln.cl,1]
      ln.cl<-brewer.pal(pal.num,ln.cl)
    }
  line.cols<-alpha(colorRampPalette(ln.cl)(tot.sp),alpha)
  
  #define distance of lines from labels
  if(is.null(ln.st)){
    str.max<-max(strwidth(row.names(dt.dat.srt[[1]])))
    if(diff(x.locs)-length(dt.dat.srt)*str.max < 0){
      warning('not enough space for lines between columns')
      wrn.val<-T
    }
    else
      ln.st<-0.2*str.max + str.max/2
  }
  
  for(val in 1:(length(dt.dat.srt)-1)){
    
    #temp data to plot
    plt.tmp<-dt.dat.srt[c(val,val+1)]
    x.tmp<-x.vals[c(val,val+1)]
    
    #plot temp text for column, remove spp if rnks
    rowtxt <- row.names(plt.tmp[[1]])
    if(rnks)
      rowtxt <- gsub('([1-9]*)\\..*', '\\1', rowtxt) 
    text(x.tmp[[1]],y.vals,rowtxt)
    
    if(val == length(dt.dat.srt)-1){
      rowtxt <- row.names(plt.tmp[[2]])
      if(rnks)
        rowtxt <- gsub('([1-9]*)\\..*', '\\1', rowtxt) 
      text(x.tmp[[2]],y.vals,rowtxt)
      if(dt.tx){
        dt.txt<-substitute(italic(x),list(x=names(plt.tmp)[2]))
        text(unique(x.tmp[[2]]),y.locs[2],dt.txt)
      }
    }	
    
    if(dt.tx){
      dt.txt<-substitute(italic(x),list(x=names(plt.tmp)[1]))
      text(unique(x.tmp[[1]]),y.locs[2],dt.txt)
    }
    
    srt.ln.y<-match(row.names(plt.tmp[[1]]),row.names(plt.tmp[[2]]))
    
    #if no line rescale, use first element of rs.ln
    if(rsc) lwd.val<-plt.tmp[[1]][,1]
    else lwd.val<-rep(rs.ln[1],tot.sp)
    
    #vector for species selection of line segments
    if(is.null(sp.names)) sel.sp<-rep(T,tot.sp)
    else{
      sel.names<-unlist(lapply(strsplit(row.names(plt.tmp[[1]]),' '),function(x) x[2]))
      sel.sp<-(sel.names %in% sp.names)
    }
    
    #for lines
    if(!wrn.val)
      segments(
        x.tmp[[1]][sel.sp]+ln.st,
        y.vals[sel.sp],
        x.tmp[[2]][sel.sp]-ln.st,
        y.vals[srt.ln.y][sel.sp],
        col=line.cols[sel.sp],
        lwd=lwd.val[sel.sp]
      )
    
    #resort color vector for next colummn
    srt.cl.y<-match(row.names(plt.tmp[[2]]),row.names(plt.tmp[[1]]))
    line.cols<-line.cols[srt.cl.y]
    
  }
  
}


test <- FALSE

if (test){
  #create random data
  set.seed(5)
  
  #time steps
  step<-as.character(seq(2007,2012))
  
  #species names
  sp<-paste0('sp',seq(1,25))
  
  #random data for species frequency occurrence
  sp.dat<-runif(length(step)*length(sp),0,15)
  
  #create data frame for use with plot
  sp.dat<-matrix(sp.dat,nrow=length(step),ncol=length(sp))
  sp.dat<-data.frame(step,sp.dat)
  names(sp.dat)<-c('step',sp)
  
  str(sp.dat)
  head(sp.dat)
  
  #reassign values of four variables
  
  #pick random species names
  vars<-sp[sp %in% sample(sp,4)]
  
  #function for getting value at each time step
  time.fun<-function(strt.val,steps,mean.val,lim.val){
    step<-1
    x.out<-strt.val
    while(step<steps){
      x.new<-x.out[step] + rnorm(1,mean=mean.val)
      x.out<-c(x.out,x.new)
      step<-step+1
    }
    if(lim.val<=0) return(pmax(lim.val,x.out))
    else return(pmin(lim.val,x.out))
  }
  
  #use function to reassign variable values
  sp.dat[,vars[1]]<-time.fun(14.5,6,-3,0) #start at 14.5, decrease rapidly
  sp.dat[,vars[2]]<-time.fun(13.5,6,-1,0) #start at 13.5, decrease less rapidly
  sp.dat[,vars[3]]<-time.fun(0.5,6,3,15) #start at 0.5, increase rapidly
  sp.dat[,vars[4]]<-time.fun(1.5,6,1,15) #start at 1.5, increase less rapidly
  
  windows(10,7)
  par(mar=numeric(4),family='serif')
  plot.qual(sp.dat,rs.ln=c(3,15))
}



