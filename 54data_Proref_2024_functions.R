#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 
#
# Functions -----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o 

# get_data_summary
# - returns something like
# A tibble: 433 × 3
# PARAM                        cod mussel
# <chr>                      <int>  <int>
#   1 % C__WW                       18     29
#   2 % N__WW                       18     29
#   3 1,2,3,4,6,7,8-HeptaCDD__WW     0      2
#   4 1,2,3,4,6,7,8-HeptaCDF__WW     0      2


# get_coverage_data
# example:
# - get_coverage_data(data_all2) %>% select(PARAM, PARAM_orig, Component.Name, Substance.Group, cod, mussel)
# - returns something like (with a lot more columns)
#     PARAM PARAM_orig Component.Name Substance.Group         cod mussel
#   1 AG    AG__WW     silver         Metals and metalloids    20     70
#   2 AS    AS__WW     arsenic        Metals and metalloids    24     87
#   3 BA    BA__WW     barium         Metals and metalloids     0     10

# get_coverage(data_all2)
# get_coverage_species(data_all2)
# - returns something like
#         mussel
# cod     FALSE TRUE
#   FALSE     0   16
#   TRUE      3  130



get_data_summary <- function(data){
  data %>% 
    ungroup() |>
    distinct(PARAM, LATIN_NAME, STATION_CODE) %>% 
    count(PARAM, LATIN_NAME) %>% 
    filter(grepl("__WW$", PARAM) & 
             LATIN_NAME %in% c("Gadus morhua", "Mytilus edulis")) %>% 
    pivot_wider(names_from = LATIN_NAME, values_from = n, values_fill = 0) %>% 
    rename(cod = `Gadus morhua`, mussel = `Mytilus edulis`)
}
if (FALSE){
  get_data_summary(data_all2)
}

# global parameter: 'lookup_param'  
get_coverage_data <- function(data){
  summ <- get_data_summary(data)
  lookup_param %>% 
    left_join(summ, by = join_by(PARAM_orig == PARAM), relationship = "one-to-one") %>%
    mutate(
      cod = ifelse(is.na(cod), 0, cod),
      mussel = ifelse(is.na(mussel), 0, mussel)
    )
}

if (FALSE){
  # debugonce(get_coverage_data)
  get_coverage_data(data_all2)
  get_coverage_data(data_all2_filtered) %>% 
    filter(cod == 0 & mussel == 0) %>% 
    select(PARAM, Component.Name, Substance.Group)
}

# global parameter: 'lookup_param'  
get_coverage <- function(data){
  cover <- get_coverage_data(data)
  cover %>% 
    mutate(
      covered = cod > 0 | mussel > 0
      ) |> 
    xtabs(~covered, data = _)
}

# global parameter: 'lookup_param'  
get_coverage_species <- function(data){
  cover <- get_coverage_data(data)
  cover %>% 
    mutate(cod = cod > 0, mussel = mussel > 0) |> 
    xtabs(~cod + mussel, data = _)
}

# global parameter: 'lookup_param'  
get_coverage <- function(data){
  cover <- get_coverage_data(data)
  cover %>% 
    mutate(
      covered = cod > 0 | mussel > 0
      ) |> 
    xtabs(~covered, data = _)
}

# global parameter: 'lookup_param'  
get_coverage_missing <- function(data){
  get_coverage_data(data) %>% 
    filter(cod == 0 & mussel == 0) %>% 
    select(PARAM, Component.Name, Substance.Group)
}

if (FALSE){
  # example
  summ <- data_summary(data_all2)
  cover <- get_coverage_data(data_all2) %>% select(PARAM, PARAM_orig, cod, mussel)
  get_coverage(data_all2)
}