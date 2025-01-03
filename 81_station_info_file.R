
# for Merete G

library(dplyr)
library(readxl)

dat1_orig <- read_excel("Info/011B stations PROREF 02012025.xlsx") 
dat1 <- dat1_orig %>%
  select(Species, StationCode)
  
#
# Add coordinates ----
#

dat_coord <- read_excel("../Milkys2_pc/Files_to_Jupyterhub_2021/Kartbase_edit.xlsx")

dat2a <- dat1 %>%
  left_join(
    dat_coord %>% select(StationCode = STATION_CODE, Latitude = Lat, Longitude = Long),
    by = join_by(StationCode)
  )

#
#

coord_lacking <- dat2a %>%
  filter(is.na(Longitude)) %>%
  pull(StationCode)

library(sf)
library(ggplot2)
# Norway (coastline) in UTM coordinates
shape <- st_read("milkys_shape_2025_01_03_14_32")
plot(shape)
st_crs(shape) <- "+proj=utm +zone=33"  # set coordinate system
df_shape1 <- as.data.frame(shape)

df_shape2 <- df_shape1 %>%
  distinct(StationCode = STATION_CO, LATITUDE, LONGITUDE) %>%
  arrange(StationCode) %>%
  filter(StationCode %in% coord_lacking) 

dat2b <- dat2a %>%
  left_join(df_shape2, by = join_by(StationCode)) %>%
  mutate(
    Longitude = case_when(
      StationCode %in% coord_lacking ~ LONGITUDE,
      TRUE ~ Longitude),
    Latitude = case_when(
      StationCode %in% coord_lacking ~ LATITUDE,
      TRUE ~ Latitude))


#
# Add other info ----
#

dat_all1 <- readRDS("Data/54_data_2024.rds") %>%
  filter(grepl("__WW", PARAM)) %>%
  mutate(PARAM = sub("__WW", "", PARAM))


# Add parameter groups

dat_param_raw <- readxl::read_excel("Input_data/2022/010C PARAMs with PROREF WW.xlsx")
dat_param <- dat_param_raw %>%
  distinct(PARAM, Substance.Group) 

check <- dat_param %>%
  add_count(PARAM) %>%
  filter(n > 1)

if (nrow(check) > 0){
  stop("Some PARAM have more than one Substance.Group")
}

check2 <- dat_param %>%
  filter(is.na(Substance.Group))

dat_all2 <- dat_all1 %>%
  # remove the original one
  select(-Substance.Group) %>%
  left_join(dat_param,
            by = join_by(PARAM))

dat_summ <- dat_all2 %>%
  arrange(STATION_CODE, LATIN_NAME, PARAM) %>%
  group_by(
    STATION_CODE, LATIN_NAME) %>%
  summarize(
    `Year started` = min(YEAR),
    `Year ended` = max(YEAR),
    `Parameters monitored during period` = paste(unique(PARAM), collapse = ','),
    Substance.Group = paste(unique(Substance.Group), collapse = ','),
    .groups = 'drop'
  ) %>%
  mutate(
    Species = case_when(
      LATIN_NAME == "Gadus morhua" ~ "Cod",
      LATIN_NAME == "Mytilus edulis" ~ "Mussel")) %>%
  select(
    Species, StationCode = STATION_CODE, `Year started`, `Year ended`, `Parameters monitored during period`, Substance.Group)
  
dat3 <- dat2b %>%
  select(-LATITUDE, -LONGITUDE) %>%
  left_join(dat_summ,
            by = join_by(Species, StationCode))


#
# Save
#

writexl::write_xlsx(dat3, "Info/011B stations PROREF 03012025c.xlsx")


#
# For interactive map (script 82)
#
# Not finished 
#

txt1 <- c("Biological effects: molecular/biochemical/cellular/assays", 
  "Biomarkers", "Chlorinated paraffins", "Chlorobiphenyls", "Dichloro-diphenyl-trichloroethane (DDTs)", 
  "Fat and dry weight", "Hexachlorocyclohexanes", "Isotopes", "Metals and metalloids", 
  "Organo-metallic compounds", "Organobromines", "Organochlorines (general)", 
  "Organofluorines", "Others", "Pesticides", "Phenols/chlorophenols", 
  "Phosphorus flame retardant (PFR)", "Polycyclic aromatic hydrocarbons (PAHs)", 
  "Siloxans")
txt2 <- c("Bioeff",
          "Biomark", "ChlParaf", "PCBs", "DDTs",
          "Fat/drywt", "OtherChl", "Isotop", "Metals",
          "OrgMetal", "OrgBrom", "OtherChl",
          "OrgFluor", "Others", "Pestic", "Phenol",
          "PFRs", "PAHs", 
          "Silox")

dat_all3 <- dat_all2 %>%
  left_join(data.frame(Substance.Group = txt1, Substance.Group.short = txt2))

# NOT USED
dat_substances <- dat_all3 %>%
  arrange(STATION_CODE, LATIN_NAME, PARAM) %>%
  mutate(
    Species = case_when(
      LATIN_NAME == "Gadus morhua" ~ "Cod",
      LATIN_NAME == "Mytilus edulis" ~ "Mussel")) %>%
  distinct(
    Species, StationCode = STATION_CODE, Substance.Group.short) %>%
  mutate(
    Present = "x") %>%
  filter(
    !is.na(Substance.Group.short)) %>%
  tidyr::pivot_wider(names_from = Substance.Group.short, values_from = Present)


dat_summ_map <- dat_all3 %>%                      # changed this to dat_all3
  arrange(STATION_CODE, LATIN_NAME, PARAM) %>%
  filter(!is.na(Substance.Group.short)) %>%       # added
  group_by(
    STATION_CODE, LATIN_NAME) %>%
  summarize(
    `Year started` = min(YEAR),
    `Year ended` = max(YEAR),
    `Parameters monitored during period` = paste(unique(PARAM), collapse = ','),
    Substance.Group = paste(unique(Substance.Group.short), collapse = ','),       # changed this to 'Substance.Group.short'
    .groups = 'drop'
  ) %>%
  mutate(
    Species = case_when(
      LATIN_NAME == "Gadus morhua" ~ "Cod",
      LATIN_NAME == "Mytilus edulis" ~ "Mussel")) %>%
  select(
    Species, StationCode = STATION_CODE, `Year started`, `Year ended`, `Parameters monitored during period`, Substance.Group)

dat3_map <- dat2b %>%
  select(-LATITUDE, -LONGITUDE) %>%
  left_join(dat_summ_map,
            by = join_by(Species, StationCode))

#
# Save
#

writexl::write_xlsx(dat3_map, "Info/011B stations PROREF 03012025c_map.xlsx")






