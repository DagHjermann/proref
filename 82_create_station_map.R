
#
# Created with Claude (and some extra additions from myself) 03.01.2024
#

# Sample data used:
# dat <- structure(list(
#   Species = c("Mussel", "Mussel", "Cod"),
#   StationCode = c("I307", "I242", "80B"),
#   Latitude = c(59.7445, 60.3948, 63.4456),
#   Longitude = c(10.5228, 5.2668, 10.3717),
#   `Year started` = c(1995, 1995, 2009),
#   `Year ended` = c(2016, 2010, 2022)
# ), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))

library(flexdashboard)
library(leaflet)
library(DT)
library(crosstalk)

writeLines('---
title: "Research Stations"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
library(leaflet)
library(DT)
library(crosstalk)
library(readxl)
library(dplyr)

dat <- readxl::read_excel("Info/011B stations PROREF 03012025c_map.xlsx") %>% 
  select(-`Parameters monitored during period`) %>%
  arrange(Species, StationCode) %>%
  mutate(
    Longitude = round(Longitude, 5),
    Latitude = round(Latitude, 5)
  )

sd <- SharedData$new(dat)

# Create filters
filter_species <- filter_select("species", "Species", sd, ~Species)
filter_station <- filter_select("station", "StationCode", sd, ~StationCode)
filter_year1 <- filter_slider("year1", "Year started", sd, ~`Year started`, sep = "")
filter_year2 <- filter_slider("year2", "Year ended", sd, ~`Year ended`, sep = "")
# filter_lat <- filter_slider("lat", "Latitude", sd, ~Latitude)
# filter_long <- filter_slider("long", "Longitude", sd, ~Longitude)
filter_substance <- filter_select("substance", "Substance groups", sd, ~Substance.Group)

```

Column {data-width=650}
-----------------------------------------------------------------------

### Map

```{r}
leaflet(sd) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    color = ~ifelse(Species == "Mussel", "blue", "red"),
    radius = 8,
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste("Station:", StationCode, "<br>",
                  "Species:", Species, "<br>",
                  "Years:", `Year started`, "-", `Year ended`, "<br>",
                  "Substance groups:", Substance.Group),
    label = ~StationCode
  )
```

Column {data-width=350}
-----------------------------------------------------------------------

### Filters
```{r}
filter_species
filter_station
filter_year1
filter_year2
filter_substance
```

### Data Table
```{r}
datatable(
  sd,
  options = list(
    pageLength = 50,
    scrollY = "600px",
    dom = "t",
    scrollCollapse = TRUE
  ),
  rownames = FALSE
)
```
', "station_dashboard.Rmd")

rmarkdown::render("station_dashboard.Rmd")
