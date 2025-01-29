library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggrepel)
library(glue)

source("app_functions.R")

#
# data ----
#

datasets <- tibble::tribble(
  ~analysis_name, ~fn_rawdata, ~fn_series, ~fn_result, ~year1, ~year2,
  "Original (1992-2022)", "54_data_2024.rds", "54_dataseries_2024.rds", "54_result_detailed_2024-11-29.rds", 1992, 2022,
  "Original (2003-2022)", "54_data_2024.rds", "54_dataseries_2024.rds", "54_result_detailed_2003-2022_2025-01-05-T1812.rds", 2003, 2022,
  "LOQ-filtered (2003-2022)", "54_data_2024_loqfilter3x.rds", "54_dataseries_2024_loqfilter3x.rds", "54_result_detailed_2003-2022_2025-01-20-T1603.rds", 2003, 2022
)

read_data <- function(fn){
  readRDS(paste0("../Data/", fn))
}

result_detailed <- bind_rows(
  bind_cols(data.frame(Analysis = datasets$analysis_name[1]), read_data(datasets$fn_result[1])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[2]), read_data(datasets$fn_result[2])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[3]), read_data(datasets$fn_result[3]))
) %>%
  mutate(Analysis = fct_inorder(Analysis))


#
# Calculate proref value ----
#

lookup_background <- result_detailed %>%
  select(Analysis, Station, LATIN_NAME, PARAM, Background1b) %>%
  rename(Background = Background1b)

#
# .-- raw data ----
# 

# Read raw data (avoid reading same file twice by using 'unique')
rawdata_list <- lapply(unique(datasets$fn_rawdata), read_data)

# Get index for which data in 'rawdata_list' tha belongs to which row in 'datasets'
#   (same length as 'datasets')
get_index <- function(fn)
  which(unique(datasets$fn_rawdata) == fn) 
raw_index <- sapply(datasets$fn_rawdata,get_index)

# Make combined data frame for raw data for all three analyses
data_all_comb <- bind_rows(
  bind_cols(data.frame(Analysis = datasets$analysis_name[1]), rawdata_list[[raw_index[1]]] %>% filter(YEAR %in% datasets$year1[1]:datasets$year2[1])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[2]), rawdata_list[[raw_index[2]]] %>% filter(YEAR %in% datasets$year1[2]:datasets$year2[2])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[3]), rawdata_list[[raw_index[3]]] %>% filter(YEAR %in% datasets$year1[3]:datasets$year2[3]))
) %>%
  mutate(Analysis = fct_inorder(Analysis))
# table(data_all_comb$Analysis)
# Original (1992-2022)     Original (2003-2022) LOQ-filtered (2003-2022) 
#              2124276                  1580937                  1378626 

#
# .-- background stations ----
#
data_backgr_all <- lookup_background %>%
  # These parameters are only found in raw data file number 1 - they were fixed later
  filter(!grepl("SCCP__", PARAM)) %>%
  filter(!grepl("MCCP__", PARAM)) %>%
  filter(!grepl("Krysen__", PARAM)) %>%
  select(Analysis, Station, LATIN_NAME, PARAM, Background) %>%
  left_join(data_all_comb %>%
              filter(!is.na(Concentration)) %>%
              select(Analysis, Station = STATION_CODE, LATIN_NAME, PARAM, YEAR, Concentration, FLAG1), 
            by = join_by(Analysis, Station, LATIN_NAME, PARAM), 
            relationship = "one-to-many") %>%
  filter(!is.na(Background)) %>%
  mutate(
    Station_bg = case_when(
      Background %in% "Other" ~ NA,
      Background %in% "Background" ~ Station)
  ) %>%
  arrange(LATIN_NAME, PARAM, Analysis, desc(Background), Station_bg) %>%
  mutate(LOQ = case_when(
    is.na(FLAG1) ~ "Over LOQ",
    TRUE ~ "Under LOQ"))

#
# .-- proref value (90th percentile)----
#
data_proref <- data_backgr_all %>% 
  filter(Background %in% "Background") %>%
  group_by(Analysis, LATIN_NAME, PARAM) %>%
  summarize(PROREF = quantile(Concentration, 0.9) %>% signif(3),
            Median = quantile(Concentration, 0.5) %>% signif(3),
            .groups = "drop")

params_menu <- readRDS(paste0("../Data/", datasets$fn_result[3])) %>%
  pull(PARAM) %>%
  unique()


#
# UI ----------------------------------------------------------------------------
#

ui <- fluidPage(
  titlePanel("Parameter and Species Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Analysis selection
      selectInput("analysis1", "Analysis 1",
                  choices = datasets$analysis_name,
                  selected = datasets$analysis_name[1]),
      selectInput("analysis2", "Analysis 2",
                  choices = datasets$analysis_name,
                  selected = datasets$analysis_name[2]),
      
      # Parameter selection with text input for partial matching
      # textInput("param", "Parameter (partial matching)", value = "CB118__WW"),
      selectizeInput("param", "Parameter", 
                     choices = params_menu, 
                     selected = "CB118__WW", multiple = FALSE,
                     options = NULL),
      
      # Species selection
      selectInput("species", "Species",
                  choices = c("mussel", "cod"),
                  selected = "cod"),
      
      # Add info about the selected parameter
      verbatimTextOutput("param_info")
    ),
    
    mainPanel(
      # Three plots in tabs
      tabsetPanel(
        tabPanel("Algorithm Plot", 
                 plotOutput("plot1"),
                 shiny::numericInput("max_rank", "Show first ... stations", value = 50)),
        tabPanel("Time Range Plot", plotOutput("plot2")),
        tabPanel("Medians Plot", plotOutput("plot3"),
                 shiny::checkboxInput("plot3_log", "Log y axis", value = TRUE)),
        tabPanel("Raw data Plot", plotOutput("plot4"),
                 shiny::checkboxInput("plot4_log", "Log y axis", value = FALSE),
                 shiny::checkboxInput("show_all_data", "Show all stations", value = TRUE),
                 shiny::checkboxInput("restrict_y_axis", "Restrict y axis to background station data", value = TRUE)
        )
      )
    )
  )
)

#
# Server ------------------------------------------------------------------------
#

server <- function(input, output, session) {
  
  # reactive: selected_data ----
  # Reactive values for filtered data
  
  selected_data <- reactive({
    
    analysis1 <- input$analysis1
    analysis2 <- input$analysis2
    
    param <- input$param
    
    # Convert species common name to Latin name
    species <- if (input$species == "mussel") {
      "Mytilus edulis"
    } else {
      "Gadus morhua"
    }
    
    # . - select data ---- 
    
    result_sel <- result_detailed %>%
      filter(Analysis %in% c(analysis1, analysis2),
             PARAM == param,
             LATIN_NAME == species) %>%
      mutate(Analysis = fct_drop(Analysis))
    
    data_proref_sel <- data_proref %>%
      filter(Analysis %in% c(analysis1, analysis2),
             PARAM == param,
             LATIN_NAME == species) %>%
      mutate(Analysis = fct_drop(Analysis))
    
    data_sel <- data_all_comb %>%
      filter(Analysis %in% c(analysis1, analysis2),
             PARAM == param,
             LATIN_NAME == species) %>%
      mutate(Analysis = fct_drop(Analysis))
    
    data_backgr_sel <- data_backgr_all %>%
      filter(Analysis %in% c(analysis1, analysis2),
             PARAM == param,
             LATIN_NAME == species) %>%
      mutate(Analysis = fct_drop(Analysis))
    
    # For plotting line at 2x the cleanest station
    line_2x_cleanest <- result_sel %>%
      group_by(Analysis) %>%
      summarize(Median2 = 2*first(Median2))
    
    # For setting default y axis range
    max_bg = data_backgr_sel %>%
      filter(Background %in% "Background") %>%
      pull(Concentration) %>%
      max()
    
    # For setting colours
    number_bg_stations <- unique(data_backgr_sel$Station_bg) %>% length()

    list(result_sel = result_sel, 
         line_2x_cleanest = line_2x_cleanest,
         data_sel = data_sel,
         data_backgr_sel = data_backgr_sel,
         data_proref_sel = data_proref_sel,
         max_bg = max_bg)
    
  })
  
  # Plot 1: Algorithm plot ----
  output$plot1 <- renderPlot({
    
    req(selected_data())

    result_sel <- selected_data()$result_sel %>%
      filter(Rank <= input$max_rank)
    
    # line_2x_cleanest <- selected_data()$line_2x_cleanest
    
    gg <- ggplot(result_sel, aes(Rank, Median2)) +
      geom_line() +
      geom_point(aes(col = Background1b)) +
      geom_text_repel(aes(label = glue("{Station} (P = {P_round})")), hjust = 0, 
                      nudge_x = 0.5, point.padding = 0.2, box.padding = 0.25, direction = "y") +
      # Line at 2x the cleanest station
      geom_hline(data = selected_data()$line_2x_cleanest, aes(yintercept = Median2),
                linetype = "dashed", colour = "red") +
      # Proref line
      geom_hline(
        data = selected_data()$data_proref_sel, aes(yintercept = PROREF),
        colour = "blue2", linetype = "dashed") +
      expand_limits(y = 0) +
      facet_wrap(vars(Analysis)) +
      labs(subtitle = "Blue line = PROREF, red line = 2x cleanest station") +
      theme_bw() +
      theme(
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 12)
      ) 
    
    if (nrow(result_sel) == 0) { 
      return(NULL)
    } else {
      return(gg)
    }
    
  })
  
  # Plot 2: Time range plot ----
  output$plot2 <- renderPlot({
    
    # overall median by station 
    
    data_overall_median <- selected_data()$data_backgr_sel %>%
      ungroup() %>%
      group_by(Analysis, Station) %>%
      summarize(Median = median(Concentration),
                Min_year = min(YEAR),
                Max_year = max(YEAR),
                Station_bg = first(Station_bg),
                .groups = "drop") %>%
      mutate(
        Background = case_when(
          !is.na(Station_bg) ~ "Background",
          is.na(Station_bg) ~ "Other")) 
    
    # overall median + time range plot 

    gg <- ggplot(data_overall_median %>%
             arrange(Analysis, desc(Background)), aes(x = Min_year, xend = Max_year, y = Median)) +
      geom_segment(aes(col = Background)) +
      # Proref line
      geom_hline(
        data = selected_data()$data_proref_sel, aes(yintercept = PROREF),
        colour = "blue2", linetype = "dashed") +
      expand_limits(y = 0) +
      facet_wrap(vars(Analysis)) +
      labs(subtitle = "Median + time range for each time series (red = background stations). Blue line = PROREF") +
      theme_bw() +
      theme(
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 12)
      ) 
    
    if (nrow(data_overall_median) == 0) { 
      return(NULL)
    } else {
      return(gg)
    }
    
  })
  
  # Plot 3: Medians plot ----
  output$plot3 <- renderPlot({
    
    # median by station and year
    
    data_medians_sel <- selected_data()$data_backgr_sel %>%
      ungroup() %>%
      group_by(Analysis, Station, YEAR) %>%
      summarize(Concentration = median(Concentration),
                Percent_below_LOQ = mean(!is.na(FLAG1))*100,
                Half_below_LOQ = (Percent_below_LOQ >= 50),
                Station_bg = first(Station_bg),
                .groups = "drop") %>%
      mutate(
        Background = case_when(
          !is.na(Station_bg) ~ "Background",
          is.na(Station_bg) ~ "Other"))
    
    #  plot 
    
    gg <- ggplot(data_medians_sel %>% filter(Background != "Other"), 
                 aes(YEAR, Concentration, group = Station)) +
      geom_line(
        data = data_medians_sel %>% filter(Background %in% "Other"), color = "grey70") +
      geom_line(aes(color = Station_bg)) +
      geom_point(aes(color = Station_bg, shape = Half_below_LOQ)) +
      scale_shape_manual(values = c("TRUE" = 6, "FALSE" = 16)) +
      geom_hline(data = selected_data()$data_proref_sel, aes(yintercept = PROREF), 
                 linetype = "dashed", colour = "blue") +
      facet_wrap(vars(Analysis)) +
      labs(subtitle = "Annual medians for each time series (coloured = background stations). Blue line = PROREF") +
      theme_bw() +
      theme(
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 12)
      )
    
    if (input$plot3_log){
      gg <- gg + scale_y_log10()
    }
    
    if (nrow(data_medians_sel) == 0) { 
      return(NULL)
    } else {
      return(gg)
    }
    
  })
  
  
  # Plot 4: raw data and proref plot ----  
  
  output$plot4 <- renderPlot({
    
    # function fior plot  
    
    create_proref_plot <- function(data, data_proref, log_y){
      gg1 <- ggplot(data, aes(YEAR, Concentration)) +
        geom_jitter(
          aes(color = Station_bg, size = Background, shape = LOQ), width = 0.2) +
        scale_size_manual(values = c("Other"=1, "Background"=2)) +
        scale_shape_manual(values = c("Under LOQ" = 6, "Over LOQ" = 16)) +
        geom_hline(
          data = data_proref, aes(yintercept = PROREF),
          colour = "blue", linetype = "dashed", linewidth = 1) +
        facet_wrap(vars(Analysis), nrow = 1) +
        labs(subtitle = "All values (coloured = background stations). Blue line = PROREF") +
        theme_bw() +
        theme(
          strip.text = element_text(size = 12), 
          legend.text =  element_text(size = 12)
        )
      
      # If we have max 8 stations, use brewer Set1 palette, otherwise we stick 
      #   with the default palette
      number_bg_stations <- table(data$Station_bg) %>% length()
      if (number_bg_stations <= 8){
        gg1 <- gg1 +
          scale_colour_brewer(palette = "Set1", na.value = "grey80")
      }
      
      # Add proref label
      proreflabel_x <- ggplot_build(gg1)$layout$panel_scales_x[[1]]$range$range[1]
      
      gg2 <- gg1 +
        geom_text(
          data = data_proref, aes(label = paste0("PROREF = ", PROREF),
                                  x = proreflabel_x, y = +Inf),
          colour = "blue", hjust = 0, vjust = 1.5)
      
      gg2
      
    }
    
    
    
    gg_all <- create_proref_plot(selected_data()$data_backgr_sel, 
                                 selected_data()$data_proref_sel,
                                 input$plot4_log)
    gg_bg <- create_proref_plot(selected_data()$data_backgr_sel %>% filter(Background %in% "Background"),
                                selected_data()$data_proref_sel,
                                input$plot4_log)
    
    # if 'restrict_y_axis' = TRUE, set max value of y axis to maximum value at background stations
    # min value of y axis should be 0 for a linear y axis, and just leave at default for log y axis
    if (input$restrict_y_axis){
      y_limits_linear <- c(0, selected_data()$max_bg)
      y_limits_log <- c(NA, selected_data()$max_bg)
    } else {
      y_limits_linear <- NULL
      y_limits_log <- NULL
    }
    
    if (input$show_all_data){
      # Show all data
      if (input$plot4_log){
        # log y axis
        gg_all + scale_y_log10(limits = y_limits_log)
      } else {
        # linear y axis
        gg_all + scale_y_continuous(limits = y_limits_linear)
      }
    } else {
      # Show data for background station data only
      if (input$plot4_log){
        # log y axis
        gg_bg + scale_y_log10(limits = y_limits_log)
      } else {
        # linear y axis
        gg_bg + scale_y_continuous(limits = y_limits_linear)
      }
    }
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
