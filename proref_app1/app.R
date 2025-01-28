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
data_all_comb <- bind_rows(
  bind_cols(data.frame(Analysis = datasets$analysis_name[1]), read_data(datasets$fn_rawdata[1]) %>% filter(YEAR %in% datasets$year1[1]:datasets$year2[1])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[2]), read_data(datasets$fn_rawdata[2]) %>% filter(YEAR %in% datasets$year1[2]:datasets$year2[2])),
  bind_cols(data.frame(Analysis = datasets$analysis_name[3]), read_data(datasets$fn_rawdata[3]) %>% filter(YEAR %in% datasets$year1[3]:datasets$year2[3]))
) %>%
  mutate(Analysis = fct_inorder(Analysis))

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
        tabPanel("Medians Plot", plotOutput("plot3")),
        tabPanel("Raw data Plot", plotOutput("plot4"),
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
  
  # reactive: selected_analysis ----
  # selected_analysis <- reactive({
  #   
  #   d1 <- which(datasets$analysis_name == input$analysis1)
  #   d2 <- which(datasets$analysis_name == input$analysis2)
  #   result_txt1 <- datasets$analysis_name[d1]
  #   result_txt2 <- datasets$analysis_name[d2]
  #   
  # 
  #   
  #   # if uncommented:
  #   # browser()
  #   # ...this shows that
  #   # lookup_background has 29375 rows
  #   # data_all2 has 2223050 rows
  #   # data_all_backgr has 3570728 rows
  #   
  #   
  #   #
  #   # Proref
  #   #
  #   data_proref <- data_all_backgr %>% 
  #     filter(Background %in% "Background") %>%
  #     group_by(Analysis, LATIN_NAME, PARAM) %>%
  #     summarize(PROREF = quantile(Concentration, 0.9) %>% signif(3))
  #   
  #   list(
  #     result_sel = result_sel,
  #     data_proref_sel = data_proref_sel,
  #     data_sel = data_sel,
  #     data_backgr_sel = data_backgr_sel
  #   )
  #   
  # })
  
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
    
    # browser()
    
    # req(selected_analysis())
    # result_sel <- selected_analysis()$result_sel
    # data_proref_sel <- selected_analysis()$data_proref_sel
    # data_sel <- selected_analysis()$data_sel
    # data_backgr_sel <- selected_analysis()$data_backgr_sel
    

    # Filter result_detailed based on partial parameter match
    # param_pattern <- input$param
    # matching_params <- unique(result_detailed$PARAM[grepl(param_pattern, result_detailed$PARAM, ignore.case = TRUE)])

    # # Update result_sel
    # result_sel <- result_detailed %>%
    #   filter(PARAM %in% input$param & LATIN_NAME == species)
    
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
  
  # Show matching parameters
  # output$param_info <- renderText({
  #   data <- selected_data()
  #   if (nrow(data$result_sel) == 0) {
  #     return("No matching parameters found")
  #   }
  #   paste("Matching parameter(s):", 
  #         paste(unique(data$result_sel$PARAM), collapse = ", "))
  # })
  
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
    req(selected_data())
    result_sel <- selected_data()$result_sel
    
    if (nrow(result_sel) == 0) return(NULL)
    
    ggplot(result_sel, aes(x = Min_year, xend = Max_year, y = Median2)) +
      geom_segment(aes(col = Background1b)) +
      expand_limits(y = 0) +
      facet_wrap(vars(Analysis)) +
      theme_bw() +
      theme(
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 12)
      )
  })
  
  # Plot 3: Medians plot ----
  output$plot3 <- renderPlot({
    
    req(selected_analysis())
    result_txt1 <- selected_analysis()$result_txt1
    result_txt2 <- selected_analysis()$result_txt2
    
    req(selected_data())
    data_sel <- selected_data()$data_sel
    result_sel <- selected_data()$result_sel
    
    # browser()
    
    if (nrow(result_sel) == 0) return(NULL)
    
    # Get background stations
    bg1 <- result_sel %>%
      filter(Analysis == result_txt1, Background1b == "Background") %>%
      pull(Station)
    bg2 <- result_sel %>%
      filter(Analysis == result_txt2, Background1b == "Background") %>%
      pull(Station)
    
    # Calculate medians
    data_medians_sel <- data_sel %>%
      group_by(STATION_CODE, YEAR) %>%
      summarize(Concentration = median(Concentration),
                Percent_below_LOQ = mean(!is.na(FLAG1))*100,
                Half_below_LOQ = (Percent_below_LOQ >= 50),
                .groups = "drop") %>%
      rename(Station = STATION_CODE) %>%
      mutate(
        Background = case_when(
          (Station %in% bg1) & !(Station %in% bg2) ~ "Background 1",
          !(Station %in% bg1) & (Station %in% bg2) ~ "Background 2",
          (Station %in% bg1) & (Station %in% bg2) ~ "Background 1+2",
          TRUE ~ "Other"),
        Background = factor(Background, 
                            levels = c("Background 1", "Background 2", 
                                       "Background 1+2", "Other"))
      ) %>%
      arrange(rev(Background))
    
    ggplot(data_medians_sel %>% filter(Background == "Other"), 
           aes(YEAR, Concentration, group = Station)) +
      geom_line(color = "grey70") +
      scale_y_log10() +
      geom_line(data = data_medians_sel %>% filter(Background != "Other"), 
                aes(color = Background)) +
      geom_point(data = data_medians_sel %>% filter(Background != "Other"), 
                 aes(color = Background, shape = Half_below_LOQ)) +
      scale_color_manual(
        values = c("Background 1" = "red", "Background 2" = "blue", 
                   "Background 1+2" = "purple", "Other" = "grey70")
      ) +
      scale_shape_manual(
        values = c("TRUE" = 6, "FALSE" = 16)
      ) +
      theme_bw() +
      theme(
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 12)
      )
  })
  
  # Plot 4: raw data and proref plot ----
  output$plot4 <- renderPlot({
    req(selected_data())
    data_all_backgr_sel <- selected_data()$data_all_backgr_sel
    data_proref_sel <- selected_data()$data_proref_sel
    max_bg <- selected_data()$max_bg
    # browser()
    gg_all <- create_proref_plot(data_all_backgr_sel, 
                               data_proref_sel)
    gg_bg <- create_proref_plot(data_all_backgr_sel %>% filter(Background %in% "Background"),
                              data_proref_sel)
    if (input$show_all_data){
      # Show all data
      if (input$restrict_y_axis){
        gg_all + ylim(0, max_bg)
      } else {
        gg_all
      }
    } else {
      # Show background station data
      if (input$restrict_y_axis){
        gg_bg + ylim(0, max_bg)
      } else {
        gg_bg
      }
    }
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
