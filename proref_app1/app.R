library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggrepel)
library(glue)

#
# data ----
#
data_all2 <- readRDS("../Data/54_data_2024.rds")
df_series_sel <- readRDS("../Data/54_dataseries_2024.rds")

result_detailed1 <- readRDS("../Data/54_result_detailed_2024-11-29.rds")
result_txt1 <- "Original (1992-2022)"
result_detailed2 <- readRDS("../Data/54_result_detailed_2003-2022_2025-01-05-T1812.rds")
result_txt2 <- "2003-2022"

result_detailed <- bind_rows(
  bind_cols(data.frame(Analysis = result_txt1), result_detailed1),
  bind_cols(data.frame(Analysis = result_txt2), result_detailed2)
) %>%
  mutate(Analysis = fct_inorder(Analysis))



# UI
ui <- fluidPage(
  titlePanel("Parameter and Species Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # Parameter selection with text input for partial matching
      # textInput("param", "Parameter (partial matching)", value = "CU__WW"),
      selectizeInput("param", "Parameter", 
                     choices = unique(result_detailed$PARAM), 
                     selected = "CD__WW", multiple = FALSE,
                     options = NULL),
      
      # Species selection
      selectInput("species", "Species",
                  choices = c("mussel", "cod"),
                  selected = "mussel"),
      
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
        tabPanel("Medians Plot", plotOutput("plot3"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values for filtered data
  selected_data <- reactive({
    # Convert species common name to Latin name
    species <- if (input$species == "mussel") {
      "Mytilus edulis"
    } else {
      "Gadus morhua"
    }
    
    # Filter result_detailed based on partial parameter match
    # param_pattern <- input$param
    # matching_params <- unique(result_detailed$PARAM[grepl(param_pattern, result_detailed$PARAM, ignore.case = TRUE)])
    
    
    # Update result_sel
    result_sel <- result_detailed %>%
      filter(PARAM %in% input$param & LATIN_NAME == species)
    
    # For plotting line at 2x the cleanest station
    line_2x_cleanest <- result_sel %>%
      group_by(Analysis) %>%
      summarize(Median2 = 2*first(Median2))
    
    # Update data_sel
    data_sel <- data_all2 %>%
      filter(PARAM %in% input$param & LATIN_NAME == species)
    
    list(result_sel = result_sel, 
         line_2x_cleanest = line_2x_cleanest,
         data_sel = data_sel)
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
  
  # Plot 1: Algorithm plot
  output$plot1 <- renderPlot({
    req(selected_data())
    result_sel <- selected_data()$result_sel %>%
      filter(Rank <= input$max_rank)
    line_2x_cleanest <- selected_data()$line_2x_cleanest
    
    if (nrow(result_sel) == 0) return(NULL)
    
    ggplot(result_sel, aes(Rank, Median2)) +
      geom_line() +
      geom_point(aes(col = Background1b)) +
      geom_text_repel(aes(label = glue("{Station} (P = {P_round})")), 
                      hjust = 0, nudge_x = 0.5, 
                      point.padding = 0.2, box.padding = 0.25, 
                      direction = "y") +
      geom_hline(data = line_2x_cleanest, aes(yintercept = Median2), 
                 linetype = "dashed", colour = "brown") +
      expand_limits(y = 0) +
      facet_wrap(vars(Analysis))
  })
  
  # Plot 2: Time range plot
  output$plot2 <- renderPlot({
    req(selected_data())
    result_sel <- selected_data()$result_sel
    
    if (nrow(result_sel) == 0) return(NULL)
    
    ggplot(result_sel, aes(x = Min_year, xend = Max_year, y = Median2)) +
      geom_segment(aes(col = Background1b)) +
      expand_limits(y = 0) +
      facet_wrap(vars(Analysis))
  })
  
  # Plot 3: Medians plot
  output$plot3 <- renderPlot({
    req(selected_data())
    result_sel <- selected_data()$result_sel
    data_sel <- selected_data()$data_sel
    
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
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
