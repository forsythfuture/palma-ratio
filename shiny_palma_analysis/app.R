library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(kableExtra)

##################### Section to edit ##############################

file_name <- 'palmas_comparison.csv'

indicator_name <- 'Palma'

# Enter data sources. Each line below represents a single line in the app
data_source <- c('US Census Public Use Micro Sample')

# Enter interpretation of data. Each line below represents a single line in the app
# HTML tags can be used to format text:
# Examples:
#   italics: <i>text</i>
#   bold: <b>text</b>
#   line break: <br>

interpretation <- c("Over the past two years, Forsyth County's rate has been higher than Guilford's or Durham's.",
                    "But, the differences are not statistically significant.<br>",
                    "Forsyth County's Palma ratio rose from 2014 to 2016, 
                    and the 2014 and 2016 difference is statistically significant.",
                    "The 2014 and 2017 difference is not statistically significant.")

################### End section to edit ############################

# load all custom functions
source('global.R')

################# try ################

# # load all datasets into a list
# filenames <- list.files(pattern='.csv')
# # create list of filenames without .csv for naming items in list
# list_names <- str_replace_all(filenames, '.csv', '')

#########################################

# load file
df <- read_csv(file_name) %>%
  select(geo_description, year, estimate, moe, se, cv, type, subtype)
#
# # crate tableau dataset
tableau_df <- df %>%
  select(geo_description, year, estimate) %>%
  rename(`Geographic Area` = geo_description, Year = year, `Palma Ratio` = estimate)

# unique demographics, for drop down menu
#unique_demo <- unique(df$type)

# unique years, for z-score checkbox
unique_year <- unique(df$year)

# unique geography, for z-score checkbox
unique_geo <- unique(df$geo_description)


ui <- dashboardPage(
  
  dashboardHeader(title = indicator_name),
  dashboardSidebar(
    
    # table sources
    htmlOutput("source"),

    # download tableau data buttom
    downloadButton("download_tableau", "Download Tableau")
    
  ),
  
  dashboardBody(
    fluidRow(
      tabsetPanel(
        tabPanel("Plots", 
                 plotlyOutput("plot_line"),
                 tags$hr(),
                 plotlyOutput("plot_bar"),
                 # interpretations
                 htmlOutput("interpretations")
                 ),
        tabPanel("Z Scores",
                 checkboxGroupInput('year_check', 'Years:', unique_year, selected = max(unique_year), inline = TRUE),
                 checkboxGroupInput('geo_check', 'Geography:', unique_geo, selected = 'Forsyth', inline = TRUE),
                 uiOutput("ui_demo_check"),
                 tableOutput('table_zscore')
                 ),
        tabPanel("Raw Data",
                 dataTableOutput('table_raw_data'))
      )
    )
  )
)

server <- function(input, output, session) { 
  
  # create dataset based on which demographic is selected
  # used for plots
  # df_demo <- eventReactive(input$demographic, {
  #   df %>%
  #     filter(type == input$demographic)
  # })
  
  # output data sources
  output$source <- renderUI({
    source_title <- '<br><b>Data Sources</b><br>'
    sources <- paste(data_source, collapse='<br>')
    HTML(paste0(source_title, sources))
  })

  output$download_tableau <- downloadHandler(
    filename = function() {
      paste0('tableau_', file_name)
    },
    content = function(con) {
      write.csv(tableau_df, con, row.names = FALSE)
    },
    contentType = 'text/csv'
  )
  
  output$plot_line <- renderPlotly({
    
    plotly_plots(df, 'line')
    
  })
  
  output$plot_bar <- renderPlotly({
    
    plotly_plots(df, 'bar')
    
  })
  
  # # output interpretations
  output$interpretations <- renderUI({

    # '&nbsp;' dds additional spaces to indent line
    interp_title <- '<br><b>Interpretation</b><br><br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
    interps <- paste(interpretation, collapse='<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;')
    HTML(paste0(interp_title, interps))
  })
  
  output$table_zscore <-  function() {
    
    df %>%
      filter(year %in% input$year_check,
             geo_description %in% input$geo_check) %>%
      ff_acs_zscore_kable('estimate', 'se', c('year', 'geo_description', 'subtype' ))
    
  }
  
  output$table_raw_data <- DT::renderDataTable({
    
    df %>%
      ff_data_dt(col_names = c('Geography', 'Year', 'Estimate', '95% MOE', 
                               'St. Error', 'CV', 'Type', 'Subtype'))
  })
  
}
shinyApp(ui, server)