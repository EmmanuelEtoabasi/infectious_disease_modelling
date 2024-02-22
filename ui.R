source("global.R")

ui <- fluidPage(
  
  pause_sliders(),
  # model_head_UI("tab_model_params"),

  # Application title
  titlePanel("Infectious Disease Modelling"),
  sidebarLayout(
    # SIDEBAR
    sidebarPanel(
      model_select_UI("tab_model_params"),
      # ADD MODEL INPUT
      simlutation_timeframe_tabs,
      model_input_UI("tab_model_params"),
      # model_parameters_tabs,
      
      # simlutation_timeframe_tabs,
      # uiOutput("time_additional_input"),
      verbatimTextOutput("days"),
      
      simulate_UI("simulate")
    ),
    # MAINPANEL
    mainPanel(
      fluidRow(column(10, verbatimTextOutput("modelPath_id"))),
      hr(),
      fluidRow(
        column(7, plotlyOutput("modelPlot1", height = "300px")),
        column(
          5, plotlyOutput("modelPlot2", height = "150px"),
          plotlyOutput("modelPlot3", height = "150px")
        )
      )
    )
  )
)
