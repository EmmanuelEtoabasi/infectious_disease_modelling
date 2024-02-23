source("global.R")

ui <- fluidPage(
  
  pause_sliders(),
  titlePanel("Infectious Disease Modelling"),
  sidebarLayout(
    # SIDEBAR
    sidebarPanel(
      model_select_UI("tab_model_params"),
      model_duration_input_UI("tab_model_params"),
      model_input_UI("tab_model_params"),
      simulate_UI("simulate")
    ),
    # MAINPANEL
    mainPanel(
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
