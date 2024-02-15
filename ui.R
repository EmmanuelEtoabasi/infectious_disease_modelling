source("global.R")

ui <- fluidPage(
  
  pause_sliders(),

  # Application title
  titlePanel("Infectious Disease Modelling"),
  sidebarLayout(
    # SIDEBAR
    sidebarPanel(
      selectInput("model_id",
        label = h3("Select Model:"),
        choices = list_of_models,
        # selected = " ",
        multiple = FALSE
      ),
      selectInput("timeframe_id",
                  label = h4("Time unit:"),
                  choices = c("Days" = "days", "Years" = "years"),
                  # selected = "days",
                  multiple = FALSE
      ),

      # ADD MODEL INPUT
      simlutation_timeframe_tabs,
      model_parameters_tabs,
      
      # simlutation_timeframe_tabs,
      # uiOutput("time_additional_input"),
      verbatimTextOutput("days"),
      actionButton("simulate_id", "Simulate!", icon("refresh")),
      # submitButton("Generate Model", icon("refresh")),
      helpText(
        "Click the button to simulate at fixed p_vacc",
        "Or click play above to animate plots at looping p_vacc"
      )
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
