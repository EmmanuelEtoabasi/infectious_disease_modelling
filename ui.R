source("global.R")

ui <- fluidPage(
  # use_waiter(),
  
  # Application title
  titlePanel("Infectious Disease Modelling"),
  sidebarLayout(
    # SIDEBAR
    sidebarPanel(
      selectInput("model_id",
        label = h3("Select Model:"),
        choices = list_of_models, # "Choice 2" = 2, "Choice 3" = 3),
        # selected = "sirv_c",
        selected = " ",
        multiple = FALSE
      ),
      uiOutput("model_additional_input"),
      selectInput("time_unit_id",
        label = h4("Time unit:"),
        choices = c("Days" = "days", "Years" = "years"),
        selected = "days",
        multiple = FALSE
      ),
      uiOutput("time_additional_input"),
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
