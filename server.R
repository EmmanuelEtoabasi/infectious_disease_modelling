source("global.R")


# SERVER============
server <- function(input, output, session) {
  model_notify()
  model_path <- reactive({
    req(input$model_id)
    # Waiter$new(id="modelPlot1", html=spin_ripple())$show()
    model_notify() # generate notifications for user 
    path_to_specific_model(input$model_id)
  })

  # Create model path environment: This will prevent global namespace overloading
  model_path_env <- reactive({
    env <- new.env()
    sys.source(model_path(), envir = env)
    env
  })

  # Define sources of reactive dependencies: input$simulate, and input$*_loop if exist
  current_state <- reactive({
    input_IDs <- names(input)
    looping_param <- input_IDs[grep("_loop$", input_IDs)]
    ifelse(length(looping_param),
      list(input$simulate_id, input$model_id, input[[looping_param]]),
      list(input$simulate_id, input$model_id)
    )
  })

  # PRINT SELECTED MODEL
  output$modelPath_id <- renderPrint({
    model_path()
  })


  # DYNAMICALLY ADD INPUT WIDGETS TO UI BASED ON MODEL SELECTED
  output$model_additional_input <- renderUI({
    req(input$model_id)
    model_params <- model_path_env()$parameters
    inputs <- list()
    for (param in names(model_params)) {
      inputs[[param]] <- model_params[[param]] # call each input
    }
    do.call(tagList, inputs) # render all the input
  })


  # DYNAMICALLY ADD INPUT WIDGETS INTO UI BASED ON TIME UNIT
  output$time_additional_input <- renderUI({
    if (input$time_unit_id == "days") {
      numericInput("days_id", "Select number of days:",
        min = 0, max = 1095, value = 100
      )
    } else {
      numericInput("years_id", "Select number of years:",
        min = 0, max = 30, value = 10
      )
    }
  })

  # output$days <- renderPrint({
  #   input$days_id[[1]]
  # })

  # COMPUTE MODEL & SAVE OUTPUT IN DATAFRAMES: Consume the defined reactive dependencies
  model_output_dfs <- eventReactive(current_state(), {
    model_path_env()$modelOutput(input)
  })

  # Remind users to select a model
  observeEvent(input$simulate_id, {
    if (!isTruthy(input$model_id)) {
      shinyalert("Select a Model", "Hi!👋 Please select a model from the list", type = "info")
    }
  })

  # RENDER COMBINED PLOTS
  output$modelPlot1 <- renderPlotly({
    all_plot(model_output_dfs()$df_long)
  })

  # RENDER INFECTION PREVALENCE
  output$modelPlot2 <- renderPlotly({
    infection(model_output_dfs()$df)
  })

  # RENDER VACCINATION PREVALENCE
  output$modelPlot3 <- renderPlotly({
    model_path_env()$vaccination(model_output_dfs()$df)
  })
}
