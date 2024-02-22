source("global.R")

# ===============================
# SERVER
# ===============================
server <- function(input, output, session) {
  
  model <- model_Server("tab_model_params")

  # Create model path environment: 
  # This will prevent global namespace overloading
  play_loops <- model$play_loops
  model_id_react <- model$model_id_react
  model_path_env <- model$model_path_env
  pauseLoop_params <- model$pauseLoop_params

  # Get looping sliderInput IDs. It is a reactive object
  # looping_sliders <- get_specific_inputs("loop")
  # looping_sliders <- model$sliders_with_loop()

  
  current_state <- reactive({
    list(
      input$simulate_id,
      model$sliders_with_loop
      )
  },label = "current_state")


  # # When model choice or timeframe choice changes:
  observeEvent(pauseLoop_params(), {
    # Pause all looping sliders
    session$sendCustomMessage(type = "pauseSliders", message = as.list(play_loops()))
  }, label="obsE_timeframe_model")

  simulate_Server("simulate", model_id_react)
  
  




  # COMPUTE MODEL & SAVE OUTPUT IN DATAFRAMES:====
  # Consume the defined reactive dependencies
  model_output_dfs <- eventReactive(current_state(), {
    model_path_env()$modelOutput(input)
  }, label="model_output_dfs")



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
