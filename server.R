source("global.R")

# ===============================
# SERVER
# ===============================
server <- function(input, output, session) {
  
  model <- model_Server("tab_model_params")

  play_loops_ids <- model$play_loops_ids
  model_id_react <- model$model_id_react
  model_path_env <- model$model_path_env # Create model path environment: This will prevent global namespace overloading
  pauseLoop_params <- model$pauseLoop_params
  play_loops_values <- model$play_loops_values


  # # When model choice or timeframe choice changes: Pause all looping sliders
  observeEvent(pauseLoop_params(), {
    session$sendCustomMessage(type = "pauseSliders", message = as.list(play_loops_ids()))
  }, label="obsE_timeframe_model")

  # Simulate action button server
  simulate <- simulate_Server("simulate", model_id_react)
  
  # current_state <- reactive({
  #   list(
  #     isSimulate_model,
  #     play_loops_values
  #   )
  # },label = "current_state")

  # COMPUTE MODEL & SAVE OUTPUT IN DATAFRAMES:====
  model_output_dfs <- eventReactive(c(simulate(), play_loops_values()),
    {
      model_path_env()$modelOutput(model$inputs)
    },
    label = "model_output_dfs"
  )

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
