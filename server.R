source("global.R")

# ===============================
# SERVER
# ===============================
server <- function(input, output, session) {
  model_path <- reactive({
    # input$tab_model_params is the currently selected or active model from the list of models
    req(input$tab_model_params)
    path_to_specific_model(input$tab_model_params)
  }, label = "model_path")

  # Create model path environment: This will prevent global namespace overloading
  model_path_env <- reactive({
    env <- new.env()
    sys.source(model_path(), envir = env)
    env
  }, label = "model_path_env")

  # Get looping sliderInput IDs. It is a reactive object
  # looping_sliders <- get_specific_inputs("loop")
  looping_sliders <- reactive({
    all_inputs_names <- names(input)
    all_inputs_names[grep("^loop", all_inputs_names)]
  }, label = "looping_sliders")

  
  current_state <- reactive({
    if (length(looping_sliders()) > 0) {
      looping_inputs <- list()
      for (slider in looping_sliders()) {
        print(slider)
        looping_inputs[[slider]] <- input[[slider]]
        print("this is OK!!!")
      }
      c(list(input$simulate_id, input$tab_model_params), looping_inputs)
    } else {
      list(input$simulate_id, input$tab_model_params)
    }
    print("INSIDE CURRENT STATE2")
    print(length(looping_sliders()))
    print(typeof(c(looping_sliders())))
  }, label = "current_state")

  time_frame_changed <- reactiveVal(FALSE)

  observeEvent(input$timeframe_id, {
    # Convert model parameter values to corresponding timeframe value
    if (time_frame_changed()) {
    
      for (param_id in model_specific_param_ids()) {
        # freezeReactiveValue(input, param_id)
        if (input$timeframe_id == "days") {
          # convert yearly value ---> daily value
          # print("==================")
          # print(input[[param_id]])
          new_value <- input[[param_id]] / 365
          updateNumericInput(inputId = param_id, value = new_value, label="per day")
        } else if (input$timeframe_id == "years") {
          # convert daily value ---> yearly value
          new_value <- input[[param_id]] * 365
          updateNumericInput(inputId = param_id, value = new_value, label="per year")
        }
      }
      
    }
    time_frame_changed(TRUE)
  }, label = "obsE_timeframe")


  # When model choice or timeframe choice changes:
  observeEvent(c(input$model_id, input$timeframe_id), {
    req(input$model_id, input$timeframe_id)

    # (1.) Reveal the corresponding hidden tabs
    updateTabsetPanel(inputId = "tab_model_params", selected = input$model_id)
    updateTabsetPanel(inputId = "time_params", selected = input$timeframe_id)

    # (2.) Pause all looping sliders
    session$sendCustomMessage(type = "pauseSliders", message = as.list(looping_sliders()))
  }, label="obsE_timeframe_model")

  # model_specific_param_ids <- get_specific_inputs("model", input)
  model_specific_param_ids <- reactive({
    all_inputs_names <- names(input)
    grep("^model(?!.*_id$)", all_inputs_names, value = TRUE, perl = TRUE)
  }, label="model_specific_param_ids")


  observe({
    print(paste("Currently selected tabPanel within time_params:", input$time_params))
    print(paste("Currently selected tabPanel within tab_model_params:", input$tab_model_params))
  }, label="obs_print")


  # PRINT SELECTED MODEL
  output$modelPath_id <- renderPrint({
    model_path()
  })

  # COMPUTE MODEL & SAVE OUTPUT IN DATAFRAMES:====
  # Consume the defined reactive dependencies
  model_output_dfs <- eventReactive(current_state(), {
    model_path_env()$modelOutput(input)
  }, label="model_output_dfs")

  # Remind users to select a model====
  observeEvent(input$simulate_id, {
    if (!isTruthy(input$model_id)) {
      shinyalert("Select a Model", "Hi! Please select a model🔢 from the list", type = "info")
      return(NULL)
    }
    if (!isTruthy(input$timeframe_id)) {
      shinyalert("Select a Timeframe", "Hi! Please select a Timeframe⏳ from the list", type = "info")
    }
  }, label="obsE_simulateiId_alert")

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
