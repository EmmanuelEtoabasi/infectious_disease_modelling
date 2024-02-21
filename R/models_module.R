model_UI <- function(id = "tab_model_params") {
  tagList(tabsetPanel(
    id = NS(id, "tabsetpanel_id"),
    type = "hidden",
    tabPanel(""), # : to prevent default exposure of the first tab
    specific_model_env("sirv_c")$inputTabPanel,
    specific_model_env("sirv_v")$inputTabPanel
    # other model specific tabPanels
  ))
}

model_Server <- function(id = "tab_model_params", selected_model, selected_timeframe) {
  moduleServer(id, function(input, output, session) {
    # Create model path environment: This will prevent global namespace overloading
    model_path_env <- reactive(
      {
        # input$tabsetpanel_id is the currently selected or active model from the list of models
        # req(input$tabsetpanel_id)
        specific_model_env(selected_model())
      },
      label = "model_path_env"
    )

    # Get looping sliderInput IDs. It is a reactive object
    # looping_sliders <- get_specific_inputs("loop")
    play_loops <- reactive({
      looping_inputs <- list()
      all_inputs_names <- names(input)
      looping_sliders <- all_inputs_names[grep("^loop", all_inputs_names)]
      if (length(looping_sliders) > 0) {
        for (slider in looping_sliders()) {
          looping_inputs[[slider]] <- input[[slider]]
        }
      }
    })
    
    # When model choice or timeframe choice changes:
    observeEvent(c(selected_model(), selected_timeframe()), { # input$model_id, input$timeframe_id
      # req(selected_model(), selected_timeframe())
      
      # (1.) Reveal the corresponding hidden tabs
      updateTabsetPanel(session, inputId = "tabsetpanel_id", selected = selected_model())
      # ************************** move out into its module
      #updateTabsetPanel(session, inputId = "time_params", selected = selected_timeframe())
      
      # (2.) Pause all looping sliders
      session$sendCustomMessage(type = "pauseSliders", message = as.list(play_loops()))
    }, label="obsE_timeframe_model")
    
    # ...

    list(
      model_env = model_path_env,
      sliders_with_loop = play_loops
      # selected_model = reactive(input$tabsetpanel_id)
    )
  })
}
