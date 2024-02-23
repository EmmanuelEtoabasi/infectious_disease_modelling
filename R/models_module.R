model_select_UI <- function(id) {
  tagList(
    selectInput(NS(id, "model_id"),
      label = h3("Select Model:"),
      choices = list_of_models,
      # selected = " ",
      multiple = FALSE
    ),
    selectInput(NS(id, "timeframe_id"),
      label = h4("Time unit:"),
      choices = c("Days" = "days", "Years" = "years"),
      # selected = "days",
      multiple = FALSE
    )
  )
}

model_input_UI <- function(id) {
  tabsetPanel(
    id = NS(id, "tabsetpanel_id"),
    type = "hidden",
    tabPanel(""), # : to prevent default exposure of the first tab
    specific_model_env("sirv_c")$inputTabPanel(id), # a tabPanel
    specific_model_env("sirv_v")$inputTabPanel(id) # a tabPanel
    # .... other model specific tabPanels ....
  )
}

model_duration_input_UI <- function(id){
  tabsetPanel(
    id = NS(id, "duration_id"),
    type = "hidden",
    tabPanel(""), # to ensure no default exposure of the first tab
    tabPanel(
      "days",
      numericInput(
        inputId = NS(id, "input_days"), label = "Select number of days:",
        min = 0, max = 1095, value = 100
      )
    ),
    tabPanel(
      "years",
      numericInput(
        inputId = NS(id, "input_years"), label = "Select number of years:",
        min = 0, max = 30, value = 10
      )
    )
  )
}

simulate_UI <- function(id) {
  tagList(
    actionButton(NS(id, "simulate_id"), "Simulate!", icon("refresh")),
    # submitButton("Generate Model", icon("refresh")),
    helpText(
      "Click the button to simulate at fixed p_vacc",
      "Or click play above to animate plots at looping p_vacc"
    )
  )
}

simulate_Server <- function(id, model_id_react) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$simulate_id,
      { # Remind users to select a model
        if (!isTruthy(model_id_react())) {
          shinyalert("Select a Model", "Hi! Please select a model🔢 from the list", type = "info")
        }
        req(model_id_react(), cancelOutput = TRUE)
        input$simulate_id
      },
      label = "obsE_simulateiId_alert"
    )
  })
}

model_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
   
    # Create model path environment:
    # This will prevent global namespace overloading
    model_env <- reactive(
      {
        req(input$model_id)
        specific_model_env(input$model_id)
      },
      label = "model_env"
    )

    # Get looping sliderInput IDs. It is a reactive object
    all_play_loops <- reactive(get_all_play_loops(id, input))
    looping_slider_ids <- reactive(all_play_loops()$looping_slider_ids)
    looping_slider_values <- reactive(all_play_loops()$looping_slider_values)
    model_timeframe_ids <- reactive(c(input$model_id, input$timeframe_id))

    # When model or timeframe choice changes:
    observeEvent(c(input$model_id, input$timeframe_id),
      {
        req(input$model_id)
        # Reveal the corresponding hidden tabs
        updateTabsetPanel(session, inputId = "tabsetpanel_id", selected = input$model_id)
        updateTabsetPanel(session, inputId = "duration_id", selected = input$timeframe_id)
      },
      label = "obs_timeframe_model"
    )


    # CONVERT MODEL PARAM VALUES to EQUIVALENT in DAYS or YEARS ====
    # 1.) Get all model param values
    model_specific_param_ids <- reactive(
      {
        all_inputs_id <- names(input)
        grep("^model(?!.*_id$)", all_inputs_id, value = TRUE, perl = TRUE)
      },
      label = "model_specific_param_ids"
    )

    # 2.) get a react value to monitor change in timeframe
    time_frame_changed <- reactiveVal(FALSE)

    # 3.) observe timeframe change then convert model param values accordingly
    observeEvent(input$timeframe_id,
      {
        if (time_frame_changed()) {
          for (param_id in model_specific_param_ids()) {
            if (input$timeframe_id == "days") {
              # convert yearly value --> daily value
              new_value <- input[[param_id]] / 365
              updateNumericInput(session, inputId = param_id, value = new_value, label = "per day")
            } else if (input$timeframe_id == "years") {
              # convert daily value --> yearly value
              new_value <- input[[param_id]] * 365
              updateNumericInput(session, inputId = param_id, value = new_value, label = "per year")
            }
          }
        }
        time_frame_changed(TRUE)
      },
      label = "obsE_timeframe"
    )
    # ....
    list(
      inputs = input,
      model_path_env = model_env,
      play_loops_ids = looping_slider_ids,
      play_loops_values = looping_slider_values,
      pauseLoop_params = model_timeframe_ids,
      model_id_react = reactive(input$model_id)
    )
  })
}
