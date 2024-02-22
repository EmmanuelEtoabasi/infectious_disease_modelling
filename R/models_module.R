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
    # Remind users to select a model====
    observeEvent(input$simulate_id,
      {
        if (!isTruthy(model_id_react())) {
          shinyalert("Select a Model", "Hi! Please select a model🔢 from the list", type = "info")
          return(NULL)
        }
        # if (!isTruthy(timeframe_id_react)) {
        #   shinyalert("Select a Timeframe", "Hi! Please select a Timeframe⏳ from the list", type = "info")
        # }
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
    all_play_loops <- reactive(get_all_play_loops(id, input)$names)
    model_timeframe_ids <- reactive(c(input$model_id, input$timeframe_id))

    # When model choice or timeframe choice changes:
    observeEvent(model_timeframe_ids(),
      {
        # (1.) Reveal the corresponding hidden tabs
        updateTabsetPanel(session,
          inputId = "tabsetpanel_id",
          selected = input$model_id
        )

        # ************************** move out into its module
        # updateTabsetPanel(session, inputId = "time_params", selected = selected_timeframe())
      },
      label = "obsE_timeframe_model"
    )


    # CONVERT MODEL PARAM VALUES to EQUIVALENT in DAYS or YEARS ====
    # 1.) Get all model param values
    model_specific_param_ids <- reactive(
      {
        all_inputs_names <- names(input)
        grep("^model(?!.*_id$)", all_inputs_names, value = TRUE, perl = TRUE)
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
      model_path_env = model_env,
      play_loops = all_play_loops,
      pauseLoop_params = model_timeframe_ids,
      model_id_react = reactive(input$model_id)
      # selected_model = reactive(input$tabsetpanel_id)
    )
  })
}
