here::i_am("global.R")

# load libraries
for (package in
  c(
    "shiny", "deSolve", "reshape2", "dplyr", "DT", "data.table", "shinyalert",
    "ggplot2", "plotly", "tidyr"
)) { # "waiter"
  library(package, character.only = T)
}

# list of models
list_of_models <- c(
  "Please choose a model" = "",
  "SIRV-v" = "sirv_v",
  "SIRV-c" = "sirv_c"
)

# Define function path_to_specific_model()
# To run in the server file
# It will take ui inputs to determine path to the specific model

path_to_specific_model <- function(selected_model) {
  model_path <- here::here("R/models", paste(selected_model, ".R", sep = ""))

  return(model_path)
}


all_plot <- function(output_long) {
  plot_ly(output_long,
    x = ~time, y = ~proportion, color = ~variable, type = "scatter", mode = "lines"
  ) %>%
    layout(
      xaxis = list(title = "Time", title_standoff = 30),
      yaxis = list(title = "Proportion of population", range = c(0, 1)),
      legend = list(
        title = list(text = "Compartment"),
        orientation = "h", xanchor = "center", x = 0.5, y = -0.2
      )
    )
}

infection <- function(output) {
  p <- ggplot(output, aes(x = time, y = I)) +
    geom_line(color = "darkgreen") +
    # xlab("Time duration") +
    ylab("Infected") +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  return(p)
}


notify <- function(msg, id = NULL) {
  showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
}

message <- c("Generating Model...", "Solving differential equations...", "Computing model output...")

model_notify <- function(message) {
  id <- notify(message)
  on.exit(removeNotification(id), add = TRUE)
  Sys.sleep(1)
}

simlutation_timeframe_tabs <- tabsetPanel(
  id = "time_params",
  type = "hidden",
  tabPanel(""), # to ensure no default exposure of the first tab
  tabPanel(
    "days",
    numericInput(
      inputId = "input_days", label = "Select number of days:",
      min = 0, max = 1095, value = 100
    )
  ),
  tabPanel(
    "years",
    numericInput(
      inputId = "input_years", label = "Select number of years:",
      min = 0, max = 30, value = 10
    )
  )
)

model_parameters_tabs <- tabsetPanel(
  id = "tab_model_params",
  type = "hidden",
  tabPanel(""), # : to prevent default exposure of the first tab
  tabPanel(
    "sirv_c",
    numericInput(
      inputId = "pop_demo", label = "Population:",
      value = 500000, min = 0, max = NA, step = 1
    ),
    sliderInput(
      inputId = "loop_p_vacc_demo",
      label = "Proportion Vaccinated:",
      value = 0.0,
      min = 0,
      max = 1,
      step = 0.05,
      animate = animationOptions(
        interval = 1500,
        loop = TRUE
      )
    ),
    hr(),
    h4("Infection rate (β)"),
    numericInput(
      inputId = "model_beta",
      label = "per day",
      value = 2.5,
      min = 0,
      max = NA,
      step = 0.01
    ),
    hr(),
    h4(" Recovery rate (γ))"),
    numericInput(
      inputId = "model_gamma",
      label = "per day",
      value = 0.8,
      min = 0,
      max = NA,
      step = 0.01
    )
  ),
  tabPanel(
    "sirv_v",
    numericInput(
      inputId = "pop_new_demo",
      label = "Population:",
      value = 500000,
      min = 0,
      max = NA,
      step = 1
    ),
    sliderInput(
      inputId = "loop_p_vacc_new_demo",
      label = "Proportion Vaccinated**:",
      value = 0.0,
      min = 0,
      max = 1,
      step = 0.05,
      animate = animationOptions(
        interval = 1500,
        loop = TRUE
      )
    )
  )
)




pause_sliders <- function() {
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('pauseSliders', function(looping_sliders) {
      looping_sliders.forEach(function(sliderId) {
        var $animateButton = $('#' + sliderId + ' + .slider-animate-container .slider-animate-button');
        if ($animateButton.hasClass('playing')) {
          $animateButton.click();
        }
      });
    });
  ")))
}


# pause_sliders <- function(){
#   tags$head(tags$script(HTML("
#       Shiny.addCustomMessageHandler('pauseSlider', function(message) {
#       var isPlaying = $('.slider-animate-button').hasClass('playing');
#       if (isPlaying) {
#         $('.slider-animate-button').click();
#       }
#     })
#   ")))
# }

# get_specific_inputs <- function(input_string, input) {
#   reactive({
#     all_inputs_names <- names(input)
#     specific_inputs <- all_inputs_names[grep(paste0("^", input_string), all_inputs_names)]
#     specific_inputs
#   })
# }



# convert_model_param_ids <- function(param_ids, timeframe) {
#   # timeframe <- input$timeframe_id
#   
#   for (param_id in param_ids) {
#     if (timeframe == "days") {
#       # convert yearly value ---> daily value
#       new_value <- input[[param_id]] / 365
#     } else if (timeframe == "years") {
#       # convert daily value ---> yearly value
#       new_value <- input[[param_id]] * 365
#     } 
#     updateNumericInput(inputId = param_id, value = new_value)
#   }
# }