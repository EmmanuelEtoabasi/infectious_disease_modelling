here::i_am("global.R")

# load libraries
for (package in 
     c("shiny", "deSolve", "reshape2", "dplyr", "DT", "data.table", "shinyalert", 
       "ggplot2", "plotly", "tidyr")) { # "waiter"
  library(package, character.only = T)
}

# list of models
list_of_models <- list(
  "Please choose a model" = "",
  "SIRV-c" = "sirv_c",
  "SIRV-v" = "sirv_v"
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


notify <- function(msg, id=NULL){
  showNotification(msg, id=id, duration=NULL, closeButton=FALSE)
}

model_notify <- function(){
    id <- notify("Generating Model...")
    on.exit(removeNotification(id), add=TRUE)
    Sys.sleep(1)
    
    notify("Solving differential equations...", id=id)
    Sys.sleep(1)
    
    notify("Computing model output...", id=id)
    Sys.sleep(1)
}

