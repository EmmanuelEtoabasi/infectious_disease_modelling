inputTabPanel <- function(id) {
  tabPanel(
    "sirv_v",
    numericInput(
      inputId = NS(id, "pop_new_demo"),
      label = "Population:",
      value = 500000,
      min = 0,
      max = NA,
      step = 1
    ),
    sliderInput(
      inputId = NS(id, "loop_p_vacc_sirv_v"),
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
}
