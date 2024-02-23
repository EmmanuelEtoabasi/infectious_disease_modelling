# ========================================================================== #
#                         PLOTTING THE VACCINE OUTPUT
# ========================================================================== #

vaccination <- function(output) {
  p <- ggplot(output, aes(x = time, y = V)) +
    geom_line(color = "purple") +
    xlab("Time") +
    ylab("Vaccinated") +
    theme_minimal()
  p <- ggplotly(p) %>% layout(xaxis = list(title_standoff = 30))
  p
}
# ========================================================================== #
#                      DEFINE PARAMETERS
# ========================================================================== #
inputTabPanel <- function(id) {
  tabPanel(
    "sirv_c",
    numericInput(
      inputId = NS(id, "pop_demo_sirv_c"), label = "Population:",
      value = 500000, min = 0, max = NA, step = 1
    ),
    sliderInput(
      inputId = NS(id, "loop_p_vacc_sirv_c"),
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
      inputId = NS(id, "model_beta_sirv_c"),
      label = "per day",
      value = 2.5,
      min = 0,
      max = NA,
      step = 0.01
    ),
    hr(),
    h4(" Recovery rate (γ))"),
    numericInput(
      inputId = NS(id, "model_gamma_sirv_c"),
      label = "per day",
      value = 0.8,
      min = 0,
      max = NA,
      step = 0.01
    )
  )
}
# ========================================================================== #
#                      DEFINE MODEL OUTPUT FUNCTION
# ========================================================================== #
modelOutput <- function(uiInputParameters) {
  # model_notify("Checking inputs...")
  req(
    uiInputParameters$pop_demo_sirv_c,
    uiInputParameters$loop_p_vacc_sirv_c,
    uiInputParameters$model_beta_sirv_c,
    uiInputParameters$model_gamma_sirv_c,
    uiInputParameters$timeframe_id # the currently selected timeframe
  )

  # model_notify("Define model...")
  # THE MODEL
  model_SIRVc <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), { # tell R to unpack variable names from the state and parameters inputs

      # Calculating the total population size N (the sum of the number of people in each compartment)
      N <- S + I + R + V

      # Defining lambda as a function of beta and I:
      lambda <- beta * I / N

      # The differential equations
      dS <- -lambda * S
      dI <- lambda * S - gamma * I
      dR <- gamma * I
      dV <- 0

      # Return the number of people in the S, I and R compartments at each timestep
      # )************(in the same order as the input state variables)************
      return(list(c(dS, dI, dR, dV)))
    })
  }

  # OBTAIN PARAMETERS FROM INPUT WIDGETS

  # DEMOGRAPHIC VALUES:
  pop <- uiInputParameters$pop_demo_sirv_c
  p_vacc <- uiInputParameters$loop_p_vacc_sirv_c

  # MODEL VALUES:
  beta <- uiInputParameters$model_beta_sirv_c # the infection rate, which acts on susceptibles per year
  gamma <- uiInputParameters$model_gamma_sirv_c

  # MODEL PARAMS
  modelParams <- c(beta = beta, gamma = gamma)


  # CALCULATE MODEL INITIAL STATE VALUES
  initial_state_values <- c(
    S = (1 - p_vacc) * pop - 1,
    I = 1,
    R = 0,
    V = p_vacc * pop
  )

  # TIMESTEPS
  timeframe <- uiInputParameters$timeframe_id
  times <- switch(timeframe,
    "days" = seq(from = 0, to = uiInputParameters$input_days[[1]], by = 1),
    "years" = seq(from = 0, to = uiInputParameters$input_years[[1]], by = 10 / 365)
  )

  # model_notify("Solving differential equations...")
  # MODEL OUTPUTS (solving the differential equations using the ode integration algorithm):
  output <- as.data.frame(ode(
    y = initial_state_values,
    times = times,
    func = model_SIRVc,
    parms = modelParams
  ))

  output_long <- reshape2::melt(as.data.frame(output), id = "time")

  output_long$proportion <- output_long$value / sum(initial_state_values)
  # model_notify("Computing model output...")
  return(list(df = output, df_long = output_long))
}


################# Resume here: To convert model params
modelParams <- c("model_gamma_sirv_c", "model_beta_sirv_c")

convertModelparams <- function(uiInputParameters) {
  s$timeframe_id
  req(paste(uiInputParameters, modelParams, sep = "$"))
}
