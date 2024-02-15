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
parameters <- list(
  demo_pop = numericInput(
    inputId = "demo_pop",
    label = "Population:",
    value = 500000,
    min = 0,
    max = NA,
    step = 1
  ),
  demo_p_vacc_loop = sliderInput(
    inputId = "demo_p_vacc_loop",
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
  model_beta = numericInput(
    inputId = "model_beta",
    label = "Infection rate/unit time (β):",
    value = 2.5,
    min = 0,
    max = NA,
    step = 0.01
  ),
  model_gamma = numericInput(
    inputId = "model_gamma",
    label = "Rate of recovery/unit time (γ):",
    value = 0.8,
    min = 0,
    max = NA,
    step = 0.01
  )
)


# ========================================================================== #
#                      DEFINE MODEL OUTPUT FUNCTION
# ========================================================================== #
modelOutput <- function(uiInputParameters) {
  req(
    uiInputParameters$demo_pop,
    uiInputParameters$demo_p_vacc_loop,
    uiInputParameters$model_beta,
    uiInputParameters$model_gamma
  )
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
  pop <- uiInputParameters$demo_pop
  p_vacc <- uiInputParameters$demo_p_vacc_loop

  # MODEL VALUES:
  beta <- uiInputParameters$model_beta # the infection rate, which acts on susceptibles per year
  gamma <- uiInputParameters$model_gamma

  # TIME MEASURE
  time_measure <- uiInputParameters$time_unit_id
  to <- ifelse(time_measure == "days",
    uiInputParameters$days_id[[1]],
    uiInputParameters$years_id[[1]]
  )

  # MODEL PARAMS
  modelParams <- c(beta = beta, gamma = gamma)


  # CALCULATE MODEL INITIAL STATE VALUES
  initial_state_values <- c(
    S = (1 - p_vacc) * pop - 1,
    I = 1,
    R = 0,
    V = p_vacc * pop
  )
  # })

  # TIMESTEPS
  if (time_measure == "days") {
    times <- seq(
      from = 0,
      to = to,
      by = 1
    )
  } else {
    times <- seq(
      from = 0,
      to = to,
      by = 10 / 365
    )
  }


  # MODEL OUTPUTS (solving the differential equations using the ode integration algorithm):
  output <- as.data.frame(ode(
    y = initial_state_values,
    times = times,
    func = model_SIRVc,
    parms = modelParams
  ))

  output_long <- reshape2::melt(as.data.frame(output), id = "time")

  output_long$proportion <- output_long$value / sum(initial_state_values)

  return(list(df = output, df_long = output_long))
}
