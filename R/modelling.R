#' Run Mixed Effect Model
#'
#' This SQUEAK function wraps some code from nlme to run a mixed-effect regression model.
#' It will error out ungracefully if problems are detected, and expects a data frame with the columns: 'time','group','ID','measurement' It is suggested to run the utility function check_long() to determine if your data is the correct format.
#'
#' Some guidelines:
#' - You should use random slope almost always. This means 'each individual can grow at a different rate', and the model will factor that in.
#' - If the model fails to converge, you can consider setting random_intercept to FALSE, which produces a less-strict but still-plausible model.
#' - If the model still fails to converge, the data may simply not be viable for this analysis.
#' 
#' @param ready_data The input data.frame in long format, containing the columns: time, group, ID and measurement (required)
#' @param random_slope Option to set a random slope for each individual in the model (optional)
#' @param random_intercept Option to set a random intercept for each individual in the model (optional)
#' @return The model fit.
#' @export
mixed_effect_model = function(ready_data, random_slope = TRUE, random_intercept = TRUE) {
  
  if (random_slope == TRUE & random_intercept == TRUE) {
    model = nlme::lme(fixed = log(measurement) ~ time * group, 
              random = ~ (1+time)|ID, # random slope random intercept
              control = list(msVerbose = TRUE, returnObject = TRUE),
              data = ready_data)
  }
  if (random_slope == TRUE & random_intercept == FALSE) {
    model = nlme::lme(fixed = log(measurement) ~ time * group,
              random = ~ 0+time | ID , # random slope 
              control = list(msVerbose = TRUE, returnObject = TRUE),
              data = ready_data) 
  }
  if (random_slope == FALSE & random_intercept == TRUE) {
    model = nlme::lme(fixed = log(measurement) ~ time * group,
                      random = ~ 1 |ID, # random intercept 
                      control = list(msVerbose = TRUE, returnObject = TRUE),
                      data = ready_data) 
  }
  if (random_slope == FALSE ) {
    cli::cli_alert_info("Are you sure about not having random slopes? That is not really what this package was designed for.")
    stop()
  }
 return(model) 
}

