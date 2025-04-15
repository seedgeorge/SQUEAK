#' Run Mixed Effect Model
#'
#' This SQUEAK function wraps some code from nlme to run a mixed-effect regression model.
#' It will error out ungracefully if problems are detected, and expects a data frame with the columns: 'time','group','ID','measurement' It is suggested to run the utility function check_long() to determine if your data is the correct format.
#'
#' @param ready_data The input data.frame in long format, containing the columns: time, group, ID and measurement. 
#' @return The model fit.
#' @export
mixed_effect_model = function(ready_data) {
  nlme::lme(fixed = log(measurement) ~ time * group,
            random = ~ (1+time)|ID, # random slope random intercept
            control = list(msVerbose = TRUE, returnObject = TRUE),
            data = ready_data)
}