#' Run Mixed Effect Model
#'
#' This SQUEAK function wraps some code from nlme to run a mixed-effect regression model.
#' It will error out ungracefully if problems are detected, and expects a data frame with the columns: 'time','group','ID','measurement' It is suggested to run the utility function check_long() to determine if your data is the correct format.
#'
#' @param ready_data The input data.frame in long format, containing the columns: time, group, ID and measurement (required)
#' @param random_slope Option to set a random slope for each individual in the model (optional)
#' @param random_intercept Option to set a random intercept for each individual in the model (optional)
#' @return The model fit.
#' @export
mixed_effect_model = function(ready_data, random_slope = TRUE, random_intercept = TRUE) {
  if(random_slope == TRUE & random_intercept == TRUE) {
    model = nlme::lme(fixed = log(measurement) ~ time * group,
              random = ~ (1+time)|ID, # random slope random intercept
              control = list(msVerbose = TRUE, returnObject = TRUE),
              data = ready_data)
  }
  if (random_slope == TRUE & random_intercept == FALSE) {
    model = nlme::lme(fixed = log(measurement) ~ time * group,
              random = ~ 1 |ID, # random slope random intercept
              control = list(msVerbose = TRUE, returnObject = TRUE),
              data = ready_data) 
  }
  if (random_slope == FALSE & random_intercept == FALSE) {
    cli::cli_alert_info("Are you sure about not having random slopes or intercepts? Currently not supported...")
    stop()
  }
 return(model) 
}


# m1 = plot_model(t1,terms = c("time:groupIpatasertib",
#                         "time:groupS63845",
#                         "time:groupIpa+S63845"),
#            pred.type = "re",show.intercept = T) $data
# m1$term = recode(m1$term,"time:groupIpatasertib" = "Ipatasertib",
#                 "time:groupS63845" = "S63845",
#                 "time:groupIpa+S63845" = "Ipa+S63845")
# m1$pvalformatted = paste("p =",format.pval(m1$p.value,eps= 0.001,digits = 2))
# model_forest = ggplot(data = m1,
#                       aes(y=term, x=estimate,xmin = conf.low, 
#                           xmax = conf.high,col = term,label=term)) + 
#   geom_pointrange() + 
#   xlim(-0.1,0.05) +
#   theme_minimal() + 
#   theme(legend.position = "none",axis.text.y = element_blank(),
#         axis.title = element_text(size=10)) +
#   scale_x_continuous(name = "Coefficient") +
#   scale_color_manual(name = "Arm",
#                      values = c("Ipatasertib" = "tomato",
#                                 "S63845" = "dodgerblue",
#                                 "Ipa+S63845" = "purple3")) +
#   scale_y_discrete(name = "Drug Arm") + 
#   geom_vline(xintercept = 0,linetype="dashed",col="grey10") + 
#   geom_text(nudge_y = 0.25,size=3) +
#   geom_text(data=m1,
#             aes(y=term, x=estimate,col = term,label=pvalformatted),
#             nudge_y = -0.25,size=3) + 
#   ggtitle(label = "Growth Rate Difference",
#           subtitle = "Versus Vehicle")
# model_forest
# 
# 
# 
# m1 = plot_model(t2,terms = c("time:groupIpatasertib",
#                              "time:groupS63845",
#                              "time:groupIpa+S63845"),
#                 pred.type = "re",show.intercept = T) $data
# m1$term = recode(m1$term,"time:groupIpatasertib" = "Ipatasertib",
#                  "time:groupS63845" = "S63845",
#                  "time:groupIpa+S63845" = "Ipa+S63845")
# m1$pvalformatted = paste("p =",format.pval(m1$p.value,eps= 0.001,digits = 2))
# model_forest = ggplot(data = m1,
#                       aes(y=term, x=estimate,xmin = conf.low, 
#                           xmax = conf.high,col = term,label=term)) + 
#   geom_pointrange() + 
#   xlim(-0.1,0.05) +
#   theme_minimal() + 
#   theme(legend.position = "none",axis.text.y = element_blank(),
#         axis.title = element_text(size=10)) +
#   scale_x_continuous(name = "Coefficient") +
#   scale_color_manual(name = "Arm",
#                      values = c("Ipatasertib" = "tomato",
#                                 "S63845" = "dodgerblue",
#                                 "Ipa+S63845" = "purple3")) +
#   scale_y_discrete(name = "Drug Arm") + 
#   geom_vline(xintercept = 0,linetype="dashed",col="grey10") + 
#   geom_text(nudge_y = 0.25,size=3) +
#   geom_text(data=m1,
#             aes(y=term, x=estimate,col = term,label=pvalformatted),
#             nudge_y = -0.25,size=3) + 
#   ggtitle(label = "Growth Rate Difference",
#           subtitle = "Versus Vehicle")
# model_forest
