#' Plot Raw Slopes Per Individual and Group
#'
#' This SQUEAK function uses analysis-ready data and plots raw slopes (simple linear models), one per individual.
#' Log10 transformation is applied to the y-axis so make linear models usable.
#' A grouping variable is used to evaluate different treatments; if only one group is present this can be set to a dummy variable.
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#' The optional parameters allow the user to specify axis/title labels.
#'
#' @param ready_data The input data.frame in long format, containing the columns: time, group, ID and measurement
#' @param title A custom title for the graph (optional)
#' @param subtitle A custom title for the graph (optional)
#' @param xtitle A custom x-axis title for the graph (optional)
#' @param ytitle A custom y-axis title for the graph (optional)
#' @param palette A custom palette for the graph, from the set_palette() function (optional)
#' @return A graph in ggplot format, plotting the slope for each individual over time, split by the grouping variable (eg. drug arm).
#' @examples
#' data("long_mice")
#' ready_data = check_long(long_mice,
#'                         timecol = "Days",
#'                         IDcol = 'Number',
#'                         groupcol = 'Group',
#'                         measurementcol = 'Value')
#' plot_raw_slopes(ready_data = ready_data ) # expects the columns: time, group, ID and measurement
#' @export
plot_raw_slopes = function(ready_data, title = NULL, subtitle = NULL, xtitle = NULL, ytitle = NULL,palette = NULL) {
  if(is.null(title)) {
    cli::cli_alert_warning("No title specified, using default.")
  }
  if(is.null(subtitle)) {
    cli::cli_alert_warning("No subtitle specified, using default.")
  }
  arm_slopes = ggplot(ready_data,aes(x = .data$time, y = log10(.data$measurement),group=.data$ID)) + 
    facet_wrap(~ .data$group,nrow=length(unique(ready_data$group))) + 
    stat_smooth(method = "lm",col="grey10",se = F,linetype=2,size=0.5) + 
    theme_minimal() +
    theme(axis.title = element_text(size=10)) +
    theme(panel.grid.minor = element_blank()) +
    geom_point(aes(col=.data$group),alpha=1,size=1) + 
    scale_x_continuous(name = ifelse(!is.null(xtitle),xtitle,"Time From Baseline")) + 
    scale_y_continuous(name = ifelse(!is.null(ytitle),ytitle,"Tumour Volume")) +
    scale_color_brewer(palette="Dark2") + 
    theme(legend.position = "none", axis.title = element_text(size=10)) +
    ggtitle(label = ifelse(!is.null(title),title,"Raw Measurement Data"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"per Group")) 
  if(!is.null(palette)) {
    arm_slopes = arm_slopes + scale_color_manual(values = palette)
  }
  return(arm_slopes)
}

#' Plot Lines Per Individual and Group
#'
#' This SQUEAK function uses analysis-ready data and plots lines for each individual.
#' No y-axis transformation is applied, so curves should be apparent.
#' A grouping variable is used to evaluate different treatments; if only one group is present this can be set to a dummy variable.
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#' The optional parameters allow the user to specify axis/title labels.
#'
#' @param ready_data The input data.frame in long format, containing the columns: time, group, ID and measurement. 
#' @param facet Should each group be a separate graph (TRUE), or combined into one (FALSE)? 
#' @param title A custom title for the graph (optional)
#' @param subtitle A custom title for the graph (optional)
#' @param xtitle A custom x-axis title for the graph (optional)
#' @param ytitle A custom y-axis title for the graph (optional)
#' @param palette A custom palette for the graph, from the set_palette() function (optional)
#' @return A graph in ggplot format, plotting one line for each individual over time, split by the grouping variable (eg. drug arm).
#' @examples
#' data("long_mice")
#' ready_data = check_long(long_mice,
#'                         timecol = "Days",
#'                         IDcol = 'Number',
#'                         groupcol = 'Group',
#'                         measurementcol = 'Value')
#' plot_raw_lines(ready_data = ready_data ) # expects the columns: time, group, ID and measurement
#' @export
plot_raw_lines = function(ready_data, facet=TRUE, title = NULL, subtitle = NULL, xtitle = NULL, ytitle = NULL,palette = NULL) {
  if(is.null(title)) {
    cli::cli_alert_warning("No title specified, using default.")
  }
  if(is.null(subtitle)) {
    cli::cli_alert_warning("No subtitle specified, using default.")
  }
  arm_slopes = ggplot(ready_data,aes(x = .data$time, y = .data$measurement,group=.data$ID)) + 
    geom_line(col="darkgrey") + 
    theme_minimal() +
    theme(axis.title = element_text(size=10)) +
    theme(panel.grid.minor = element_blank()) +
    geom_point(aes(col=.data$group), alpha=0.5, size=1) +
    scale_x_continuous(name = ifelse(!is.null(xtitle),xtitle,"Time From Baseline")) + 
    scale_y_continuous(name = ifelse(!is.null(ytitle),ytitle,"Tumour Volume")) +
    scale_color_brewer(palette="Dark2") + 
    theme(legend.position = "none",axis.title = element_text(size=10)) +
    ggtitle(label = ifelse(!is.null(title),title,"Raw Measurement Data"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"per Group")) 
  if(!is.null(palette)) {
    arm_slopes = arm_slopes + scale_color_manual(values = palette)
  }
  if(facet == TRUE) {
    arm_slopes = arm_slopes + facet_wrap(~ .data$group,nrow=length(unique(ready_data$group))) 
  }
  return(arm_slopes)
}


#' Plot Estimated Curves from the ME Model
#'
#' This SQUEAK function uses analysis-ready data and plots curves for each group
#' No y-axis transformation is applied, so curves should be apparent.
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#' The optional parameters allow the user to specify axis/title labels.
#'
#' @param model The mixed-effect model produced by the mixed_effect_model function
#' @param xtitle A custom x-axis title for the graph (optional)
#' @param ytitle A custom y-axis title for the graph (optional)
#' @param title A custom title for the graph (optional)
#' @param subtitle A custom title for the graph (optional)
#' @param palette A custom palette for the graph, from the set_palette() function (optional)
#' @param grouptitle A custom title for the grouping variable, replacing the default 'group' (optional)
#' @param ylim A custom set of y-axis limits, as a two-element vector ie. c(0,100) (optional)
#' @param show_confidence Do you want to see confidence intervals? TRUE/FALSE, default=TRUE (optional)
#' @return A graph in ggplot format, plotting one growth curve per the grouping variable (eg. drug arm).
#' @examples
#' data("long_mice")
#' ready_data = check_long(long_mice,
#'                         timecol = "Days",
#'                         IDcol = 'Number',
#'                         groupcol = 'Group',
#'                         measurementcol = 'Value')
#' mixed_model = mixed_effect_model(ready_data = ready_data)
#' plot_modelled_curves(model = mixed_model ) 
#' @export
plot_modelled_curves = function(model,xtitle = NULL,ytitle = NULL,title = NULL, subtitle = NULL,palette = NULL,grouptitle = NULL, ylim=NULL,show_confidence = TRUE) {
  model_interactions_curves  = sjPlot::plot_model(model,type="int",ci.lvl = ifelse(show_confidence == TRUE,0.95,NA)) + 
    theme_minimal() + 
    scale_y_continuous(name = ifelse(!is.null(ytitle),ytitle,"Tumour Volume")) + 
    scale_x_continuous(name = ifelse(!is.null(xtitle),xtitle,"Time From Baseline")) + 
    scale_color_brewer(palette="Dark2") + # improve this with a custom optional palette option
    ggtitle(label = ifelse(!is.null(title),title,"Predicted Value per Group"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"Mixed-Effect Linear Model")) +
    theme(axis.title = element_text(size=10)) 
  if(!is.null(palette)) {
    model_interactions_curves = model_interactions_curves + scale_color_manual(values = palette)
  }
  if(!is.null(grouptitle)) {
    model_interactions_curves = model_interactions_curves +  ggplot2::guides(color=ggplot2::guide_legend(title=grouptitle))
  }
  if(!is.null(ylim)) {
    model_interactions_curves = model_interactions_curves + ggplot2::coord_cartesian(ylim=ylim)
  }
  return(model_interactions_curves)
}


#' Plot Estimated Slopes from the ME Model
#'
#' This SQUEAK function a pre-computed model and plots curves for each group
#' Y-axis is log-transformed, so curves should be apparent.
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#' The optional parameters allow the user to specify axis/title labels.
#'
#' @param model The mixed-effect model produced by the mixed_effect_model function
#' @param xtitle A custom x-axis title for the graph (optional)
#' @param ytitle A custom y-axis title for the graph (optional)
#' @param title A custom title for the graph (optional)
#' @param subtitle A custom title for the graph (optional)
#' @param palette A custom palette for the graph, from the set_palette() function (optional)
#' @param grouptitle A custom title for the grouping variable, replacing the default 'group' (optional)
#' @return A graph in ggplot format, plotting one growth curve per the grouping variable (eg. drug arm).
#' @examples
#' data("long_mice")
#' ready_data = check_long(long_mice,
#'                         timecol = "Days",
#'                         IDcol = 'Number',
#'                         groupcol = 'Group',
#'                         measurementcol = 'Value')
#' mixed_model = mixed_effect_model(ready_data = ready_data)
#' plot_modelled_slopes(model = mixed_model ) 
#' @export
plot_modelled_slopes = function(model,xtitle = NULL,ytitle = NULL,title = NULL,subtitle = NULL,palette=NULL,grouptitle = NULL) {
  curves = plot_modelled_curves(model)
  d = curves$data
  d = as.data.frame(d)
  model_interactions_slopes = ggplot(data = d, aes(x= .data$x,y=log(.data$predicted),col=.data$group)) + 
    geom_line(size=1) + 
    theme_minimal() + 
    scale_y_continuous(name = ifelse(!is.null(ytitle),ytitle,"Tumour Volume")) + 
    scale_x_continuous(name = ifelse(!is.null(xtitle),xtitle,"Time From Baseline")) + 
    scale_color_brewer(palette="Dark2") + 
    ggtitle(label = ifelse(!is.null(title),title,"Predicted Value per Group"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"Mixed-Effect Linear Model")) +
    theme(axis.title = element_text(size=10)) 
  if(!is.null(palette)) {
    model_interactions_slopes = model_interactions_slopes + scale_color_manual(values = palette)
  }  
  if(!is.null(grouptitle)) {
    model_interactions_slopes = model_interactions_slopes + ggplot2::guides(color=ggplot2::guide_legend(title=grouptitle))
  }
  return(model_interactions_slopes)
}


#' Plot Interaction Terms from the ME Model
#'
#' This SQUEAK function uses a pre-computed model and plots the interaction term results for each group.
#' The results are presented against a reference level. 
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#'
#' @param model The mixed-effect model produced by the mixed_effect_model function
#' @param palette A custom palette for the graph, from the set_palette() function (optional)
#' @param title A custom title for the graph (optional)
#' @param subtitle A custom title for the graph (optional)
#' @return A graph in ggplot format, a forest plot of interaction terms, ie. slope coefficients.
#' @export
plot_interaction_forest = function(model, palette=NULL,title = NULL,subtitle = NULL) {
  # all credit to sjPlot, we are leaning on it hard here
  
  # forest model of interactions
  m = sjPlot::plot_model(model,
                         terms = colnames(stats::coef(model))[grepl(x = colnames(stats::coef(model)),pattern = ":")],
                         pred.type = "re",show.intercept = T)$data
  m$term = gsub(x=as.vector(m$term),pattern = "time:group",replacement = "")
  m$term = factor(m$term,levels = rev(m$term))
  m$pvalformatted = paste("p =", format.pval(m$p.value, eps= 0.001, digits = 3))
  # make plot 
  model_forest = ggplot(data = m,
                        aes(y=.data$term, 
                            x=.data$estimate,
                            xmin = .data$conf.low, 
                            xmax = .data$conf.high,
                            col = .data$term,label=.data$term)) + 
    geom_pointrange() + 
    xlim(-0.1,0.05) +
    theme_minimal() + 
    theme(legend.position = "none", 
          axis.text.y = element_blank(),
          axis.title = element_text(size=10)) +
    scale_x_continuous(name = "Coefficient") +
    scale_color_brewer(palette="Dark2") + 
    scale_y_discrete(name = "Group") + 
    geom_vline(xintercept = 0, linetype="dashed", col="grey10") + 
    geom_text(nudge_y = 0.25, size=3) +
    geom_text(data=m,
              aes(y=.data$term, 
                  x=.data$estimate,
                  col = .data$term,
                  label=.data$pvalformatted),
              nudge_y = -0.25,size=3) + 
    ggtitle(label = ifelse(!is.null(title),title,"Growth Rate Difference"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"Versus Control")) 
  if(!is.null(palette)) {
    model_forest = model_forest + scale_color_manual(values = palette)
  }
  return(model_forest)
}
