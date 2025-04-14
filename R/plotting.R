#' Plot Raw Slopes Per Individual+Group
#'
#' This SQUEAK function uses analysis-ready data and plots raw slopes (simple linear model), one per individual.
#' Log10 transformation is applied to the y-axis so make linear models usable.
#' Grouping variable is used to see different treatments etc. 
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#' The optional parameters allow the user to specify axis/title labels.
#'
#' @param long_data The input data.frame in long format.
#' @return A nice faceted plot.
#' @export
plot_raw_slopes = function(ready_data, title = NULL, subtitle = NULL, xtitle = NULL, ytitle = NULL) {
  if(is.null(title)) {
    cli::cli_alert_warning("No title specified, using default.")
  }
  if(is.null(subtitle)) {
    cli::cli_alert_warning("No subtitle specified, using default.")
  }
  arm_slopes = ggplot(ready_data,aes(x = time, y = log10(measurement),group=ID)) + 
    facet_wrap(~ group,nrow=length(unique(ready_data$group))) + 
    stat_smooth(method = "lm",col="grey10",se = F,linetype=2,size=0.5) + 
    theme_minimal() +
    theme(axis.title = element_text(size=10)) +
    theme(panel.grid.minor = element_blank()) +
    geom_point(aes(col=group),alpha=1,size=1) + 
    scale_x_continuous(name = ifelse(!is.null(xtitle),xtitle,"Time From Baseline")) + 
    scale_y_continuous(name = ifelse(!is.null(ytitle),ytitle,"Tumour Volume")) +
    scale_color_brewer(palette="Dark2") + # improve this with a custom palette option
    theme(legend.position="none") +
    ggtitle(label = ifelse(!is.null(title),title,"Raw Measurement Data"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"per Group")) 
  return(arm_slopes)
}

#' Plot Lines Per Individual+Group
#'
#' This SQUEAK function uses analysis-ready data and plots lines for each individual.
#' No y-axis transformation is applied, so curves should be apparent.
#' Grouping variable is used to see different treatments etc. 
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#' The optional parameters allow the user to specify axis/title labels.
#'
#' @param long_data The input data.frame in long format.
#' @return A nice faceted plot.
#' @export
plot_raw_lines = function(ready_data, title = NULL, subtitle = NULL, xtitle = NULL, ytitle = NULL) {
  if(is.null(title)) {
    cli::cli_alert_warning("No title specified, using default.")
  }
  if(is.null(subtitle)) {
    cli::cli_alert_warning("No subtitle specified, using default.")
  }
  arm_slopes = ggplot(ready_data,aes(x = time, y = measurement,group=ID)) + 
    facet_wrap(~ group,nrow=length(unique(ready_data$group))) + 
    geom_line(col="darkgrey") + 
    theme_minimal() +
    theme(axis.title = element_text(size=10)) +
    theme(panel.grid.minor = element_blank()) +
    geom_point(aes(col=group), alpha=0.5, size=1) +
    scale_x_continuous(name = ifelse(!is.null(xtitle),xtitle,"Time From Baseline")) + 
    scale_y_continuous(name = ifelse(!is.null(ytitle),ytitle,"Tumour Volume")) +
    scale_color_brewer(palette="Dark2") + # improve this with a custom optional palette option
    theme(legend.position="none") +
    ggtitle(label = ifelse(!is.null(title),title,"Raw Measurement Data"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"per Group")) 
  return(arm_slopes)
}

