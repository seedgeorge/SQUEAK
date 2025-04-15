#' Plot Raw Slopes Per Individual and Group
#'
#' This SQUEAK function uses analysis-ready data and plots raw slopes (simple linear models), one per individual.
#' Log10 transformation is applied to the y-axis so make linear models usable.
#' A grouping variable is used to evaluate different treatments; if only one group is present this can be set to a dummy variable.
#' It performs minimal error checking and will crash ungracefully if the wrong things are given to it.
#' The optional parameters allow the user to specify axis/title labels.
#'
#' @param ready_data The input data.frame in long format, containing the columns: time, group, ID and measurement. 
#' @param title A custom title for the graph (optional)
#' @param subtitle A custom title for the graph (optional)
#' @param xtitle A custom x-axis title for the graph (optional)
#' @param ytitle A custom y-axis title for the graph (optional)
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
plot_raw_slopes = function(ready_data, title = NULL, subtitle = NULL, xtitle = NULL, ytitle = NULL) {
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
    scale_color_brewer(palette="Dark2") + # improve this with a custom palette option
    theme(legend.position="none") +
    ggtitle(label = ifelse(!is.null(title),title,"Raw Measurement Data"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"per Group")) 
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
#' @param title A custom title for the graph (optional)
#' @param subtitle A custom title for the graph (optional)
#' @param xtitle A custom x-axis title for the graph (optional)
#' @param ytitle A custom y-axis title for the graph (optional)
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
plot_raw_lines = function(ready_data, title = NULL, subtitle = NULL, xtitle = NULL, ytitle = NULL) {
  if(is.null(title)) {
    cli::cli_alert_warning("No title specified, using default.")
  }
  if(is.null(subtitle)) {
    cli::cli_alert_warning("No subtitle specified, using default.")
  }
  arm_slopes = ggplot(ready_data,aes(x = .data$time, y = .data$measurement,group=.data$ID)) + 
    facet_wrap(~ .data$group,nrow=length(unique(ready_data$group))) + 
    geom_line(col="darkgrey") + 
    theme_minimal() +
    theme(axis.title = element_text(size=10)) +
    theme(panel.grid.minor = element_blank()) +
    geom_point(aes(col=.data$group), alpha=0.5, size=1) +
    scale_x_continuous(name = ifelse(!is.null(xtitle),xtitle,"Time From Baseline")) + 
    scale_y_continuous(name = ifelse(!is.null(ytitle),ytitle,"Tumour Volume")) +
    scale_color_brewer(palette="Dark2") + # improve this with a custom optional palette option
    theme(legend.position="none") +
    ggtitle(label = ifelse(!is.null(title),title,"Raw Measurement Data"),
            subtitle = ifelse(!is.null(subtitle),subtitle,"per Group")) 
  return(arm_slopes)
}

