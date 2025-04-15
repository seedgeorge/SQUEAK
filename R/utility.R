#' Check Long-Format Data File
#'
#' This SQUEAK function checks a long-format data frame for downstream compatibility.
#' It will error out if problems are detected, and otherwise return an analysis-ready data frame.
#' It expects a data frame with the columns: 'time','group','ID','measurement'
#' The optional parameters (timecol etc) allow the user to specify the names of the columns that should be used instead of the above. The analyses this package is geared for are essentially predicated on having: multiple measurements (for example, tumour volume) over time (days, weeks, etc), across multiple individuals (animal models) and treatment arms (or strains, etc).
#'
#' @param long_data The input data.frame in long format.
#' @param timecol Name of the 'time' column (optional).
#' @param groupcol Name of the 'group' column (optional).
#' @param IDcol Name of the 'ID' column (optional).
#' @param measurementcol Name of the 'measurement' column (optional).
#' @return The infile, checked and cleaned, still in long format.
#' @export
check_long = function(long_data,timecol = NULL, groupcol = NULL, IDcol = NULL, measurementcol = NULL) {
  # Check if input is a data frame
  if (!is.data.frame(long_data)) {
    stop("Input must be a data frame!")
  }
  # Check there are 4 columns
  expected_cols = 4
  if (ncol(long_data) < expected_cols){
    stop("Not enough columns to this data - are you sure this method is appropriate?")
  }
  
  # Do we have the right columns? 
  model_columns = c('time','group','ID','measurement')
  # time
  if(is.null(timecol)) {
    if (sum(grepl(pattern = '^time$',x=colnames(long_data))) != 1) {
      stop("No time column found! Add one or specify using the 'timecol' parameter!")
    }
  }
  # group
  if(is.null(groupcol)) {
    if (sum(grepl(pattern = '^group$',x=colnames(long_data))) != 1) {
      stop("No group column found! Add one or specify using the 'groupcol' parameter!")
    }
  }
  # ID
  if(is.null(IDcol)) {
    if (sum(grepl(pattern = '^ID$',x=colnames(long_data))) != 1) {
      stop("No individual ID column found! Add one or specify using the 'IDcol' parameter!")
    }
  }
  # measurement
  if(is.null(measurementcol)) {
    if (sum(grepl(pattern = '^measurement$',x=colnames(long_data))) != 1) {
      stop("No measurement column found! Add one or specify using the 'measurementcol' parameter!")
    }
  }
  
  ## Ok so far? Leave a note.
  cli::cli_alert_success("All columns detected!")

  ## add something here to check columnn types
  ## numeric for time/measurement definitely
  ## character for the others maybe?
  
  ## Any NA data?
  NArows = sum(rowSums(is.na(long_data)))
  cli::cli_alert_warning(paste0(NArows," rows with NA detected, dropping these rows."))
  long_data = long_data[complete.cases(long_data),]
  cli::cli_alert_success(paste0(nrow(long_data)," rows remaining."))
  
  # Some summary numbers.
  cat("Data statistics:\n")
  cli::cli_alert_info(paste0(length(unique(unlist(long_data[,IDcol]))),' Individuals'))
  cli::cli_alert_info(paste0(length(unique(unlist(long_data[,groupcol]))),' Groups'))
  cli::cli_alert_info(paste0('Start Time: ',min(long_data[,timecol])))
  cli::cli_alert_info(paste0('End Time: ',max(long_data[,timecol])))
  cli::cli_alert_info(paste0('Minimum measurement: ',min(long_data[,measurementcol])))
  cli::cli_alert_info(paste0('Maximum measurement: ',max(long_data[,measurementcol])))
  
  # output just the columns we want to output
  fixed_data = long_data[,c(timecol,groupcol,IDcol,measurementcol)]
  colnames(fixed_data) = model_columns
  
  # return the model-ready data frame
  return(fixed_data)
}

