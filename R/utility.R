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
#' @param set_nested Individual IDs should usually be unique to treatment groups. If this is not desired, set this flag to FALSE.
#' @return The infile, checked and cleaned, still in long format.
#' @export
check_long = function(long_data, timecol = NULL, groupcol = NULL, IDcol = NULL, measurementcol = NULL, set_nested = TRUE) {
  model_columns = c('time','group','ID','measurement') # these are the standard names
  
  ## check the number of columns is right etc, if not, crash out violently
  long_data = column_check(long_data,timecol=timecol,groupcol=groupcol,IDcol=IDcol,measurementcol = measurementcol)

  ## figure out if ID data is crossed or nested within group and raise a warning if crossed
  ## most mouse experiments are nested, ie. each mouse is unique to a group - you don't treat the same mouse on multiple arms
  groupsplit = split(long_data,long_data[,groupcol])
  detect_crossed = any(as.vector(unique(groupsplit[[1]][,IDcol]))[[1]] %in% as.vector(unique(groupsplit[[2]][,IDcol]))[[1]])
  if (detect_crossed == TRUE) {
    cli::cli_alert_warning("Non-unique individual IDs detected in the first two groups, this will result in a crossed model rather than a nested one. Usually mice are only treated once.")
    if(set_nested == TRUE) {
      cli::cli_alert_warning("Making unique IDs! Change this behaviour with the 'set_nested' parameter.")
      long_data[,IDcol] = apply(long_data[,c(groupcol,IDcol)],1,paste,collapse = '_')
    }
  } else {
    cli::cli_alert_info("Data structure suggests nested model, with unique individuals per group.")
  }
  
  ## Set Group Order based on internal order [add option to this later?]
  long_data[,groupcol] = factor(as.vector(unlist(long_data[,groupcol])),
                                levels = unique(as.vector(unlist(long_data[,groupcol]))))
  groups = as.vector(unlist(unique(long_data[,groupcol])))
  cli::cli_alert_info("Groups: {.val {groups}}")
  cli::cli_alert_info("Assuming reference level: {.val {groups[1]}}")
  
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



#' Column Check for Long Format Data Frae
#'
#' This internal SQUEAK function looks at the columns of a data.frame in long format.
#' It will errors out if common issues are detected.
#' It expects a data frame with the columns: 'time','group','ID','measurement'
#' The optional parameters (timecol etc) allow the user to specify the names of the columns that should be used instead of the above. The analyses this package is geared for are essentially predicated on having: multiple measurements (for example, tumour volume) over time (days, weeks, etc), across multiple individuals (animal models) and treatment arms (or strains, etc).
#'
#' @param long_data The input data.frame in long format.
#' @param timecol Name of the 'time' column (optional).
#' @param groupcol Name of the 'group' column (optional).
#' @param IDcol Name of the 'ID' column (optional).
#' @param measurementcol Name of the 'measurement' column (optional).
#' @return The infile, checked and cleaned, still in long format.
column_check = function(long_data, timecol = NULL, groupcol = NULL, IDcol = NULL, measurementcol = NULL) {
  # Check if input is a data frame
  if (!is.data.frame(long_data)) {
    stop("Input must be a data frame!")
  }
  # Check there are 4 columns
  expected_cols = 4
  if (ncol(long_data) < expected_cols){
    stop("Not enough columns to this data - are you sure this method is appropriate?")
  }
  
  # time column found
  if(is.null(timecol)) {
    if (sum(grepl(pattern = '^time$',x=colnames(long_data))) != 1) {
      stop("No time column found! Add one or specify using the 'timecol' parameter!")
    }
  }
  # group column found
  if(is.null(groupcol)) {
    if (sum(grepl(pattern = '^group$',x=colnames(long_data))) != 1) {
      stop("No group column found! Add one or specify using the 'groupcol' parameter!")
    }
  }
  # ID column found
  if(is.null(IDcol)) {
    if (sum(grepl(pattern = '^ID$',x=colnames(long_data))) != 1) {
      stop("No individual ID column found! Add one or specify using the 'IDcol' parameter!")
    }
  }
  # measurement column found
  if(is.null(measurementcol)) {
    if (sum(grepl(pattern = '^measurement$',x=colnames(long_data))) != 1) {
      stop("No measurement column found! Add one or specify using the 'measurementcol' parameter!")
    }
  }
  
  ## Ok so far? Leave a note.
  cli::cli_alert_success("All columns detected!")
  
  ## add something here to check columnn types
  ## numeric for time/measurement definitely
  measurement_numeric = is.numeric(unlist(long_data[,colnames(long_data) == 'measurement' | colnames(long_data) == measurementcol]))  
  if(measurement_numeric == FALSE) {
    stop("Measurements are not numeric!")
  }
  time_numeric = is.numeric(unlist(long_data[,colnames(long_data) == 'time' | colnames(long_data) == timecol]))  
  if(time_numeric == FALSE) {
    stop("Times are not numeric!")
  }
  cli::cli_alert_success("Data types good!")
  
  return(long_data)
}
