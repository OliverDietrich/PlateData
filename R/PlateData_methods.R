### Methods for the PlateData class

#-------------------------------------------------------------------------------
# Constructor function for PlateData class
#-------------------------------------------------------------------------------

#' Create a \code{PlateData} object
#'
#' Create a \code{PlateData} object from layout and data
#'
#' @param layout Data.frame containing columns 'plate' and 'well' alongside metadata
#' @param data Data.frame containing columns \code{plate} and \code{well} alongside \code{value} and \code{time}
#' @param value Measurement values
#' @param time Name of the column that defines longitudinal measurements (e.g. 'Hours' for time series).
#' @param treatment Column with treatment variable (e.g. Phage)
#' @param predictors Column(s) that predict treatment effect on value (e.g. Bacterial strain AND Plasmid)
#' @param blanks Boolean vector indicating layout rows serving as blank
#' @param controls Boolean vector indicating layout rows serving as controls
#'
#' @return A \code{\link{PlateData}} object
#'
#' @rdname CreatePlateData
#' @export
#'
CreatePlateData <- function(
    layout,
    raw,
    value,
    time,
    predictors,
    treatment,
    blanks = NULL,
    controls = NULL,
    ...
) {

    # Check input
    stopifnot(
        is.data.frame(layout),
        is.data.frame(raw)
    )
    if ('key' %in% names(layout)) {
        warning("Column 'key' present in layout, will be removed...")
        layout$key <- NULL
    }
    if ('key' %in% names(raw)) {
        warning("Column 'key' present in raw, will be overwritten...")
    }
    if ('Plate' %in% names(layout)) {
        layout$plate <- layout$Plate
        layout$Plate <- NULL
        warning("Found column 'Plate' in layout, changed to 'plate'.")
    }
    if ('Plate' %in% names(raw)) {
        raw$plate <- raw$Plate
        raw$Plate <- NULL
        warning("Found column 'Plate' in raw, changed to 'plate'.")
    }
    if ('Well' %in% names(layout)) {
        layout$well <- layout$Well
        layout$Well <- NULL
        warning("Found column 'Well' in layout, changed to 'well'.")
    }
    if ('Well' %in% names(raw)) {
        raw$well <- raw$Well
        raw$Well <- NULL
        warning("Found column 'Well' in raw, changed to 'well'.")
    }
    if (!'well' %in% names(layout)) stop("Column 'well' missing from layout. Stopped...")
    if (!'well' %in% names(raw)) stop("Column 'well' missing from raw Stopped...")
    
    # Create key
    row.names(layout) <- paste(layout$plate, layout$well, sep = '_')
    raw$key <- paste(raw$plate, raw$well, sep = '_')

    # Remove key components from data
    raw$well <- NULL
    raw$plate <- NULL

    # Extract row and column from 'well' in layout
    layout$row <- factor(as.character(stringr::str_split_fixed(layout$well, "", 2)[,1]))
    layout$col <- factor(as.character(stringr::str_split_fixed(layout$well, "", 2)[,2]))

    # Check containment of data in layout
    ind <- names(layout) %in% names(raw)
    if (any(ind)) {
        msg <- paste0('Layout columns "', paste(names(layout)[ind], collapse = ', '), '" also appear in raw Aborting...')
        stop(msg)
    }

    # Create combined from layout and data
    combined <- merge(raw, layout, by.x = 'key', by.y = "row.names")
    combined$value <- combined[[value]]
    combined$time <- combined[[time]]
    combined$treatment <- combined[[treatment]]
    if (length(predictors) == 1) {
        combined$predictor <- combined[[predictors]]
    } else {
        combined$predictor <- apply(x[, predictors], 1, paste, collapse = '--')
    }

    # Detect plate type
    my_plate_type <- detect_plate_type(layout)

    
    # Deal with unregistered wells
    # ...

    # Separate blank
    blank_keys <- rownames(layout[blanks, ])
    ind <- combined$key %in% blank_keys
    blank <- combined[ind, ]
    blank <- dplyr::group_by(blank, plate, time)
    blank <- dplyr::summarize(blank, blank = mean(value), sd_blank = sd(value))
    blank <- as.data.frame(blank)

    # Separate controls
    control_keys <- rownames(layout[controls, ])
    ind <- combined$key %in% control_keys
    control <- combined[ind, ]
    control <- dplyr::group_by(control, plate, time, predictor)
    control <- dplyr::summarize(control, control = mean(value), sd_control = sd(value))
    control <- as.data.frame(control)

    # Add blank to control
    control <- merge(control, blank)
    control$corrected_control <- control$control - control$blank
    control$sd_corrected_control <- sqrt( control$sd_control^2 + control$sd_blank^2 - 0 * control$sd_control * control$sd_blank)
    
    # Separate data
    ind <- which(!combined$key %in% c(blank_keys, control_keys)) # Why the fuck does this not work???
    data <- combined[ind, ]

    # Summarize data
    summary <- dplyr::group_by(data, plate, time, predictor, treatment) %>% summarize(mean_value = mean(value), median_value = median(value), sd_value = sd(value))

    # Add control (and, by extension, blanks) to data and summary
    data <- merge(data, control)
    summary <- merge(summary, control)

    # Correct data
    data$corrected_value <- data$value - data$blank
    data$sd_corrected_value <- data$sd_blank

    # Normalise data
    data$normalised_value <- data$corrected_value / data$corrected_control
    data$sd_normalised_value <- sqrt(data$normalised_value^2) * sqrt( (data$sd_corrected_value / data$corrected_value)^2 + (data$sd_corrected_control / data$corrected_control)^2 - 0 )
    
    # Correct summary
    summary$corrected_mean_value <- summary$mean_value - summary$blank
    summary$corrected_median_value <- summary$median_value - summary$blank
    summary$sd_corrected_mean_value <- sqrt( summary$sd_value^2 + summary$sd_blank^2 - 0 * summary$sd_value * summary$sd_blank)
    
    # Normalise summary
    summary$normalised_mean_value <- summary$corrected_mean_value / summary$corrected_control
    summary$normalised_median_value <- summary$corrected_median_value / summary$corrected_control
    summary$sd_normalised_mean_value <- sqrt(summary$normalised_mean_value^2) * sqrt( (summary$sd_corrected_mean_value / summary$corrected_mean_value)^2 + (summary$sd_corrected_control / summary$corrected_control)^2 - 0 )

    # Create object
    methods::new("PlateData", 
                 layout = layout, 
                 raw = raw, 
                 value = value,
                 time = time,
                 treatment = treatment,
                 predictors = predictors,
                 combined = combined, 
                 blank = blank,
                 control = control,
                 data = data,
                 summary = summary,
                 type = my_plate_type
                )
}

#-------------------------------------------------------------------------------
# Define validity check for PlateData class object
#-------------------------------------------------------------------------------

.pd_validity <- function(object) {
    msg <- NULL
    valid <- TRUE

    # Check that the key is present in data and used as unique row.names in layout
    if ('key' %in% names(layout(object))) {
      msg <- c(msg, "Columm 'key' must not be present in layout(data).")
      valid <- FALSE
    }
    
    if(valid) TRUE else msg
}

methods::setValidity("PlateData", .pd_validity)

#-------------------------------------------------------------------------------
# subsetting a PlateData object
#-------------------------------------------------------------------------------

.pd_subset <- function(x) {

}

setMethod("subset", "PlateData", .pd_subset)

#-------------------------------------------------------------------------------
# merging PlateData object
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# raw
#-------------------------------------------------------------------------------

#' Accessors for the 'raw' element of an PlateData object.
#'
#' @description 
#' The \code{raw} slot in an PlateData object holds
#' a data.frame containing raw data imported from device-specific 
#' output files formatted by import_* functions
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' raw(object)
setMethod("raw", "PlateData", function(x) x@raw)

#-------------------------------------------------------------------------------
# layout
#-------------------------------------------------------------------------------

#' Accessors for the 'layout' element of an PlateData object.
#'
#' @description 
#' The \code{layout} slot in an PlateData object holds
#' a data.frame containing information attributable to
#' individual wells. Contains one row per unique well.
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' layout(object)
setMethod("layout", "PlateData", function(x) x@layout)

#-------------------------------------------------------------------------------
# Column assignments
#-------------------------------------------------------------------------------

setMethod("value", "PlateData", function(x) x@value)
setMethod("time", "PlateData", function(x) x@time)
setMethod("treatment", "PlateData", function(x) x@treatment)
setMethod("predictors", "PlateData", function(x) x@predictors)

#-------------------------------------------------------------------------------
# combined
#-------------------------------------------------------------------------------

#' Accessors for the 'combined' element of an PlateData object.
#'
#' @description 
#' The \code{combined} slot in an PlateData object holds
#' a data.frame containing data combined from raw and layout
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' combined(object)
setMethod("combined", "PlateData", function(x) x@combined)
          
#-------------------------------------------------------------------------------
# blank
#-------------------------------------------------------------------------------

#' Accessors for the 'data' element of an PlateData object.
#'
#' @description 
#' The \code{blank} slot in an PlateData object holds
#' a data.frame containing blank measurements extracted from combined(object)
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' blank(object)
setMethod("blank", "PlateData", function(x) x@blank)

#-------------------------------------------------------------------------------
# control
#-------------------------------------------------------------------------------

#' Accessors for the 'control' element of an PlateData object.
#'
#' @description 
#' The \code{control} slot in an PlateData object holds
#' a data.frame containing blank measurements extracted from combined(object)
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' control(object)
setMethod("control", "PlateData", function(x) x@control)
          
#-------------------------------------------------------------------------------
# data
#-------------------------------------------------------------------------------

#' Accessors for the 'data' element of an PlateData object.
#'
#' @description 
#' The \code{data} slot in an PlateData object holds
#' a data.frame containing data (not blank or control) extracted from combined(object)
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' data(object)
setMethod("data", "PlateData", function(x) x@data)

#-------------------------------------------------------------------------------
# summary
#-------------------------------------------------------------------------------

#' Accessors for the 'summary' element of an PlateData object.
#'
#' @description 
#' The \code{summary} slot in an PlateData object holds
#' a data.frame containing summaries from data not attributable to wells
#' but grouped by 'Plate' and 'Hours'
#'
#' @author Oliver Dietrich
#' @export
#' 
#' @examples 
#' summary(object)
setMethod("summary", "PlateData", function(x) x@summary)

#-------------------------------------------------------------------------------
# type
#-------------------------------------------------------------------------------
setMethod("type", "PlateData", function(x) x@type)

#-------------------------------------------------------------------------------
# show
#-------------------------------------------------------------------------------

.pd_show <- function(object) {
    cat(is(object), "\n",
        "Total", nrow(layout(object)), "wells (see layout)", "\n",
        "across", length(type(object)), "plates (see type)", "\n",
        "measuring", nrow(data(object)), "data points", "\n", "\n"
       )
    cat(paste('Formula:', value(pd), '~', time(pd), '+', treatment(pd), '+', paste(predictors(pd), collapse = ' - ')))
    cat("\n\n")
    cat("layout", "\n")
    str(layout(object))
    cat("\n")
    cat("raw", "\n")
    str(raw(object))
    cat("\n")
    cat("combined", "\n")
    str(combined(object), max.level = 0)
    cat("\n")
    cat("blank", "\n")
    str(blank(object), max.level = 0)
    cat("\n")
    cat("control", "\n")
    str(control(object), max.level = 0)
    cat("\n")
    cat("data", "\n")
    str(data(object), max.level = 0)
    cat("\n")
    cat("summary", "\n")
    str(summary(object), max.level = 0)
    cat("\n")
    cat("type:", type(object))
}

#' @export 
setMethod("show", "PlateData", .pd_show)