########################################
# Accessors for PlateData slots

# There are only getter functions for PlateData slots,
# because raw data ('raw') and layout need to be available for creating the object
# and all other slots will be filled internally.

#' @export
setGeneric("raw", function(x) standardGeneric("raw"))

#' @export
setGeneric("layout", function(x) standardGeneric("layout"))

#' @export
setGeneric("value", function(x) standardGeneric("value"))

#' @export
setGeneric("time", function(x) standardGeneric("time"))

#' @export
setGeneric("treatment", function(x) standardGeneric("treatment"))

#' @export
setGeneric("predictors", function(x) standardGeneric("predictors"))

#' @export
setGeneric("combined", function(x) standardGeneric("combined"))

#' @export
setGeneric("blank", function(x) standardGeneric("blank"))

#' @export
setGeneric("control", function(x) standardGeneric("control"))
           
#' @export
setGeneric("data", function(x) standardGeneric("data"))

#' @export
setGeneric("summary", function(x) standardGeneric("summary"))

#' @export
setGeneric("type", function(x) standardGeneric("type"))