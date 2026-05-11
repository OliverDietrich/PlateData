#' The PlateData Class
#'
#' The PlateData object contains measurements that are associated to a specific well in a microtiter plate.
#' Data from multiple plates can be stored together
#'
#' @slot data Data.frame associating measurements and metadata with well and plate indices
#' @slot layout Data.frame containing the plate layout
#' @slot key Character vector indicating the column to use as key between layout and data
#' @slot type Character vector indicating plate type
#' @slot misc A list of miscellaneous information
#' 
#' @exportClass PlateData
#'
methods::setClass(
  Class = "PlateData",
  slots = c(
    raw = "data.frame",
    layout = "data.frame",
    combined = "data.frame",
    blank = "data.frame",
    control = "data.frame",
    data = "data.frame",
    summary = "data.frame",
    type = "vector",
    misc = "list"
  )
)