#' Import plate layout
#'
#' Import plate layout from excel file
#'
#' @param file_path Location of file
#' @param well_key Column indicating wells in microtiter plate
#' @param plate_name Name of the plate the layout is associated with
#'
#' @export
#'
import_plateLayout <- function(
    file_path = NULL
) {
    stopifnot(
        !is.null(file_path),
        file.exists(file_path)
    )
    
    # Read data
    object <- as.data.frame(readxl::read_excel(file_path))
    
    ind <- stringr::str_to_lower(names(object)) == 'well'
    if (sum(ind) < 1) stop('Layout must contain column "well".')
    if (sum(ind) > 1) stop('Layout must contain only one column "well". Found more than one...')
    
    # Handle uppercase 'Well' by (re-)assigning vector to lowercase 'well'
    object$well <- object[[which(ind)]]
    object$Well <- NULL
    
    # Remove empty rows
    object <- object[!is.na(object$well), ]
    
    return(object)
}

#' Import data from 

#' Import data from Tekan Spark
#'
#' Import plate reader data from an excel sheet using the formatting by
#' Tekan's Spark.
#'
#' @param file_path Location of file (must be xls or xlsx)
#'
#' @returns Tidy data frame with six columns (cycle, time, temp, well, count, mode)
#' @export
#'
import_tekanSpark <- function(
    file_path = NULL,
    plate_name = 'unspecified'
) {

  stopifnot(
    file.exists(file_path)
  )

  # Check input type
  if (!any(endsWith(file_path, c("xlsx", "xls")))) {
    stop("Unknown data format. Please specify excel file (.xlsx)")
  }

  # Read data
  object <- suppressMessages(as.data.frame(readxl::read_excel(file_path, col_names = FALSE)))

  # Re-format data
  data <- list()
  index <- which(stringr::str_detect(object[[1]], "Label"))
  names(index) <- paste("Label", 1:length(index), sep = "_")
  for (ind in names(index)) {

    # Set index
    i <- index[[ind]]

    # Extract measurement modes
    name <- object[[1]][i]
    mode <- object[[2]][which(stringr::str_detect(object[[2]], name))-1]

    # Detect data frame
    top <- which(object[[1]] == name) + 1
    na_vert <- which(is.na(object[[1]]))
    bottom <- min(na_vert[na_vert > top]) - 1

    # Extract data frame
    df <- object[top:bottom, ]
    col_names <- as.character(df[, 1])
    col_names[1:3] <- c("cycle", "time", "temp")
    df <- as.data.frame(t(df)) # Note: if colnames are not extracted above, formatting errors might occur (R 3.5.1)
    df <- df[-1, ]
    colnames(df) <- col_names

    # Re-format with tidyr::gather throws error in Jupyter !!!

    # Add measurement mode
    df$mode <- mode

    # Add to list
    data[[ind]] <- df
    rm(df)
  }

  # Concatenate lists
  #data <- dplyr::bind_rows(data)
  data <- do.call("rbind", data)

  # Re-format
  data <- tidyr::gather(data, "well", "count", -cycle, -time, -temp, -mode)

  # Adjust data types
  for (i in c("time", "temp", "count")) {
    data[[i]] <- as.numeric(as.character(data[[i]]))
  }
  data[["time"]] <- data[["time"]] / 60 / 60
  for (i in c("well", "mode", "cycle")) {
    data[[i]] <- as.character(data[[i]])
  }

  # Add plate column and remove components
  data$plate <- plate_name
  data$key <- paste0(data$plate, "_", data$well)
  data <- data[, -which(names(data) %in% c("plate", "well"))]

  return(data)
}

#' Import data from Elecsys
#' 
#' @param file_path Path to file
#' @param run_name Name of run
#' @param sep Separator used in CSV
#' @param plate_well Column storing plate and well, separated by underscore (_)
#' 
import_elecsys <- function(
  file_path = NULL,
  run_name = 'unspecified',
  sep = ';',
  plate_well = 'PID'
) {
  stopifnot(
    !is.null(file_path)
  )

  # Read data
  object <- read.table(file_path, sep = sep, header=TRUE)

  # Add file and run name
  object[['file_name']] <- basename(file_path)
  object[['run_name']] <- run_name

  # Adjust antibody names
  object[['antibody']] <- object$PSET
  ind <- stringr::str_detect(object$antibody, "-")
  if (sum(ind) >= 1) {
    object$antibody[ind] <- stringr::str_split(object$antibody[ind], "-", simplify=TRUE)[,2]
  }

  # Convert MM to Dilution
  lookup <- c("0"=0, "0_01"=1e-2, "0_1"=1e-1, "1"=1e-0, "10"=1e1, "100"=1e2, "1000"=1e3, "10000"=1e4)
  object$Dilution <- as.numeric(lookup[object$MM])
  
  # Convert SROH to Signal
  object$Signal <- object[["SROH"]]
  object$Signal_log10 <- log10(object$Signal)

  # Create PlateData specific columns
  x <- stringr::str_split(object[[plate_well]], '_', simplify=TRUE) 
  object[['plate']] <- x[, 1]
  object[['well']] <- x[, 2]
  object <- mutate_well_to_row_col_indices(object)

  # Add custom row.names
  object[['.index']] <- object[[plate_well]]
  object <- mutate(group_by(object, .index), measurement_number = 1:length(.index))
  object[['.index']] <- NULL
  object[['measurement']] <- paste0(object[[plate_well]], '_', object[['measurement_number']])

  return(object)
}

#' Import concentration measurements
#' 
#' Import ELISA measurements for antibodies screened via Elecsys.
#' Not well standardized excel spreadsheet with rectangular plate-like shape containing
#' the antibody names (e.g. 23.068-63H4-IgG) and IgG concentration in ng/mL.
#' 
#' @param file_path File path
#' 
#' @return Data.frame
#' 
#' @export 
#' 
import_elecsys_concentrations <- function(
  file_path = NULL,
  plate_name = NULL
) {

  stopifnot(
    !is.null(file_path)
  )
  if (is.null(plate_name)) {
    stop('Please specify the plate_name.')
  } else if (class(plate_name) != 'character' & length(plate_name) == 1) {
    stop('Wrong type of plate_name. Please supply a character vector of length 1.')
  }

  df <- suppressMessages(
    read_excel(file_path, col_names = FALSE)
  )

  # Detect plate borders
  tl <- which(str_detect(df[[1]], "A")) - 1
  bl <- which(str_detect(df[[1]], "H"))

  # Extract plate layout
  l <- df[tl[1]:bl[1], ]
  names(l) <- c("row", 2:ncol(l)-1)
  l <- l[-1, ]
  l <- gather(l, "col", "antibody", -row)

  # Extract concentrations
  c <- df[tl[2]:bl[2], ]
  names(c) <- c("row", 2:ncol(c)-1)
  c <- c[-1, ]
  c <- gather(c, "col", "concentration", -row)

  # Combine
  df <- merge(l, c)
  df <- df[!is.na(df$antibody) & df$antibody != 'empty', ] # remove empty
  df <- df[!is.na(df$concentration) & df$concentration != 'empty', ] # remove empty

  # Adjust antibody names
  df$antibody <- stringr::str_split(df$antibody, "-", simplify=TRUE)[,2]

  # Adjust vector types
  df[['concentration']] <- as.numeric(df[['concentration']])

  # Create PlateData specific columns
  df[['plate']] <- plate_name
  df[['well']] <- paste0(df$row, df$col)

  return(df)
}

#' Import Biacore
#' 
#' @param file_path
#' 
#' @return data.frame
#' 
#' @export 
#' 
#' 
import_biacore <- function(
  file_path = NULL
) {

  stopifnot(
    !is.null(file_path)
  )

  # Read from excel file
  object <- readxl::read_excel(file_path)

  # Adjust colnames
  names(object) <- paste0("biacore_", names(object))

  # Remove NAs
  object <- object[!is.na(object$biacore_Barcode), ]

  # Adjust antibody name
  object$antibody <- stringr::str_split(object[["biacore_L N"]], "-", simplify=TRUE)[,2]

  # Create PlateData specific columns
  object$plate <- stringr::str_split(object$biacore_Barcode, "_", simplify=TRUE)[,1]
  object$well <- object[["biacore_Well Nr."]]
  object$row <- stringr::str_split_fixed(object$well, "", 2)[,1]
  object$col <- stringr::str_split_fixed(object$well, "", 2)[,2]

  return(object)
}