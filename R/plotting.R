#' Plot the plate layout
#'
#' @param object PlateData object
#' @param color Column used to color wells
#' @param facet_rows Number of rows to use for facet_wrap
#'
#' @export
#'
plot_plateLayout <- function(
    object = NULL,
    color = NULL,
    facet_rows = 1,
    pt.size = 8,
    theme.size = 20,
    pl.title = "Plate layout",
    xlab = NULL,
    ylab = NULL,
    pl.barwidth=1,
    pl.barheight=10,
    color.pal = NULL,
    color.pal.dir = 1,
    color.values = NULL,
    color.pal.cols = NULL
) {

  # Check input
  if (is.null(object)) {
    stop("Please provide a layout sheet to visualize.")
  } else if (class(object) != "PlateData") {
    stop("Object must be of type PlateData")
  }

  if (is.null(color)) {
    color <- "row"
  }
  if (!color %in% names(layout(object))) {
    stop(paste("The column name", color, "does not exist. Please check layout."))
  }

  # Wrangle data
  df <- data.frame(
    x = layout(object)$col,
    y = layout(object)$row,
    facet = layout(object)$plate,
    col = layout(object)[[color]]
  )

  # Create background
  bg <- dummyPlate(detect_largest_plate_type(layout(object)), plate_name = "bg")

  # Title
  if (is.null(pl.title)) {
    pl.title <- "Plate layout"
  }

  # Guides
  pals <- RColorBrewer::brewer.pal.info
  guides <- NULL
  colorscale <- NULL
  case_a <- class(df$col) %in% c("numeric", "logical")
  case_b <- class(df$col) %in% c("character", "factor", "boolean")
  c_len <- length(unique(df$col))
  if (case_a) {
    if (is.null(color.pal)) {
      color.pal <- 'Blues'
      } else if (!color.pal %in% row.names(pals[pals$category %in% c('div', 'seq'), ])) {
      message('The chosen color palette does not fit for numeric data. Defaulting to "Blues".')
      color.pal <- 'Blues'
    }
    guides <- ggplot2::guides(col = ggplot2::guide_colorbar(barwidth = pl.barwidth, barheight = pl.barheight))
    colorscale <- ggplot2::scale_color_distiller(palette = color.pal, direction = color.pal.dir)
  }
  if (case_b) {
    guides <- ggplot2::guides(col = ggplot2::guide_legend(ncol = color.pal.cols))
    if (all(unique(df$col) %in% names(color.values))) {
      color.values <- color.values[unique(df$col)]
    }
    if (length(color.values) == c_len) {
        colorscale <- ggplot2::scale_color_manual(values=color.values)
      } else if (is.null(color.pal)) {
        if (c_len <= 9) {
          colorscale <- ggplot2::scale_color_brewer(palette='Set1', direction=color.pal.dir)
          } else if (c_len <= 12) {
          colorscale <- ggplot2::scale_color_brewer(palette='Set3', direction=color.pal.dir)
          }
      } else if (color.pal %in% row.names(pals)) {
        colorscale <- ggplot2::scale_color_brewer(palette=color.pal, direction=color.pal.dir)
      }
  }
    
  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, col = col)) +
    ggplot2::geom_point(data=bg, ggplot2::aes(x=col, y=row),
                        col = "black", fill = "grey", shape = 21, size = 3) + # Background
    ggplot2::geom_point(size = pt.size) +
    ggplot2::facet_wrap(~facet, nrow = facet_rows) +
    ggplot2::theme_classic(theme.size) +
    ggplot2::scale_y_discrete(limits = rev(levels(bg$row))) +
    ggplot2::scale_x_discrete(limits = levels(bg$col)) +
    ggplot2::labs(title = pl.title, x = xlab, y = ylab, col = color) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5)
    ) +
    ggplot2::coord_fixed() +
    colorscale +
    guides

  return(p)
}

#' Plot curves
#'
#' Visualize PlateData as curves, i.e. values by time.
#' ...
#'
#' @param object PlateData object
#' @param slot Slot to get data from (data, summary)
#' @param type Whether to show 'raw', 'corrected', or 'normalised' data. Corrected data has blank subtracted. Normalised data is divided by control.
#' @param theme.size
#' @param legend.position
#' 
plot_curves <- function(object, 
                        slot = 'data',
                        type = 'raw',
                        theme.size = 20,
                        legend.position = 'top',
                        n.rows = NULL
                        ) {

  # Checks
  stopifnot(class(object) == 'PlateData')
  if (!slot %in% c('data','summary')) stop("Slot must be either 'data' or 'summary'. Check input!")
  if (!type %in% c('raw','corrected','normalised')) stop("Type must be either 'raw', 'corrected' or 'normalised'. Check input!")

  # Fetch data
  df <- slot(object, slot)

  # Define components
  if (type == 'raw') {
    gg_blank_error <- ggplot2::geom_ribbon(ggplot2::aes(y = blank, 
                                                        ymin = blank - sd_blank, 
                                                        ymax = blank + sd_blank), alpha = .25, fill = 'grey20')
    gg_blank <- ggplot2::geom_line(ggplot2::aes(y = blank))
    gg_control_error <- ggplot2::geom_ribbon(ggplot2::aes(y = control, 
                                                          ymin = control - sd_control, 
                                                          ymax = control + sd_control), alpha = .25, fill = 'grey20')
    gg_control <- ggplot2::geom_line(ggplot2::aes(y = control))
    if (slot == 'data') {
      gg_data_error <- NULL
      gg_data <- ggplot2::geom_line(ggplot2::aes(y = value, col = treatment, group = key))
    } else {
      gg_data_error <- ggplot2::geom_ribbon(ggplot2::aes(y = mean_value, 
                                                         ymin = mean_value - sd_value, 
                                                         ymax = mean_value + sd_value, 
                                                         fill = treatment), alpha = .25)
      gg_data <- ggplot2::geom_line(ggplot2::aes(y = median_value, 
                                                 col = treatment))
    }
    y_lab <- value(object)
  } else {
    gg_blank_error <- NULL
    gg_blank <- NULL
  }
  if (type == 'corrected') {
    gg_control_error <- ggplot2::geom_ribbon(ggplot2::aes(y = corrected_control, 
                                                          ymin = corrected_control - sd_corrected_control, 
                                                          ymax = corrected_control + sd_corrected_control), alpha = .25, fill = 'grey20')
    gg_control <- ggplot2::geom_line(ggplot2::aes(y = corrected_control))
    if (slot == 'data') {
      gg_data_error <- ggplot2::geom_ribbon(ggplot2::aes(y = corrected_value, 
                                                         ymin = corrected_value - sd_corrected_value, 
                                                         ymax = corrected_value + sd_corrected_value, 
                                                         fill = treatment, 
                                                         group = key), alpha = .25)
      gg_data <- ggplot2::geom_line(ggplot2::aes(y = corrected_value, col = treatment, group = key))
    } else {
      gg_data_error <- ggplot2::geom_ribbon(ggplot2::aes(y = corrected_mean_value, 
                                                         ymin = corrected_mean_value - sd_corrected_mean_value, 
                                                         ymax = corrected_mean_value + sd_corrected_mean_value, 
                                                         fill = treatment), alpha = .25)
      gg_data <- ggplot2::geom_line(ggplot2::aes(y = corrected_median_value,
                                                 col = treatment))
    }
    y_lab <- paste('Corrected', value(object))
  }
  if (type == 'normalised') {
    gg_control_error <- ggplot2::geom_ribbon(ggplot2::aes(y = 1, 
                                                          ymin = 1 - 1 * sqrt((sd_corrected_control/corrected_control)^2), 
                                                          ymax = 1 + 1 * sqrt((sd_corrected_control/corrected_control)^2)), alpha = .25, fill = 'grey20')
    gg_control <- ggplot2::geom_line(ggplot2::aes(y = 1))
    if (slot == 'data') {
      gg_data_error <- ggplot2::geom_ribbon(ggplot2::aes(y = normalised_value, 
                                                         ymin = normalised_value - sd_normalised_value, 
                                                         ymax = normalised_value + sd_normalised_value, 
                                                         fill = treatment, 
                                                         group = key), alpha = .25)
      gg_data <- ggplot2::geom_line(ggplot2::aes(y = normalised_value, col = treatment, group = key))
    } else {
      gg_data_error <- ggplot2::geom_ribbon(ggplot2::aes(y = normalised_mean_value,
                                                         ymin = normalised_mean_value - sd_normalised_mean_value, 
                                                         ymax = normalised_mean_value + sd_normalised_mean_value, 
                                                         fill = treatment), alpha = .25)
      gg_data <- ggplot2::geom_line(ggplot2::aes(y = normalised_median_value,
                                                 col = treatment))
    }
    y_lab <- paste('Normalised', value(object))
  }

  # Plot
  graph <- ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    # Blank
    gg_blank_error +
    gg_blank +
    # Control
    gg_control_error +
    gg_control +
    # Data
    gg_data_error +
    gg_data +
    ggplot2::facet_wrap(~ predictor, nrow = n.rows) +
    # Customize
    ggplot2::theme_bw(theme.size) +
    ggplot2::theme(
      legend.position = legend.position
      ) +
    ggplot2::labs(y = y_lab, x = time(pd), col = treatment(pd), fill = treatment(pd))

  # Exit
  return(graph)
}

#' Plot series data
#' 
#' Visualize numeric measurements stored in a PlateData object. 
#' Both x and y axes must be numeric. Lines will be drawn through grouped data points.
#' Can be faceted by categorical vectors. 
#' 
#' @param object PlateData object
#' @param slot Slot to get data from (combined, data, summary)
#' @param x Column used for y axis
#' @param y Column used for x axis
#' @param col Column name to color measurements by
#' @param group Column used to group measurements
#' @param facet Column used for faceting
#' 
#' @export
plot_series <- function(
  object,
    slot = 'combined',
    x = NULL,
    y = NULL,
    col = NULL,
    group = 'key',
    facet = "plate",
    facet_scales = "fixed",
    facet_rows = NULL,
    theme_size = 20,
    pl_title = "Series",
    y_transform = NULL,
    x_transform = NULL,
    xlab = x,
    ylab = y,
    pl.barwidth=1,
    pl.barheight=10,
    color.pal = "RdYlBu",
    color.pal.dir = -1,
    color.values = NULL,
    ...
) {
    
    stopifnot(
        class(object) == 'PlateData',
        slot %in% c('combined','data','summary')
    )
  
  # Fetch data
  df <- slot(object, slot)
  
  # Check presence and type of columns
  n <- 1
  cn <- c("x", "y", "group")
  for (i in c(x, y, group)) {
    j <- cn[[n]]
    if (!i %in% names(dat)) {
      stop(paste("The specified column", i, "for", j, "is not available."))
    } else { 
      df[[j]] <- dat[[i]]
    }
    n <- n+1
  }
  for (i in c(x, y)) {
    if (!class(dat[[i]]) %in% c("integer", "numeric")) {stop(paste0("Column", i, "must be numeric."))}
  }

  # Color
  if (is.null(col)) {
    df[["col"]] <- NaN
  } else if (col %in% names(dat)) {
    df[["col"]] <- dat[[col]]
  } else {
    stop(paste("The specified column", col, "does not exist."))
  }

  # Faceting
  if (stringr::str_detect(facet, '~')) {
    fc_grid <- stringr::str_split(facet, '~')[[1]]
    facet_x <- fc_grid[1]
    facet_y <- fc_grid[2]
    if (facet_x %in% names(dat)) {df$facet_x <- dat[[facet_x]]} else {stop("facet = x~y but x does not exist.")}
    if (facet_y %in% names(dat)) {df$facet_y <- dat[[facet_y]]} else {stop("facet = x~y but y does not exist.")}
    faceting <- ggplot2::facet_grid(facet_y~facet_x, scales = facet_scales)
    pl_subtitle <- paste('Split by x:', facet_x, 'and y:', facet_y)
  } else {
    if (facet %in% names(dat)) {df$facet <- dat[[facet]]} else {stop(paste("facet", facet, "does not exist."))}
    faceting <- ggplot2::facet_wrap(~facet, nrow = facet_rows, scales = facet_scales)
    pl_subtitle <- paste('Split by: ', facet)
  }
  
  # Theme
  theme <- ggplot2::theme_bw(base_size = theme_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle=45, hjust=1, vjust=1)
    )
  
  # Coordinate ranges and transformations
  def_scale_trans <- c("asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", 
  "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt", "time")
  if (!is.null(x_transform)) { # Level 1
    if (x_transform %in% def_scale_trans) { # Level 2
      x_scale <- ggplot2::scale_x_continuous(trans = x_transform)
    } else if (x_transform == "minmax") {
      df[[x]] <- minmax(df[[x]])
      x_scale <- NULL
    } else {
      warning(paste("The requested transformation", x_transform, "is not available and will be ignored."))
      x_scale <- NULL
    } # Level 2
  } else {
    x_scale <- NULL
  } # Level 1

  if (!is.null(y_transform)) { # Level 1
    if (y_transform %in% def_scale_trans) { # Level 2
      y_scale <- ggplot2::scale_y_continuous(trans = y_transform)
    } else if (y_transform == "minmax") {
      df <- dplyr::mutate(dplyr::group_by(df, .key), y = minmax(y))
      y_scale <- NULL
    } else {
      warning(paste("The requested transformation", y_transform, "is not available and will be ignored."))
      y_scale <- NULL
    } # Level 2
  } else {
    y_scale <- NULL
  } # Level 1

  # Guides
  guides <- NULL
  colorscale <- NULL
  case_a <- class(df$col) %in% c("numeric", "logical")
  case_b <- class(df$col) %in% c("character", "factor", "boolean")
  c_len <- length(unique(df$col))
  if (case_a) {
    guides <- ggplot2::guides(col = ggplot2::guide_colorbar(barwidth = pl.barwidth, barheight = pl.barheight))
    colorscale <- ggplot2::scale_color_distiller(palette = color.pal, direction = color.pal.dir)
  }
  if (case_b) {
    if (all(unique(df$col) %in% names(color.values))) {
      color.values <- color.values[unique(df$col)]
    }
    if (length(color.values) == length(unique(df$col))) {
        guides <- ggplot2::guides(col = ggplot2::guide_legend())
        colorscale <- ggplot2::scale_color_manual(values=color.values)
      }
  }

  # Re-order
  df <- df[order(df$col, decreasing=TRUE), ]

  # Plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(x, y, col = col)) +
    #ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(group = group)) +
    faceting +
    theme  +
    ggplot2::labs(title = pl_title, col = col, subtitle = pl_subtitle,
         x = xlab, y = ylab) + 
    x_scale + y_scale +
    guides + colorscale
  
  return(plot)
}