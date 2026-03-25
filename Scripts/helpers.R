# Axis config function allows clean arguments and formats in HTML to save a specification sheet with axis info. Intended to be used

set_axis <- function(p_cfg, axis = c("x", "y"), type = c("continuous", "discrete"), breaks = NULL, limits = NULL, label = NULL, unit = NULL) {
  axis <- match.arg(axis) # check if axis is x or y
  type <- match.arg(type) # check if axis type is continuous or discrete

  label <- if (!is.null(unit)) {
    paste0(
      label, "<br><span style='font-size:",
      p_cfg$text_size_primary,
      "pt; color:", p_cfg$axis_unit_color, "'>(", unit, ")</span>"
    )
  } else {
    label
  } # add label with formatted unit if unit is present, otherwise pass through label (whether populated or NULL).

  list(
    axis = axis,
    type = type,
    label = label,
    breaks = breaks,
    limits = limits
  )
}


# Axis builder function takes x and/or y axes of any type and constructs ggplot scale protoobjects with breaks and a name and a coord_cartesian object with limits.

construct_axes <- function(x = NULL, y = NULL) {
  specs <- list(x, y) # initialize list of axes
  scales <- list() # initialize empty scales list
  xlim <- NULL # initialize xlim var
  ylim <- NULL # initalize ylim var

  for (spec in specs) {
    if (is.null(spec)) next # skip loop to next if the axis is NULL

    construct_format <- switch(paste(spec$axis, spec$type, sep = "_"),
      x_continuous = scale_x_continuous,
      y_continuous = scale_y_continuous,
      x_discrete   = scale_x_discrete,
      y_discrete   = scale_y_discrete
    ) # switch to correct scale function call depending on specs

    if (!is.null(spec$limits) && spec$axis == "x") {
      xlim <- spec$limits
    } else if (!is.null(spec$limits) && spec$axis == "y") {
      ylim <- spec$limits
    } # overwrite xlim and ylim from NULL to limits if they are specified

    scales <- c(scales, construct_format(
      if (!is.null(spec$label)) name <- spec$label else NULL,
      if (!is.null(spec$breaks)) breaks <- spec$breaks else NULL
    )) # append ggplot scale object with breaks and name if present
  }

  if (!is.null(xlim) || !is.null(ylim)) {
    scales <- c(scales, coord_cartesian(xlim = xlim, ylim = ylim)) # append coord_cartesian object if any limits are provided
  }

  scales
}


# Color builder function ignores NULL items that are passed and constructs colors and fills with shared labels.

set_colors <- function(colors = NULL, fill_colors = NULL, labels = NULL, breaks = NULL) {
  color_scales <- list()

  if (!is.null(colors)) {
    color_scales <- c(color_scales, scale_color_manual(values = colors, labels = labels, breaks = breaks))
  }

  if (!is.null(fill_colors)) {
    color_scales <- c(color_scales, scale_fill_manual(values = fill_colors, labels = labels, breaks = breaks))
  }

  color_scales
}
