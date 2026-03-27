# Axis config function allows clean arguments and formats in HTML to save a specification sheet with axis info. Intended to be used

set_axis <- function(
  config = NULL,
  axis = c("x", "y"),
  type = c("continuous", "discrete"),
  breaks = NULL,
  limits = NULL,
  label = NULL,
  unit = NULL
) {
  axis <- match.arg(axis)
  type <- match.arg(type)

  # add label with formatted unit if unit is present, otherwise pass through label (whether populated or NULL).
  label <- if (!is.null(unit) && !is.null(theme)) {
    paste0(
      label,
      "<br><span style='font-size:",
      config$text_size_primary,
      "pt; color:",
      config$axis_unit_color,
      "'>(",
      unit,
      ")</span>"
    )
  } else if (!is.null(unit)) {
    paste0(
      label,
      "<br><span style='font-size:",
      "8",
      "pt; color:",
      "#4D4D4D",
      "'>(",
      unit,
      ")</span>"
    )
  } else {
    label
  }

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
  specs <- list(x, y)
  scales <- list()
  xlim <- NULL
  ylim <- NULL

  for (spec in specs) {
    # skip loop to next if the axis is NULL
    if (is.null(spec)) next

    # switch to correct scale function call depending on specs
    construct_format <- switch(paste(spec$axis, spec$type, sep = "_"),
      x_continuous = scale_x_continuous,
      y_continuous = scale_y_continuous,
      x_discrete   = scale_x_discrete,
      y_discrete   = scale_y_discrete
    )

    # overwrite xlim and ylim from NULL to limits if they are specified
    if (!is.null(spec$limits) && spec$axis == "x") {
      xlim <- spec$limits
    } else if (!is.null(spec$limits) && spec$axis == "y") {
      ylim <- spec$limits
    }

    # append ggplot scale object with breaks and name if present
    scales <- c(
      scales,
      construct_format(
        if (!is.null(spec$label)) name <- spec$label else NULL,
        if (!is.null(spec$breaks)) breaks <- spec$breaks else NULL
      )
    )
  }

  # append coord_cartesian object if any limits are provided
  if (!is.null(xlim) || !is.null(ylim)) {
    scales <- c(
      scales,
      coord_cartesian(xlim = xlim, ylim = ylim)
    )
  }

  scales
}


# Color builder function ignores NULL items that are passed and constructs colors and fills with shared labels.

set_colors <- function(
  colors = NULL,
  fill_colors = NULL,
  labels = NULL,
  breaks = NULL
) {
  color_scales <- list()

  if (!is.null(colors) && !is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_color_manual(
        values = colors,
        labels = labels,
        breaks = breaks
      )
    )
  }

  if (!is.null(fill_colors) && !is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_fill_manual(
        values = fill_colors,
        labels = labels,
        breaks = breaks
      )
    )
  }

  if (!is.null(colors) && is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_color_manual(
        values = colors,
        labels = labels
      )
    )
  }

  if (!is.null(fill_colors) && is.null(breaks)) {
    color_scales <- c(
      color_scales,
      scale_fill_manual(
        values = fill_colors,
        labels = labels
      )
    )
  }

  color_scales
}
