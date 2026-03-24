# Plot config can be used to avoid having to readjust repetitive plot arguments and easily reuse or adjust plot arguments.
# The universal_config is applied to all plots.
# The second level is a config for the output type to allow for toggling between poster and paper configs within the same script and not having to have separate scripts for posters. This is automatically merged with universal_config when the p_config is built.
# Whole ggplot theme() objects can also be stored.
# It's possible to store both global defaults (that apply to most plots) with overrides for a plot. E.g., p_config$text_themes may contain axis defaults, whereas p_config$fig1$text_themes may contain text themes specific to fig1.

p_config <- list(
  universal_config = list(
    scale_labels = function(x) {
      sapply(x, function(val) {
        if (val == floor(val)) {
          as.character(val)
        } else {
          format(val, nsmall = 0)
        }
      })
    },
    universal_theme = theme_few() + theme(
      legend.title = element_blank(),
      legend.position = "inside",
      legend.justification.inside = c(0, 1),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.key.height = unit(2, "lines"),
      panel.border = element_part_rect(
        colour = "grey30",
        side = "lb",
        linewidth = 0.5
      )
    ),
    bioage = list(
      color_scales = 
        scale_color_manual(
          values = 
            if (isTRUE(params$telolength)) {
              c("age" = "#000000",
                "age, hd_log" = "#256DAB",
                "age, kdm" = "#059748",
                "age, phenoage" = "#CB2027",
                "age, telolength" = "#7B3A96")
            } else {
              c("age" = "#000000",
                "age, hd_log" = "#256DAB",
                "age, kdm" = "#059748",
                "age, phenoage" = "#CB2027")
            },
          labels = 
            if (isTRUE(params$telolength)) {
              c("Age", "Age + HD", "Age + KDM", "Age + PhenoAge", "Age + Telolength")
            } else {
              c("Age", "Age + HD", "Age + KDM", "Age + PhenoAge")
            }
        ), 
      fill_scales = 
        scale_fill_manual(
          values = 
            if (isTRUE(params$telolength)) {
              c("age" = "#8C8C8C",
                "age, hd_log" = "#88BDE6",
                "age, kdm" = "#90CD97",
                "age, phenoage" = "#F07E6E",
                "age, telolength" = "#B276B2")
            } else {
              c("age" = "#8C8C8C",
                "age, hd_log" = "#88BDE6",
                "age, kdm" = "#90CD97",
                "age, phenoage" = "#F07E6E")
            },
          labels = 
            if (isTRUE(params$telolength)) {
              c("Age", "Age + HD", "Age + KDM", "Age + PhenoAge", "Age + Telolength")
            } else {
              c("Age", "Age + HD", "Age + KDM", "Age + PhenoAge")
            }
        )
    )
  ),
  
  paper = list(
    base_path = here::here(
      "Output",
      "Figures",
      if (isTRUE(params$telolength))
        paste0(params$hearing_var, "_telolength")
      else
        params$hearing_var
    ),
    legend_guide = 
      guides(
        colour = guide_legend(
          override.aes = list(size = 1.25),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm"),
          label.theme = element_markdown(
            "sans",
            size = 9,
            margin = margin(l = 3, t = 1.5, unit = "pt")
          ),
        )
      ),
    base_text_size = 8,
    signif_size = 14,
    text_theme = theme(
      axis.text.x = element_markdown("sans", size = 8),
      axis.text.y = element_markdown("sans", size = 8)
    ), 
    title_theme = theme(
      axis.title.x = element_markdown("sans", size = 9, lineheight = 1.1),
      axis.title.y = element_markdown("sans", size = 9, lineheight = 1.1)
    ),
    age_rate = list(
      labels = labs(
        y = paste0(
          if (params$hearing_var == "FI_high") "High" else "Low",
          " Fletcher Index<br><span style='font-size:8pt; color:#4D4D4D'>(dB HL)</span>"
          )
        ),
      der_labels = labs(
        x = "Age<br><span style='font-size:8pt; color:#4D4D4D'>(Years)</span>", 
        y = "Rate of HL<br><span style='font-size:8pt; color:#4D4D4D'>(dB HL/Year)</span>"
        )
      ),
    bioage = list(
      labels = labs(
        x = "Age Group<br><span style='font-size:8pt; color:#4D4D4D'>(Years)</span>",
        y = "Additional Predictive Capacity<br><span style='font-size:8pt; color:#4D4D4D'>(Δ MSE (dB HL)<sup>2</sup>)</span>"
        )
      ),
    advance_bioage = list(
      labels = labs(
        x = "Age Acceleration<br><span style='font-size:8pt; color:#4D4D4D'>(PhenoAge – Age (Years))</span>", 
        y = "Age Group<br><span style='font-size:8pt; color:#4D4D4D'>(Years)</span>"
        )
      )
    ),
  
  poster = list()
)
