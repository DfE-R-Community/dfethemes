.onLoad <- function(libname, pkgname) {
  cli_alert_info("setting default ggplot2 theme to {.val theme_nimbus()}")
  theme_set(theme_nimbus())

  cli_alert_info("Setting default ggplot2 colour scales to {.val scale_colour_dfe()}")

  options(
    ggplot2.continuous.colour = function() scale_colour_dfe(palette = "cool", discrete = FALSE),
    ggplot2.discrete.colour   = function() scale_colour_dfe(),
    ggplot2.continuous.fill   = function() scale_fill_dfe(palette = "cool", discrete = FALSE),
    ggplot2.discrete.fill     = function() scale_fill_dfe()
  )
}
