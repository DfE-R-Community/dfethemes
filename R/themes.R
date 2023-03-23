#' theme_dfe_light
#'
#' @return A light ggplot2 DfE theme
#' @export
#' @name themes
#' @param base_axis sets the orientation of the base and grid lines (defaults `x`)
#'
#' @return Use `base_axis` to switch between horizontal and vertical gridlines.
#' Use `y` for horizontal bar charts or `both` for all gridlines
#'
#' @examples
#' library(ggplot2)
#'
#' plot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species))+
#' geom_jitter()+
#' scale_colour_dfe()+
#'   labs(title = "This is the title",
#'          subtitle = "This is a subtitle",
#'                 caption = "This is a caption | Source: Iris dataset")
#'
#' plot + theme_dfe_light()
#'
#' plot <- ggplot(data = mtcars, aes(x=cyl, y=hp, fill = cyl))+
#'   geom_col()+
#'   coord_flip()+
#'   theme_dfe_light(base_axis = "y")
#'
#'plot
#'
theme_dfe_light <- function(base_axis = "x") {

  light_axis <- c("major" = "#d9d9d9",
                  "minor" = "#e6e6e6")

  theme(
    text = element_text(colour = "#4d4d4d", size = 14),


    axis.title.y = element_text(hjust = 0, angle = 90, margin = margin(0, 12, 0, 0)),
    axis.title.x = element_text(hjust = 0, angle = 0),
    axis.text = element_text(colour = "#4d4d4d"),
    axis.ticks = element_line(colour = "#b3b3b3"),
    axis.line = element_line(colour = "#b3b3b3"),

    panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),

    strip.background = element_rect(fill = "#FFFFFF",
                                    colour = "#b3b3b3", linewidth = 1),

    plot.title = element_text(face = "bold",
                              colour = dfe_colours["Blue"],
                              hjust = 0,
                              size = 21,
                              margin = margin(7, 7, 7, 7)),

    legend.position = "top",
    legend.key = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
    plot.margin = margin(5, 4, 4, 3)
  ) + axis_swapper(base_axis, light_axis)

}


#' theme_dfe_dark
#'
#' @return A dark ggplot2 DfE theme
#' @export
#' @name themes
#'
#' @examples
#' library(ggplot2)
#'
#' plot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species))+
#' geom_jitter()+
#' scale_colour_dfe(palette = 'likert')+
#'   labs(title = "This is the title",
#'          subtitle = "This is a subtitle",
#'                 caption = "This is a caption | Source: Iris dataset")
#' plot + theme_dfe_dark()
#'
theme_dfe_dark <- function(base_axis = "x") {

  dark_axis <- c("major" = unname(dfe_colours["blue_l"]),
                 "minor" = unname(dfe_colours["blue_l"]))

 theme(
    text = element_text(colour = "#b3b3b3", size = 14, face = "bold"),


    axis.title.y = element_text(hjust = 0, angle = 90, margin = margin(0, 12, 0, 0)),
    axis.title.x = element_text(hjust = 0, angle = 0),
    axis.text = element_text(colour = dfe_colours["blue_l"]),
    axis.ticks = element_line(colour = dfe_colours["blue_l"]),
    axis.line = element_line(colour = dfe_colours["blue_l"]),
    axis.line.y = element_blank(),
    axis.line.x = element_line(lineend = "square", linewidth = 1),

    panel.background = element_rect(fill = dfe_colours["Blue"], colour = dfe_colours["Blue"]),

    strip.background = element_rect(fill = "#183860",
                                    colour = "#b3b3b3", linewidth = 1),

    plot.title = element_text(face = "bold",
                              colour = "#A3AFBF",
                              hjust = 0,
                              size = 21,
                              margin = margin(7, 7, 7, 7)),

    legend.position = "top",
    legend.key = element_rect(fill = "#183860", colour = "#183860"),
    legend.background = element_rect(fill = "#183860", colour = dfe_colours["blue_l"]),

    plot.margin = margin(5, 4, 4, 3),
    plot.background = element_rect(fill = dfe_colours["Blue"], colour = dfe_colours["Blue"])
  ) + axis_swapper(base_axis, dark_axis)

}





#' DfE Plot Themes
#'
#' @param base_size,base_family,base_line_size,base_rect_size See
#'   `ggplot2::theme_grey()` for details
#' @param all_gridlines Set `TRUE` to show all panel gridlines - by default,
#'   the vertical gridlines will not be shown.
#'
#' @return A ggplot2 plot theme
#' @export
#'
#' @name themes
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(diamonds, aes(carat, price)) + geom_point()
#' p2 <- p1 + facet_wrap(~cut)
#'
#' p1 + theme_nimbus() + ggtitle("theme_nimbus()")
#' p2 + theme_nimbus() + ggtitle("theme_nimbus()")
theme_nimbus <- function(base_size = 11,
                         base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22,
                         all_gridlines = FALSE) {

  theme_minimal(

    base_size      = base_size,
    base_family    = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size

  ) %+replace% theme(

    text             = element_text(colour = "#01091F"),

    legend.position    = "bottom",
    strip.background   = element_rect("#B8BFC3", NA),
    panel.background   = element_rect("white", NA),
    panel.border       = element_rect(NA, "#B8BFC3"),
    plot.background    = element_rect("#dfe5e8", NA),
    plot.margin        = margin(10, 5.5, 5.5, 5.5),
    axis.title.x       = element_text(size = 14, margin = margin(t = base_size / 2)),
    axis.title.y       = element_text(size = 14, angle = 90, margin = margin(r = base_size / 4), vjust = 1),
    axis.text          = element_text(colour = "#1E2F35", size = rel(0.8)),
    plot.title         = element_text(size = 22, hjust = 0, margin = margin(b = 5)),
    plot.subtitle      = element_text(size = 14, hjust = 0, margin = margin(b = 3)),
    axis.ticks         = element_line("#B8BFC3"),
    panel.grid.major.x = if (!all_gridlines) element_blank(),
    panel.grid.minor.x = if (!all_gridlines) element_blank(),
    strip.text         = element_text(
      size = rel(0.8),
      margin = margin(0.4 * base_size, 0.4 * base_size, 0.4 * base_size, 0.4 * base_size)
    ),
    complete           = TRUE
  )

}




#unexported function to swap the gridlines on axes
axis_swapper <- function(axis_switch, axis_colours) {

  if (axis_switch == "x") {

    theme(
      axis.line.y = element_blank(),
      axis.line.x = element_line(lineend = "square", linewidth = 1),

      panel.grid.major.y = element_line(colour = axis_colours["major"], linewidth = 1),
      panel.grid.minor.y = element_line(colour = axis_colours["minor"]),

      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  } else if (axis_switch == "y") {
    theme(
      axis.line.x = element_blank(),
      axis.line.y = element_line(lineend = "square", linewidth = 1),

      panel.grid.major.x = element_line(colour = axis_colours["major"], linewidth = 1),
      panel.grid.minor.x = element_line(colour = axis_colours["minor"]),

      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  } else if (axis_switch == "both") {
    theme(
      axis.line.y = element_line(lineend = "square", linewidth = 1),

      panel.grid.major.y = element_line(colour = axis_colours["major"], linewidth = 1),
      panel.grid.minor.y = element_line(colour = axis_colours["minor"]),

      panel.grid.major.x = element_line(colour = axis_colours["major"], linewidth = 1),
      panel.grid.minor.x = element_line(colour = axis_colours["minor"]),
    )
  } else {
    cli::cli_abort(c("{.val {axis_switch}} is not a valid argument",
                     i = "'x', 'y' or 'both' are valid arguments"))
  }
}
