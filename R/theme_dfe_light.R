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

    new_theme <- theme(
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
        )


  if (base_axis == "x") {

    new_theme <- new_theme + theme(
      axis.line.y = element_blank(),
      axis.line.x = element_line(lineend = "square", linewidth = 1),

      panel.grid.major.y = element_line(colour = "#d9d9d9"),
      panel.grid.minor.y = element_line(colour = "#e6e6e6"),

      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  } else if (base_axis == "y") {
    new_theme <- new_theme + theme(
      axis.line.x = element_blank(),
      axis.line.y = element_line(lineend = "square", linewidth = 1),

      panel.grid.major.x = element_line(colour = "#d9d9d9"),
      panel.grid.minor.x = element_line(colour = "#e6e6e6"),

      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
      )
  } else if (base_axis == "both") {
      new_theme  <- new_theme + theme(
        axis.line.y = element_line(lineend = "square", linewidth = 1),

        panel.grid.major.y = element_line(colour = "#d9d9d9"),
        panel.grid.minor.y = element_line(colour = "#e6e6e6"),

        panel.grid.major.x = element_line(colour = "#d9d9d9"),
        panel.grid.minor.x = element_line(colour = "#e6e6e6"),
      )
    } else {
      cli::cli_abort(c("{.val {base_axis}} is not a valid argument",
                       i = "'x', 'y' or 'both' are valid arguments"))

    }

}
