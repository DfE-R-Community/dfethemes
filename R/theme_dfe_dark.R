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
theme_dfe_dark <- function() {

  theme_minimal()  %+replace%  theme(
    text = element_text(colour = "#b3b3b3", size = 14, face = "bold"),


    axis.title.y = element_text(hjust = 0, angle = 90, vjust = 1.5),
    axis.title.x = element_text(hjust = 0, angle = 0),
    axis.text = element_text(colour = dfe_colours["blue_l"]),
    axis.ticks = element_line(colour = dfe_colours["blue_l"]),
    axis.line = element_line(colour = dfe_colours["blue_l"]),
    axis.line.y = element_blank(),
    axis.line.x = element_line(lineend = "square", linewidth = 1),

    panel.background = element_rect(fill = dfe_colours["Blue"], colour = dfe_colours["Blue"]),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = dfe_colours["blue_l"]),
    panel.grid.minor.y = element_line(colour = dfe_colours["blue_l"]),

    strip.background = element_rect(fill = "#183860",
                                    colour = "#b3b3b3", linewidth = 1),

    plot.title = element_text(face = "bold", colour = "#A3AFBF", hjust = 0, size = 21, margin = margin(7, 7, 7, 7)),
    legend.position = "top",
    legend.key = element_rect(fill = "#183860", colour = "#183860"),
    plot.margin = margin(5, 4, 4, 3),
    plot.background = element_rect(fill = dfe_colours["Blue"], colour = dfe_colours["Blue"])
  )
}
