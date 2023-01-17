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
    plot.subtitle      = element_text(size = 14,hjust = 0, margin = margin(b = 3)),
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
