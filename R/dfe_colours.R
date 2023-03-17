
dfe_colours <- c(
  "Blue" = "#183860",
  "Red" = "#EB5C5D",
  "Turquoise" = "#2BBAD9",
  "Lime" = "#A3D55F",
  "Pink" = "#DF7CB0",
  "Purple" = "#774B99",
  "Yellow" = "#f6da40",
  "White" = "#ffffff",
  "Grey" = "#e5e5e5",
  "blue_l" = "#7488A0",
  "red_l" = "#F39D9E",
  "turq_l" = "#80D6E8",
  "lime_l" = "#C8E69F",
  "pink_l" = "#ECB0D0",
  "purp_l" = "#AD93C2"
  )


# This just selects colours based on the names - used to create palettes below
dfe_cols <- function(...) {

  cols <- c(...)

  if (is.null(cols)) {
    return(dfe_colours)
  }

  dfe_colours[cols]
}

# List of palettes for various types
dfe_palettes <- list(
  "main" = dfe_cols("Blue", "Red", "Turquoise"),
  "warm" = dfe_cols("Red", "Pink", "Purple"),
  "cool" = dfe_cols("Blue", "Turquoise", "Lime"),
  "full" = dfe_cols("Red", "Pink", "Yellow", "Lime", "Turquoise", "Blue", "Purple"),
  "likert" = dfe_cols("Red", "Grey", "Turquoise"),
  "likert2" = dfe_cols("Lime", "White", "Purple"),
  "likert3" = dfe_cols("Red", "White", "Blue"),
  "heat" = dfe_cols("White", "Red"),
  "heat2" = dfe_cols("White", "Pink"),
  "cold" = dfe_cols("White", "Blue"),
  "cold2" = dfe_cols("White", "Turquoise"),
  "pale" = dfe_cols("blue_l", "red_l", "turq_l", "lime_l", "pink_l", "purp_l")
)

# Function to create the palette
dfe_pal <- function(palette = "main", reverse = FALSE, ...) {

  pal <- dfe_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  grDevices::colorRampPalette(pal, ...)

}

#' Add colour and fill scales to ggplot with DfE theme
#'
#' @param palette The colour palette to use
#' @param discrete Is the scale discrete (defaults `TRUE`)
#' @param reverse Reverse the order of the scale (defaults `FALSE`)
#' @param ... arguments passed onto `discrete_scale()` or `scale_fill_gradientn()`
#'
#' @return These are the palettes in their current form
#'  - `main` - Blue red turquoise (discrete, 3 colours)
#'  - `warm` - Red to purple (continuous)
#'  - `cool` - Blue to lime (continuous)
#'  - `full` - all colours in the brand (discrete, 7 colours)
#'  - `likert` - red to blue, grey midpoint (diverging)
#'  - `likert2` - green to purple, white midpoint (diverging)
#'  - `likert3` -  red to blue, white midpoint (diverging)
#'  - `heat` - white to red (continuous)
#'  - `heat2` - white to pink (continuous)
#'  - `cold` - white to blue (continuous)
#'  - `cold2` - white to turqouise (continuous)
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#'
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, colour = Species)) +
#'   geom_point() +
#'   scale_colour_dfe()
#'
#' ggplot(iris, aes(x = Petal.Length, y = Sepal.Length,colour = Sepal.Width)) +
#'   geom_point(size = 3, alpha = 3/4) +
#'   scale_colour_dfe(palette = "heat", discrete = FALSE)
#'
#' ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
#'   geom_col() +
#'   scale_fill_dfe()
#'
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_dfe(discrete = FALSE, palette = "heat")
scale_colour_dfe <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

  pal_check(palette)

  pal <- dfe_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("dfe_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colours = pal(256))
  }

}




#' @describeIn scale_colour_dfe Add fill scale to ggplot with DfE theme
#' @export
scale_fill_dfe <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

    pal_check(palette)

    pal <- dfe_pal(palette = palette, reverse = reverse)

    if (discrete) {
      discrete_scale("fill", paste0("dfe_", palette), palette = pal, ...)
    } else {
      scale_fill_gradientn(colours = pal(256), ...)
    }
}



# This should be an enxported function at the moment, but it could be developed
# in the future Plays the same role as `Rcolorbrewer::display.brewer.all()` but
# in ggplot You can also pass individual palettes such as `dfe_palettes["full"]`
show_dfe_palettes <- function(palette = dfe_palettes) {

  output <- data.frame()
  pal_colour <-  NULL
  ord <-  NULL

  for (i in seq_along(palette)) {

    name <- names(palette[i])
    pal <-  palette[[i]]

    for (i in seq_along(pal)) {
      output <- rbind(
        output,
        c(name = name, pal_colour = pal[i], ord = i)
      )
    }
  }

  colnames(output)[1:3] <- c("name", "pal_colour", "ord")

  cols <- levels(factor(output$pal_colour))


  ggplot(
    output,
    aes(
      ord, stats::reorder(name, ord, FUN = max), fill = pal_colour
    )
  ) +
    geom_tile(height = .8, width = .8, colour = "#e5e5e5", linewidth = .5) +
    scale_fill_manual(values = cols) +
    coord_fixed(ratio = 9 / 16) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      legend.position = "none"
    )
}



#this checks to see if a palette is valid
pal_check <- function(palette, palette_set = dfe_palettes) {

  if (!palette %in% names(palette_set)) {

    cli::cli_abort(c(
      "{.val {palette}} is not a valid palette",
      i = "valid palettes are {.val {names(palette_set)}}"
    ))
  }
}
