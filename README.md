
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/DfE-R-Community/ggdfe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DfE-R-Community/ggdfe/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# ggdfe

This package seeks to create an easy theme to add to any ggplot to keep
it in line with DfE brand guidelines. Simply transforms existing ggplots
into the house style of the Department for Education.

<img src="man/figures/README-unnamed-chunk-3-1.png" width="90%" />

## Colours

There are two functions that work to add in colour scales:
`scale_fill_dfe()` and `scale_colour_dfe()`. These replace the usual
`scale_colour_*()` and `scale_fill_*()` functions to change colours on
ggplot.

These are the base colours

<<<<<<< HEAD
<<<<<<< HEAD
<img src="man/figures/README-unnamed-chunk-4-1.png" width="80%" />
=======
<img src="man/figures/README-unnamed-chunk-3-1.png" width="80%" />
>>>>>>> b283726 (Docs - updating the readme with themes and higher dpi)
=======
<img src="man/figures/README-unnamed-chunk-4-1.png" width="80%" />
>>>>>>> 5ba710e (Feat / Docs - altering theme, adding a plot to readme)

There are also palettes, passed to the ‘palettes’ argument. These are
for selecting a suitable subset of the colours above, depending on if
you need a discrete, continuous or diverging palette.

- main - Blue red turquoise (discrete, 3 colours)
- warm - Red to purple (continuous)
- cool - Blue to lime (continuous)
- full - all colours in the brand (discrete, 7 colours)
- likert - red to blue, grey midpoint (diverging)
- likert2 - green to purple, white midpoint (diverging)
- likert3 - red to blue, white midpoint (diverging)
- heat - white to red (continuous)
- heat2 - white to pink (continuous)
- cold - white to blue (continuous)
- cold2 - white to turqouise (continuous)

<<<<<<< HEAD
<<<<<<< HEAD
![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->
=======
![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->
>>>>>>> b283726 (Docs - updating the readme with themes and higher dpi)
=======
![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->
>>>>>>> 5ba710e (Feat / Docs - altering theme, adding a plot to readme)

Examples of usage would be:

``` r
ggplot2::ggplot(data = iris)+
  ggplot2::aes(x=Sepal.Length, colour = Species)+
  ggplot2::geom_density(linewidth = 1.5)+
  scale_colour_dfe(palette = "main")+
  labs(title = "Default title is in dark blue",
       subtitle = "Default subtitle and copy text are dark grey",
       caption = "Using theme_dfe_light()")+
  theme_dfe_light()
```

<<<<<<< HEAD
<<<<<<< HEAD
<img src="man/figures/README-unnamed-chunk-6-1.png" width="90%" />
=======
<img src="man/figures/README-unnamed-chunk-5-1.png" width="90%" />
>>>>>>> b283726 (Docs - updating the readme with themes and higher dpi)
=======
<img src="man/figures/README-unnamed-chunk-6-1.png" width="90%" />
>>>>>>> 5ba710e (Feat / Docs - altering theme, adding a plot to readme)

For clearer plot boundaries, there is also `theme_nimbus()`.

``` r
plot <- ggplot(ggplot2::diamonds, aes(carat, price)) + geom_point() +
  labs(title = "Theme_nimbus",
       subtitle = "",
       caption = "theme_nimbus()")

plot + theme_nimbus()
```

<<<<<<< HEAD
<<<<<<< HEAD
<img src="man/figures/README-unnamed-chunk-7-1.png" width="90%" />
=======
<img src="man/figures/README-unnamed-chunk-6-1.png" width="90%" />
>>>>>>> b283726 (Docs - updating the readme with themes and higher dpi)
=======
<img src="man/figures/README-unnamed-chunk-7-1.png" width="90%" />
>>>>>>> 5ba710e (Feat / Docs - altering theme, adding a plot to readme)
