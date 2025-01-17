# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#install.packages("highcharter")

#' CPUB theme for highcharts
#'
#' Our theme, Based on Highcharter Tufte theme.
#'
#' @param ... Named argument to modify the theme
#'
#' @examples
#'
#' highcharts_demo() %>%
#'   hc_add_theme(hc_theme_ctzn2())
#'
#' @export
hc_theme_ctzn2 <- function(...) {
  hc_theme_merge(
    hc_theme_tufte(),
    hc_theme(
      chart = list(
        backgroundColor = "transparent"),
      title = list(style = list(fontFamily = "Georgia")),
      subtitle = list(style = list(fontFamily = "Georgia")),
      yAxis = list(title = list(style = list(fontFamily = "Georgia")),
                   labels = list(style = list(fontFamily = "Georgia"))),
      xAxis = list(title = list(style = list(fontFamily = "Georgia")),
                   labels = list(style = list(fontFamily = "Georgia")))

    ))

}
