#' Highcharts options export function
#'
#' @param plot1
#'
#' HIghcharter HMTL widget
#'
#' @param filename
#'
#' Name of the file to save
#'
#' @examples
#'library(highcharter)
#'plot<- hchart(mtcars$mpg)
#'cpub_savejson(plot, "mtcars")
#'
#'
#' @export
cpub_savejson<-function(plot1, filename ){

  ##Helper function
  library(jsonlite)
  JS_to_json <- function(x) {
    class(x) <- "json"
    return(x)
  }

  saveJSON<-function(plotname){
    plotname$x$hc_opts <- rapply(
      object = plotname$x$hc_opts,
      f = JS_to_json,
      classes = "JS_EVAL",
      how = "replace"
    )
    js <- toJSON(
      x = plotname$x$hc_opts, pretty = TRUE, auto_unbox = TRUE, json_verbatim = TRUE,
      force = TRUE, null = "null", na = "null"
    )
    js
    #writeLines(js, paste0(plotname, ".JSON"))
  }

  js<- saveJSON(plot1)

  writeLines(js, paste0(filename,".json"))
}
