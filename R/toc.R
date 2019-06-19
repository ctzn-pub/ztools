
#' Table of contents for Distill Rmd Articles
#'
#' Our  table of contents, made for Distill articles in R markdown. Returns the HTML code needed to load various scripts used and takes a list of tab names/iframe content as arguments.
#'
#' @param list_of_tabs
#'
#' A list of named vectors with a tab title & url for content
#'
#' @examples
#'
#'toc <- list(
#'  c(title = "1. Overview", icon = "fas fa-bezier-curve", description = "this is the overview of our document"),
#'  c(title = "2. This 2nd section", icon = "fas fa-bezier-curve", description = "second section covers other stuff"),
#' c(title = "2a. Overview again", icon = "fas fa-bezier-curve", description = "this is the overview") )
#'
#' @export
#'
#'
#'


toc_function<-function(toc){
  pacman::p_load(shiny, stringr)
  toc1<- tags$ul( class="toc", style="display: block;",
                  tags$link(rel="stylesheet", href="https://www.onmundo.org/cpub/design/toc_style.css"),
                  tags$link(rel = "stylesheet",  href="https://use.fontawesome.com/releases/v5.6.3/css/all.css", integrity="sha384-UHRtZLI+pbxtHCWp1t77Bi1L4ZtiqrqD80Kn4Z8NTSRyMA2Fd33n5dQ8lWUE00s/", crossorigin="anonymous" ),
                  tags$table(style="width:100%",(lapply( 1:length(toc),function(tocnum){
                    tags$tr(

                      if (!is.na(toc[[tocnum]]["icon"])){tags$td( style="width:20%" , tags$span(class = "chapter",tags$i(class = toc[[tocnum]][['icon']]), toc[[tocnum]]['title']))}
                      else{ tags$td( style="width:20%" , tags$span(class = "chapter",tags$i(class = toc[[tocnum]][['icon']]), toc[[tocnum]]['title']))},

                      tags$td(tags$a(href = paste0("#",gsub(" ", "-",trimws(
                        gsub('^\\.',"", gsub("^\\d+|", "",tolower(toc[[tocnum]]['title'])))))), toc[[tocnum]]['description']))
                    )
                  }))))



  toc1<- gsub("[\t\n\r\v\f]", "", toc1)
  str_squish(toc1)


}

