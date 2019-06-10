
#' Mouseover Tabsets Optimized for Distill Rmd Articles
#'
#' Our tabets, made for Distill articles in R markdown. Returns the HTML code needed to load various scripts used and takes a list of tab names/iframe content as arguments.
#'
#' @param tabID
#'
#'A number, 1-9 (needs a unique ID to distinguish from other tabsets
#')
#' @param list_of_tabs
#'
#' A list of named vectors with a tab title & url for content
#'
#' @param height
#'
#'Desired height.
#'
#' @param icon optional; takes a fontawesome identifier to add an icon
#'
#' @examples
#'
#' tabs(
#' tabID = 2,
#' list_of_tabs = list(
#'   c(title = "Income Inequality", icon = "fas fa-bezier-curve", url = "https://www.onmundo.org/cpub/visualization/viz/global_adolescent_birth_rate_GINIBubbleD.html"),
#'   c(title = "GDP", url = "https://www.onmundo.org/cpub/visualization/viz/global_adolescent_birth_rate_GDPBubbleD.html"),
#'   c(title = "Gender Inequality", url = "https://www.onmundo.org/cpub/visualization/viz/global_adolescent_birth_rate_GIIBubbleD.html"),
#'   c(title = "Human Development", url = "https://www.onmundo.org/cpub/visualization/viz/global_adolescent_birth_rate_HDIBubbleD.html")
#' ),
#' height = 500)
#'
#'
#' @export
#'
#'
#'
tabs<-function(tabID, list_of_tabs, height){
library(shiny)
  tags$body(
    tags$link(rel="stylesheet", href="http://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
    tags$link(rel="stylesheet", href="https://www.onmundo.org/cpub/design/jquery-tabs.css"),
    tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.2.0/css/all.css", integrity="sha384-hWVjflwFxL6sNzntih27bfxkr27PmbbK/iSvJ+a4+0owXq79v+lsFkW54bOGbiDQ" , crossorigin="anonymous"),
    tags$div(id=paste0("tabs", tabID),


             tags$ul(lapply( 1:length(list_of_tabs),function(tabnum){
               tags$li(tags$a( href=paste0("#tabs",tabID ,"-", tabnum),
                               if (!is.na(list_of_tabs[[tabnum]]["icon"]))
                               {tags$div(tags$i(class = list_of_tabs[[tabnum]][["icon"]]), list_of_tabs[[tabnum]][["title"]])}
                               else
                               {list_of_tabs[[tabnum]][["title"]]}



               ))
             })),



             lapply( 1:length(list_of_tabs),function(tabnum){
               tags$div(id=paste0("tabs",tabID ,"-", tabnum),
                        tags$iframe(src= list_of_tabs[[tabnum]][["url"]], scrolling="no",
                                    style=paste0("border-style: none; overflow: hidden; display:block; margin:auto;  height:" , height , "px;")))
             })

    ),


    tags$script(src="https://www.onmundo.org/cpub/design/jquery.js"),
    tags$script(src="https://www.onmundo.org/cpub/design/jquery-tabs.js") )
}


