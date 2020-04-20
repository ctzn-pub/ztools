
#' Tabsets for RMD
#'
#' @param tab_contents
#'
#' The contents of the tabs (must be a list and same length/order as titles)
#'
#' @param tab_titles
#'
#' The titles of the tabs (must be a list and same length/order as content)
#'
#' @param name
#'
#'Unique name for the tabset
#'
#' @examples
#'
#' ztabs(name = 'name',  tab_contents= list('panel1 content', 'panel2 content')  ,  tab_titles =list('title1','title2') )
#'
#'You can also add in little icons with fontawesome
#'
#'  tab_titles= list(tags$div(fa(name = 'venus-mars', fill="steelblue"), "Sex")
#'
#'** note...make sure you have access to the proper styles file.
#' @export
#'
#'
#'
ztabs<-function(tab_titles, tab_contents, name){
  inp<- lapply(1:length(tab_titles),function(tabtitle){
    if(tabtitle == 1){ inputtag<- tags$input(type="radio",
                                             name=name,
                                             id=paste0(  tabtitle, "id", name),
                                             'aria-controls'=paste0(tabtitle, name) ,
                                             checked=NA)}else{inputtag<- tags$input(type="radio",
                                                                                    name=name,
                                                                                    id=paste0( tabtitle, "id", name),
                                                                                    'aria-controls'=paste0(tabtitle, name)
                                             )}

    tagList(
      inputtag,
      tags$label('for'=paste0(tabtitle, "id", name),
                 tab_titles[[tabtitle]]))})
  sec<- lapply(1:length(tab_contents),function(tab_content){
    tags$section( id=paste0( tab_content , name) ,
                  class="tab-panel", tab_contents[[tab_content]])})
  tags$div(class="tabset",
           tagList(inp),tags$div(class="tab-panels", tagList(sec) )
  )


}

