#' Highcharts for SJPlot outputs from a regression
#'
#' @param model
#'
#'An model object
#'
#' @param terms
#'
#'Desired terms
#'
#' @param type
#'
#' Optional, defaults to 'eff' but can also be 'pred' if specified.
#'
#' @param colors
#'
#' optional vectors of colors c("#9A32CD", "#FF3030", "#7FFFD4")
#'
#' @examples
#' data(efc)
#' efc$c161sex <- to_factor(efc$c161sex)
#' fit <- lm(neg_c_7 ~ c12hour + barthtot * c161sex, data = efc)
#'
#'plot_model(fit, type = "pred", terms = c("barthtot [30,50,70]", "c161sex"))
#'plot_highchart(fit,  terms = c("c161sex", "barthtot [30,50,70]"), color = c("red", "pink", "blue"))
#'
#' @export
plot_highchart<-function(model, terms, type, colors, size){
  library(manipulateWidget)
  library(sjPlot)
  library(sjmisc)
  library(ggplot2)
  library(highcharter)
  library(ztools)
  library(htmltools)
  library(shiny)
  library(ggeffects)

  library(dplyr)
  library(RColorBrewer)
  if(missing(type)) {
    type = "eff"



  }

  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


  if(missing(size)) {
size <- 400
} else{
    size<-size
  }

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
   if (missing(terms)){
     emo<- plot_model(model)
     edata<-emo$data
     edata<-edata %>% arrange(estimate
                              )


     hchart(edata, "scatter", hcaes(x = term , y = estimate, group=group)) %>% hc_colors(c("#a53310", "#3768c6")) %>%
       hc_add_series(edata, "errorbar", color=c("#a53310", "#3768c6"),
                     hcaes(x = "term",  low = 'round(conf.low, 2)',group=group,
                           high = 'round(conf.high, 2)'),
                     enableMouseTracking = FALSE,
                     showInLegend = FALSE)%>%
       hc_exporting(enabled = FALSE)%>%
       hc_add_theme(hc_theme_ctzn())%>%
       hc_yAxis(plotLines = list(list(color = "#a4a4a4",width = 2, value = 1))) %>%

       hc_title(text = emo$labels$title[[1]],
                style = list(fontSize = "14px"),
                align = "left")%>%
       hc_subtitle(text = emo$labels$subtitle,
                   style = list(fontSize = "10px"),
                   align = "left")%>%
       hc_legend(enabled = FALSE) %>%
       hc_xAxis(title=list(text="")) %>%
       hc_yAxis(title = list(text = "Odds Ratios"),
                labels = list(format = "{value}"))%>%
       hc_tooltip(crosshairs= list(enabled= TRUE,  color=hex_to_rgba("#2b908f", alpha = .15)),
                  backgroundColor = "#f0f0f0",
                  valueDecimals=0,
                  shared = TRUE,
                  borderWidth = 0,
                  headerFormat = paste0("Predicted <b>", 'Value' , '</b> <br> <span style="color: #2b908f;font-weight:bold">{point.key}</span>') ,
                  pointFormat = ": {point.y:.2f}") %>% hc_chart(inverted=TRUE)


     }

 else if (length(terms) == 1) {
    if(missing(colors)) {
      colors = colorspace::lighten("#030303", 0.7)
    }
    single<- plot_model(model, type =type, terms =terms )
    tbl<- single$data
    tbl<- tbl%>%mutate(labels = get_x_labels(.))
    if (!is.null(tbl$labels)){
      singleplot<-   hchart(tbl, "line", hcaes(x = labels , y = predicted))%>%hc_colors(colors)%>%
        hc_add_series(tbl, "errorbar", color=colors,
                      hcaes(x = "labels",  low = 'round(conf.low, 2)',
                            high = 'round(conf.high, 2)'),
                      enableMouseTracking = FALSE,
                      showInLegend = FALSE)%>%
        hc_exporting(enabled = FALSE)%>%
        hc_add_theme(hc_theme_ctzn())%>%
        hc_title(text = single$labels$title,
                 style = list(fontSize = "14px"),
                 align = "left")%>%
        hc_subtitle(text = single$labels$subtitle,
                    style = list(fontSize = "10px"),
                    align = "left")%>%
        hc_legend(enabled = FALSE) %>%
        hc_xAxis(title=list(text=simpleCap(single$labels$x))) %>%
        hc_size(height = size ) %>%

        hc_yAxis(title = list(text = paste0(single$labels$y)),
                 labels = list(format = "{value}"))%>%
        hc_tooltip(crosshairs= list(enabled= TRUE,  color=hex_to_rgba("#2b908f", alpha = .15)),
                   backgroundColor = "#f0f0f0",
                   valueDecimals=0,
                   shared = TRUE,
                   borderWidth = 0,
                   headerFormat = paste0("Predicted <b>", 'Value' , '</b> <br> <span style="color: #2b908f;font-weight:bold">{point.key}</span>') ,
                   pointFormat = ": {point.y:.2f}") %>%
        hc_add_series(tbl,"scatter",
                      showInLegend = FALSE,
                      hcaes(x = "labels", y = "predicted"),
                      marker = list(symbol ='circle', radius = 3,fillColor= '#FFFFFF', lineWidth = 2, lineColor = NULL))

    }else{
      singleplot<-  hchart(tbl, "line",marker = list(enabled = FALSE), hcaes(x = x , y = predicted))%>%hc_colors(colors)%>%
        hc_add_series(tbl, "arearange", fillOpacity= 0.08, marker = list(enabled = FALSE),
                      hcaes(x = "x", color = colorspace::lighten("#030303", 0.7), low = 'round(conf.low, 2)',
                            high = 'round(conf.high, 2)'),
                      enableMouseTracking = FALSE,
                      showInLegend = FALSE)%>%
        hc_exporting(enabled = FALSE)%>%
        hc_add_theme(hc_theme_ctzn())%>%
        hc_title(text = single$labels$title,
                 style = list(fontSize = "14px"),
                 align = "left")%>%
        hc_subtitle(text = single$labels$subtitle,
                    style = list(fontSize = "10px"),
                    align = "left")%>%
        hc_legend(enabled = FALSE) %>%
        hc_xAxis(title=list(text=simpleCap(single$labels$x))) %>%
        hc_size(height = size ) %>%

        hc_yAxis(title = list(text = paste0(single$labels$y)),
                 labels = list(format = "{value}"))%>%
        hc_tooltip(crosshairs= list(enabled= TRUE,  color=hex_to_rgba("#2b908f", alpha = .15)),
                   backgroundColor = "#f0f0f0",
                   valueDecimals=0,
                   shared = TRUE,
                   borderWidth = 0,
                   headerFormat = paste0("Predicted <b>", 'Value' , '</b> <br> <span style="color: #2b908f;font-weight:bold">{point.key}</span>') ,
                   pointFormat = ": {point.y:.2f}")
    }
    singleplot
  } else if (length(terms) == 2){
    double<- plot_model(model, type = type, terms =terms)
    if(length(levels(double$data$group)) == length(levels(model$model[[terms[2]]]))){  double$data$group <- factor(double$data$group,  levels = levels(model$model[[terms[2]]]))  }
    l3<- levels(double$data$group)
    l4<- levels(model$model[[stringr::word(terms[2])]])
    l4<-subset(l4, l4 %in% l3)
    double$data$group <- factor(double$data$group,  levels = l4)
    dbl<- double$data

    if(missing(colors)) {
      group_colors<-sample(col_vector,  length(unique(dbl$group)))
    } else{
      group_colors<-colors
    }

    labels<- sort(get_x_labels(dbl))

    if (!is.null(labels)){
      df<- as.data.frame(labels)
      names(df)<- 'labels'
      df$x <- as.numeric(rownames(df))
      dbl<- left_join(dbl, df, by = "x")
    #  dbl<-dbl %>% mutate( labels = ifelse(x == 1, labels[1],labels[2]))
      doublecont<-hchart(dbl, "line", color =group_colors, hcaes(x = labels , y = predicted,  group = group))%>%
        hc_add_series(dbl, "errorbar", color =group_colors,
                      hcaes(x = "labels", group = group, low = 'round(conf.low, 2)',
                            high = 'round(conf.high, 2)'),
                      enableMouseTracking = FALSE,
                      showInLegend = FALSE)%>%
        hc_exporting(enabled = FALSE)%>%
        hc_add_theme(hc_theme_ctzn())%>%
        hc_title(text = double$labels$title,
                 style = list(fontSize = "14px"),
                 align = "left")%>%
        hc_subtitle(text = double$labels$subtitle,
                    style = list(fontSize = "10px"),
                    align = "left")%>%
        hc_legend(enabled = TRUE,
                  align= 'center',
                  verticalAlign='bottom',
                  layout='horizontal',
                  title = list(text =simpleCap(double$labels$shape))) %>%
        hc_xAxis(title=list(text=simpleCap(double$labels$x))) %>%
        hc_yAxis(title = list(text = paste0(double$labels$y)),
                 labels = list(format = "{value}"))%>%
        hc_size(height = size ) %>%

        hc_tooltip(crosshairs= list(enabled= TRUE,  color=hex_to_rgba("#2b908f", alpha = .15)),
                   backgroundColor = "#f0f0f0",
                   valueDecimals=0,
                   shared = TRUE,
                   borderWidth = 0,
                   headerFormat = paste0("Predicted <b>", 'Value' , '</b> <br> <span style="color: #2b908f;font-weight:bold">{point.key}</span>') ,
                   pointFormat = ": {point.y:.2f}") %>%
        hc_add_series(dbl,"scatter",
                      showInLegend = FALSE, color =group_colors,
                      hcaes(x = "labels", y = "predicted", group = group),
                      marker = list(symbol ='circle', radius = 3,fillColor= '#FFFFFF', lineWidth = 2, lineColor = NULL))
    }else{
      dbl<-dbl %>% mutate( labels = x)
      doublecont<-  hchart(dbl, "line", color =group_colors, marker = list(enabled = FALSE), hcaes(x = labels , y = predicted,  group = group))%>%
        hc_add_series(dbl, "arearange", color =group_colors,fillOpacity= 0.08, marker = list(enabled = FALSE),
                      hcaes(x = "labels", group = group, low = 'round(conf.low, 2)',
                            high = 'round(conf.high, 2)'),
                      enableMouseTracking = FALSE,
                      showInLegend = FALSE)%>%
        hc_exporting(enabled = FALSE)%>%
        hc_add_theme(hc_theme_ctzn())%>%
        hc_title(text = double$labels$title,
                 style = list(fontSize = "14px"),
                 align = "left")%>%
        hc_subtitle(text = double$labels$subtitle,
                    style = list(fontSize = "10px"),
                    align = "left")%>%
        hc_legend(enabled = TRUE,
                  align= 'center',
                  verticalAlign='bottom',
                  layout='horizontal',
                  title = list(text =simpleCap(double$labels$shape))) %>%
        hc_xAxis(title=list(text=simpleCap(double$labels$x))) %>%
        hc_size(height = size ) %>%

        hc_yAxis(title = list(text = paste0(double$labels$y)),
                 labels = list(format = "{value}"))%>%
        hc_tooltip(crosshairs= list(enabled= TRUE,  color=hex_to_rgba("#2b908f", alpha = .15)),
                   backgroundColor = "#f0f0f0",
                   valueDecimals=0,
                   shared = TRUE,
                   borderWidth = 0,
                   borderWidth = 0,pointFormat='<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>',
                   headerFormat = paste0( simpleCap(double$labels$x),': <span style="color: #2b908f;font-weight:bold">{point.key}</span><br>',paste0( simpleCap(double$labels$y), ": <br>") ))


    }

    doublecont
  }
  else if (length(terms) == 3){
    triple<- plot_model(model, type = type, terms = terms)
 if(length(levels(triple$data$facet)) == length(levels(model$model[[terms[3]]]))){  triple$data$facet <- factor(triple$data$facet,  levels = levels(model$model[[terms[3]]]))  }
    if(length(levels(triple$data$group)) == length(levels(model$model[[terms[2]]]))){  triple$data$group <- factor(triple$data$group,  levels = levels(model$model[[terms[2]]]))  }

    l1<- levels(triple$data$facet)
    l2<- levels(model$model[[stringr::word(terms[3])]])
    l2<-subset(l2, l2%in% l1)
    triple$data$facet <- factor(triple$data$facet,  levels = l2)

    l3<- levels(triple$data$group)
    l4<- levels(model$model[[stringr::word(terms[2])]])
    l4<-subset(l4, l4 %in% l3)
    triple$data$group <- factor(triple$data$group,  levels = l4)

    trbl<- triple$data

    labels<- sort(get_x_labels(trbl))

    if (!is.null(labels)){
      df<- as.data.frame(labels)
      names(df)<- 'labels'
      df$x <- as.numeric(rownames(df))
      trbl<- left_join(trbl, df, by = "x")


     # trbl<-trbl %>% mutate( labels = ifelse(x == 1, labels[1],labels[2]))
      #group_colors<-sample(col_vector,  length(unique(trbl$group)))


      if(missing(colors)) {
        group_colors<-sample(col_vector,  length(unique(trbl$group)))
      } else{
        group_colors<-colors
      }


      plots3<- lapply(split(trbl, trbl$facet), function(data){
        #\end<-ifelse(unique(data$facet) == split(trbl, trbl$facet)[[length(unique(trbl$facet))]]$facet, 'tail', 'nottail')
        end<-ifelse(unique(data$facet) == split(trbl, trbl$facet)[[1]]$facet, 'tail', 'nottail')

        end<-unique(end)
        order<-ifelse(unique(data$facet) == split(trbl, trbl$facet)[[1]]$facet, 1,
                      ifelse(unique(data$facet) == split(trbl, trbl$facet)[[2]]$facet, 2, 0  )        )
        order<-unique(order)
        hchart(data, "line", color =  group_colors, hcaes(x = labels , y = predicted,  group = group))%>%
          hc_add_series(data, "errorbar", color =  group_colors,
                        hcaes(x = "labels", group = group, low = conf.low, 2,
                              high = conf.high),
                        enableMouseTracking = FALSE,
                        showInLegend = FALSE)%>%
          hc_exporting(enabled = FALSE)%>%
          hc_add_theme(hc_theme_ctzn())%>%
          hc_chart(marginTop= 40, marginBottom=120) %>%
          hc_title(text = unique(data$facet),
                   #y= 60,
                   style = list(fontSize = "14px"),
                   align = "center") %>%
          hc_legend(enabled =ifelse(end == 'tail', TRUE, FALSE),
                    align= 'left',
                    floating = TRUE,
                    x=0,y=15, padding =10,margin=0,
                    verticalAlign='bottom',
                    layout='horizontal',
                    title = list(text =simpleCap(triple$labels$shape))) %>%
          hc_xAxis(title=list(text =ifelse(order == 2 , simpleCap(triple$labels$x), ""),
                              reserveSpace = FALSE,
                              y=0)) %>%
          hc_yAxis(title=list(text =ifelse(order == 1 , simpleCap(triple$labels$y), ""),
                              reserveSpace = FALSE,
                              x=-2),
                   labels = list(enabled = ifelse(order == 1, TRUE, FALSE)),
                   max = max(trbl$conf.high), min=min(trbl$conf.low), labels = list(format = "{value}"))%>%
          hc_size(height = size ) %>%

          hc_tooltip(crosshairs= list(enabled= TRUE,  color=hex_to_rgba("#2b908f", alpha = .15)),
                     backgroundColor = "#f0f0f0",
                     valueDecimals=0,
                     shared = TRUE,
                     borderWidth = 0,
                     borderWidth = 0,pointFormat='<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>',
                     headerFormat = paste0( simpleCap(triple$labels$x),': <span style="color: #2b908f;font-weight:bold">{point.key}</span><br>',paste0( simpleCap(triple$labels$y), ": <br>")))%>%
          hc_add_series(data,"scatter",   color =  group_colors,
                        showInLegend = FALSE,
                        hcaes(x = "labels", y = "predicted", group = group),
                        marker = list(symbol ='circle', radius = 3,fillColor= '#FFFFFF', lineWidth = 2, lineColor = NULL))
      })
      browsable(
        tags$h3( triple$labels$title, style =  "margin-left: 20px; text-align: left; font-family: Georgia;font-size:16px;padding: 0",

                 tags$div(
                   lapply(1:length(unique(trbl$facet)), function(i) {
                     tags$div(style = paste0('width:',100/length(unique(trbl$facet)) , '%;display:block;float:left;'), plots3[i])
                   }))))
    } else{
      trbl<-trbl %>% mutate( labels =x)
   #   group_colors<-sample(col_vector,  length(unique(trbl$group)))

      if(missing(colors)) {
        group_colors<-sample(col_vector,  length(unique(trbl$group)))
      } else{
        group_colors<-colors
      }

      plots3<- lapply(split(trbl, trbl$facet), function(data){
        end<-ifelse(unique(data$facet) == split(trbl, trbl$facet)[[length(unique(trbl$facet))]]$facet, 'tail', 'nottail')
        end<-unique(end)
        order<-ifelse(unique(data$facet) == split(trbl, trbl$facet)[[1]]$facet, 1,
                      ifelse(unique(data$facet) == split(trbl, trbl$facet)[[2]]$facet, 2, 0  )        )
        order<-unique(order)
        hchart(data, "line",
               marker = list(enabled = FALSE), color =  group_colors, hcaes(x = labels , y = predicted,  group = group))%>%
          hc_add_series(data, "arearange",
                        fillOpacity= 0.08,         marker = list(enabled = FALSE),
                        color =  group_colors,
                        hcaes(x = "labels", group = group, low = conf.low, 2,
                              high = conf.high),
                        enableMouseTracking = FALSE,
                        showInLegend = FALSE)%>%
          hc_exporting(enabled = FALSE)%>%
          hc_add_theme(hc_theme_ctzn())%>%
          hc_chart(marginTop= 80) %>%
          hc_title(text = unique(data$facet),
                   #y= 60,
                   style = list(fontSize = "14px"),
                   align = "center") %>%
          hc_legend(enabled =ifelse(end == 'tail', TRUE, FALSE),
                    align= 'right',
                    floating = TRUE,
                    y = 0,
                    verticalAlign='top',
                    layout='horizontal',
                    title = list(text =simpleCap(triple$labels$shape))) %>%
          hc_xAxis(title=list(text =ifelse(order == 2 , simpleCap(triple$labels$x), ""),
                              reserveSpace = FALSE,
                              y=0)) %>%
          hc_size(height = size )
          hc_yAxis(title=list(text =ifelse(order == 1 , simpleCap(triple$labels$y), ""),
                              reserveSpace = FALSE,
                              x=-2),
                   labels = list(enabled = ifelse(order == 1, TRUE, FALSE)),
                   max = max(trbl$conf.high), min=min(trbl$conf.low), labels = list(format = "{value}"))%>%
          hc_tooltip(crosshairs= list(enabled= TRUE,  color=hex_to_rgba("#2b908f", alpha = .15)),
                     backgroundColor = "#f0f0f0",
                     valueDecimals=0,
                     shared = TRUE,
                     borderWidth = 0,pointFormat='<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:.2f}</b><br/>',
                     headerFormat = paste0( simpleCap(triple$labels$x),': <span style="color: #2b908f;font-weight:bold">{point.key}</span><br>',paste0( simpleCap(triple$labels$y), ": <br>")))

      })

      browsable(
        tags$h3( triple$labels$title, style =  "margin-left: 20px; text-align: left; font-family: Georgia;font-size:16px;padding: 0",

                 tags$div(
                   lapply(1:length(unique(trbl$facet)), function(i) {
                     tags$div(style = paste0('width:',100/length(unique(trbl$facet)) , '%;display:block;float:left;'), plots3[i])
                   }))))


    }
  }
}
