

#install.packages("highcharter")
#' Making plots from
#'
#' Based on Highcharter Google theme.
#'
#' @param ... Named argument to modify the theme
#'
#'
#'
#' highcharts_demo() %>%
#'   hc_add_theme(hc_theme_ctzn())
#'
#'
#'
#' @export
wbPlots <- function(indicator) {
library(highcharter)
library(wbstats)


pop_data <- wb(indicator = indicator,  startdate = 1980, enddate = 2018)

#pop_data <- wb(indicator = "SH.ALC.PCAP.LI",  startdate = 1980, enddate = 2018)
#metadata<- wbsearch("SH.ALC.PCAP.LI", fields=c("indicatorID"),extra = TRUE)

metadata<- wbsearch(indicator, fields=c("indicatorID"),extra = TRUE)
library(dplyr)
latest_data<- pop_data %>%
  group_by(country,iso3c) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup


#subtitle<-unique(wide_indicators_bar$ihme_indicator_description)
title<-unique(metadata$indicator)
subtitle<-unique(metadata$indicatorDesc)
source<-ifelse(!is.na(unique(metadata$sourceOrg)), unique(metadata$sourceOrg), "World Bank")
short_name<- substr(metadata$indicator,1, 25)
unit<-ifelse(!is.na(unique(metadata$unit)),unique(metadata$unit), "Value")

top<-latest_data%>%top_n(15, wt = value)
bottom<-latest_data%>%top_n(-15, wt = value)
bottom<-bottom%>%arrange(value)


topcountries<-  hchart(top, "bar",name=short_name, hcaes(country,value))%>%
 # hc_size(height=height)%>%
  hc_title(text=title)%>%
  hc_subtitle(text=subtitle)%>%
  hc_credits(enabled = TRUE,  text = paste0("Source: " , source))%>%
  hc_tooltip(pointFormat = paste0("{point.y:.f} ", unit),shared = TRUE)%>%
  hc_yAxis(title=list(text=unit))%>%
  hc_xAxis(title=list(text=""))%>%
  hc_chart(backgroundColor = "transparent")%>%
  hc_add_theme(hc_theme_tufte())


bottomcountries<-  hchart(bottom, "bar",name=short_name, hcaes(country,value))%>%
  # hc_size(height=height)%>%
  hc_title(text=title)%>%
  hc_subtitle(text=subtitle)%>%
  hc_credits(enabled = TRUE,  text = paste0("Source: " , source))%>%
  hc_tooltip(pointFormat = paste0("{point.y:.f} ", unit),shared = TRUE)%>%
  hc_yAxis(title=list(text=unit))%>%
  hc_xAxis(title=list(text=""))%>%
  hc_chart(backgroundColor = "transparent")%>%
  hc_add_theme(hc_theme_tufte())

library(stringr)
#Tooltip...
urlico <- "url(https://onmundo.org/cpub/visualization/flags/%s.jpg)"
ttvars <- c("country", "value")
tt <- tooltip_table(ttvars, sprintf("{point.%s}", ttvars))
tt<-str_replace_all(tt, "<th>cntry", "" )
tt<- paste0("<img src=\"{point.flagicon}\" />", tt)
tt<- str_replace(tt, "<table>", "<table style=\"padding-bottom: 5px\"/>")

library(htmltools)
t4<- str_replace(tt, "<th>country_name", paste0("<th>",short_name, ":"))
t4<-HTML(paste0("<img src=\"{point.flagicon}\" />&nbsp; <b> {point.name} </b><table style=\"padding-bottom: 5px\"/>\n
 <th>", short_name,"</th>\n <tr>\n <td> <font color='#0da094'> <b> {point.", "unscaled_value", "} ", unit ," </font></b></td> \n
  </tr>\n <tr>\n <td> <b>{point.quints_rec}</b></td> \n </tr>\n </table>"))

map <- latest_data %>% mutate(flagicon = sprintf(urlico, iso2c),flagicon = str_replace_all(flagicon, "url\\(|\\)", ""))
map<-map%>%mutate(quints= ntile(map$value, 5))
## Recoding map$quints into map$quints_rec
library(forcats)
map$quints_rec <- as.character(map$quints)
map$quints_rec <- fct_recode(map$quints_rec,
                             "Quntile 1 (Lowest)" = "1",
                             "Quintile  3" = "3",
                             "Quintile 5 (Highest)" = "5",
                             "Quintile 2" = "2",
                             "Quintile 4" = "4")
map$quints_rec <- as.character(map$quints_rec)
color1<- "#3768c6"
color2<-"#9cacc9"
stops <- data.frame(q = 0:4/4, c = c("#000000", "#5e5e5e", "#afafaf",color2, color1))
stops2 <- data.frame(q = c(1,2,3,4,5), c = c("#000000", "#5e5e5e", "#afafaf", color1,color2))

stops <- list_parse2(stops)
stops2 <- list_parse2(stops2)

cat_A <- map %>% filter(quints == 1)
cat_B <- map %>% filter(quints == 2)
cat_C <- map %>%filter(quints == 3)
cat_D<- map %>% filter(quints == 4)
cat_E <- map %>% filter(quints == 5)

mapDat <- rio::import("data/globalMapData.rds")
map_cat<- highchart(type = "map") %>%
  hc_plotOptions(map = list(
    allAreas = FALSE,
    borderColor = "#FAFAFA",
    borderWidth = 0.4,
    tooltip = list(valueDecimals = 2),
    joinBy =  c("iso-a3", "iso3c"  ),
    mapData = mapDat
  )) %>%
  hc_add_series(name = "Quintile 1 (Lowest)", data = cat_A, color = "#000000") %>%
  hc_add_series(name = "Q2", data = cat_B, color = "#5e5e5e") %>%
  hc_add_series(name = "Q3", data = cat_C, color = "#afafaf")%>%
  hc_add_series(name = "Q4", data = cat_D, color = "#9cacc9") %>%
  hc_add_series(name = "Quintile 5 (Highest)", data = cat_E, color = "#3768c6")%>%
  hc_chart(backgroundColor = "transparent")%>%
  hc_credits(enabled = TRUE, text = paste0("Source: ", source))%>%
  hc_title(text = title)%>%
  hc_subtitle(text = subtitle)%>%
  hc_tooltip(
    headerFormat = as.character(tags$b("{point.potato}")),
    pointFormat = t4,
    backgroundColor = "#fff",
    useHTML = TRUE,
    shadow = FALSE,
    shape = "square" )


map2<- highchart(type = "map") %>%
  hc_plotOptions(map = list(
    allAreas = FALSE,
    borderColor = "#FAFAFA",
    borderWidth = 0.4,
    tooltip = list(valueDecimals = 2),
    joinBy =  c("iso-a3", "iso3c"  ),
    mapData = mapDat
  )) %>%
  hc_add_series(name = "Quintile 1 (Lowest)", data = cat_A, color = "#efff33") %>%
  hc_add_series(name = "Q2", data = cat_B, color = "#00000") %>%
  hc_add_series(name = "Q3", data = cat_C, color = "#000000")%>%
  hc_add_series(name = "Q4", data = cat_D, color = "#000000") %>%
  hc_add_series(name = "Quintile 5 (Highest)", data = cat_E, color = "#3768c6")%>%
  hc_chart(backgroundColor = "transparent")%>%
  hc_credits(enabled = TRUE, text = paste0("Source: ", source))%>%
  hc_title(text = title)%>%
  hc_subtitle(text = subtitle)%>%
  hc_tooltip(
    headerFormat = as.character(tags$b("{point.potato}")),
    pointFormat = t4,
    backgroundColor = "#fff",
    useHTML = TRUE,
    shadow = FALSE,
    shape = "square" )




library( rnaturalearth )
library( topogram )

sf_world <- ne_countries(scale = 110,continent = c("europe", "australia" , "asia", "north america", "south america", "africa"), returnclass = "sf")
# Add a numeric column
latest_data_small<-latest_data%>%select(iso3c, value)
sf_world<-left_join(sf_world, latest_data_small, by = c("iso_a3" = "iso3c") )
sf_world$value[is.na(sf_world$value)] <- 0
sf_world<-sf_world%>%mutate(value = value + .05)

# Create cartogram
cartogram<- topogram( shape = sf_world,
  value = "value")



sf_world <- ne_countries(returnclass='sf')


return (list(topcountries,
bottomcountries,
map_cat,
map2,
cartogram
))


}
