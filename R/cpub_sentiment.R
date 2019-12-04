#' Augment a data frame to include text sentiments from  afinn, NRC
#'
#'GI, QDAP
#'Harvard-IV dictionary (GI) http://www.wjh.harvard.edu/~inquirer/spreadsheet_guide.htm
#'QDAP dictionary from the package qdapDictionaries
#' @param df
#'
#' Data frame to work with
#'
#' @param text_column
#'
#' Column of text
#'
#' @examples
#'library(dplyr)
#'text <- c("I hate everything smells foul and ugly and mean and evil","Kind love peace harmony","The Carriage held but just Ourselves -","and Immortality")
#'text_df <- tibble(line = 1:4, text = text)
#'cpub_sentiment(text_df, text)
#'
#' @export
cpub_sentiment<- function(df, text_column){
  # install.packages("SentimentAnalysis")
  library(SentimentAnalysis)
  ##See if the text has a link in it
  df <- df %>%  mutate(contains_url = ifelse(grepl("http", !!sym(text_column)), 1, 0))
  df$uuid<- rep(1:nrow(df))
  df<-df%>% dplyr::mutate(sentimentGI = analyzeSentiment(df[[text_column]])$SentimentGI,
                   negativityGI = analyzeSentiment(df[[text_column]])$NegaativityGI,
                   positivityGI = analyzeSentiment(df[[text_column]])$PositivityGI,
                   sentimentQDAP = analyzeSentiment(df[[text_column]])$SentimentQDAP,
                   negativityQDAP = analyzeSentiment(df[[text_column]])$NegaativityQDAP,
                   positivityQDAP = analyzeSentiment(df[[text_column]])$PositivityQDAP)

  library(tidyr)
  ##Use tidytext to unnest tokens
  library(tidytext)
  text_df<- df %>%
    unnest_tokens(word, !!sym(text_column))

  ##Count words per entry per person ..
  count<- text_df  %>%group_by(uuid) %>%  summarise(count = n())

  ###Remove stop words before sentiment calculations
  text_df <- text_df %>%
    anti_join(stop_words)

  ###Get 'sentiment' (bing)
 #  bing <- text_df %>%
 #    inner_join(get_sentiments("bing")) %>%
 #    count(uuid, sentiment)%>%
 #    spread(sentiment, n, fill = 0) %>%
 #   mutate(bing_sentiment = positive - negative)
 #
 # bing<-bing %>% mutate(bing_negative = negative,
 #                       bing_positive = positive)
 # bing<-select(bing, -negative, - positive)

  ## Get AFINN sentiment
  afinn <- text_df %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(uuid) %>%
    summarise(afinn_sentiment=  mean(value))
  single_nrc<-get_sentiments("nrc")
  single_nrc<-single_nrc %>% select(word)
  single_nrc<- unique(single_nrc)
  single_nrc$emotion<- 1
  single<- text_df %>% select(uuid, word)
  emotion_share <- single %>% left_join(single_nrc)

emotion_share<-emotion_share %>% group_by(uuid) %>%mutate(nwords = n())
emotion_share<-emotion_share %>% group_by(uuid) %>%mutate(emotion = sum(!is.na(emotion)))
emotion_share<-emotion_share %>% group_by(uuid) %>%mutate(emotion_share_nrc = emotion/nwords)
emotion_share<- emotion_share %>% select(-word)
emotion_share<- unique(emotion_share)
emotion_share<- select(emotion_share,uuid, emotion_share_nrc)
  nrc <- text_df %>%
    inner_join(get_sentiments("nrc")) %>%
    count(uuid,sentiment)%>%
    spread(sentiment, n, fill = 0)

  missing<- setdiff( c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust"),names(nrc) )
  nrc[,missing] <- 0

  nrc <-nrc %>% group_by(uuid) %>%
    mutate(emotion=sum(anger+anticipation+disgust+fear+joy+sadness+surprise+trust))%>%
    mutate(Per_anger= anger/emotion)%>%
    mutate(Per_anticipation= anticipation/emotion)%>%
    mutate(Per_disgust=disgust/emotion)%>%
    mutate(Per_fear=fear/emotion)%>%
    mutate(Per_joy=joy/emotion)%>%
    mutate(Per_sadness=sadness/emotion)%>%
    mutate(Per_surprise=surprise/emotion)%>%
    mutate(Per_trust=trust/emotion)
nrc<- select(nrc, -emotion, - c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust"))
names(nrc)[-1]<- paste0(names(nrc)[-1], "_nrc")
df<- left_join(df, afinn, by = "uuid")
#  df<- left_join(df, bing,  by = "uuid")
  df<- left_join(df, nrc, by = "uuid")
  df<- left_join(df, count, by = "uuid")
  df<- left_join(df, emotion_share, by = "uuid")



df
}


