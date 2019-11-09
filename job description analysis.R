getwd()
setwd("C:/Users/Sineus/Desktop/essaie")
job<-read.csv("job_skills.csv", header = TRUE, stringsAsFactors = F)
head(job)
colnames(job)
class(job)
str(job)
library(tm)
library(tidytext)
install.packages(SentimentAnalysis)
library(SentimentAnalysis)
library(tidyverse)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(stringr)
youtube<-job%>%
  filter(Company=="YouTube")%>%
  group_by(Category)%>%
  summarise(frequent=n())%>%arrange(desc(frequent))
google<-job%>%
  filter(Company=="Google")%>%
  group_by(Category)%>%
  summarise(freq=n())%>%
  arrange(desc(freq))
head(google)  
head(job$Location,3)
red<-head(job,20)
glimpse(job)
df<-job%>%
  select(Responsibilities)%>%
  unnest_tokens(word, Responsibilities)%>%
  count(word, sort=TRUE)
text<-job%>%
  filter(Company=="YouTube"|Category=="Business Strategy")%>%
  group_by(Category)%>%
  unnest_tokens(word, Responsibilities)%>%
  anti_join(stop_words)%>%
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  count(word) %>%
  arrange(desc(n))
head(text)
