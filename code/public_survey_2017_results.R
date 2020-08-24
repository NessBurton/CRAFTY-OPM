
# date: 24/08/20
# author: VB
# purpose: extract stats on public perceptions of OPM risk from 2017 survey
# data provided by SERG

library(tidyverse)

survey <- read.csv("./data-raw/OPM_public_survey_results_2017.csv")
head(survey)

# interested in questions 19 a,b,c,d,e
# We would like to know the risk you think that Oak Processionary Moth may pose to people and the environment generally. 
# Please indicate the extent to which you agree that oak processionary moth would be a threat to each of the following:

# a. human health
# b. animal health
# c. oak trees
# d. biodiversity
# e. business and the economy

# remove first row (contains questions)
Qs <- survey[1,]
survey <- survey[-1,]
# filter to Q19
survey <- survey[,c(42:46)]
# re-factor to make sure question levels are dropped
survey$Q19a<-factor(survey$Q19a)
survey$Q19b<-factor(survey$Q19b)
survey$Q19c<-factor(survey$Q19c)
survey$Q19d<-factor(survey$Q19d)
survey$Q19e<-factor(survey$Q19e)

# rename cols
colnames(survey) <- c("humans","animals","oaks", "biodiversity", "business")

# gather and get counts
survey_summary <- gather(survey, aspect, risk) %>%
  group_by(risk, aspect) %>%
  tally %>% 
  spread(aspect, n, fill = 0)
survey_summary<-survey_summary[-1,]

# plot to check?
plot(survey$humans)
plot(survey$animals)
plot(survey$oaks)
plot(survey$biodiversity)
plot(survey$business)

# rowSums
survey_summary$tot<-rowSums(survey_summary[,sapply(survey_summary, is.numeric)])

# as percentage
combined_tot<-sum(survey_summary$tot)
survey_summary <- survey_summary %>% 
  mutate(perc=tot/combined_tot*100)
