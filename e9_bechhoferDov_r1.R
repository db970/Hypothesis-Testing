# Dov Bechhofer
# Exercise 9: Hypothesis Testing
# DATA 306
# 12/08/23

getwd()
setwd("~/DATA 306/Exercise 9")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(rstatix)
#dataset-URL [https://www.kaggle.com/datasets/bhanupratapbiswas/olympic-data]
athletes <- read.csv("dataset_olympics.csv")
head(athletes)
str(athletes)
colnames(athletes)
dim(athletes)
na_count <-sapply(athletes, function(athletes) sum(length(which(is.na(athletes)))))
na_count # NA's Age=2732, Height=16254, Weight=17101
table(athletes$Medal)
# Medals have a lot of empty values (60310) but that’s due to the fact that when an athlete wasn’t
# in the top 3, he did not win any medals.
summary(athletes)
athletes <- athletes %>%
  drop_na(Weight, Height, Age) # removing NA's
summary(athletes)

# Age-- min = 12, mean = 25.07, max = 65
# Height-- min = 127cm, mean = 175.6cm, max = 223cm
#Weight-- min = 25kg, mean = 70.9kg, max = 214
# Year-- 1896 - 2016
n_distinct(athletes$Sport) # 55 sport type  
n_distinct(athletes$NOC) # 222 countries spamming between 1896-2016 
n_distinct(athletes$Medal)# 4 -- none, bronze, silver, and gold
#plot of number of athletes by sex
barplot(table(athletes$Sex), 
        main = "How many Olympic athletes are male and female?", 
        xlab = "Sex",
        ylab = "Number of athletes",
        ylim = c(0,60000),
        col = c("red", "blue"))

sex_count <- table(athletes$Sex) 
athletes$Sex <- as.factor(athletes$Sex)

levels(athletes$Sex)

prop.table(sex_count) # F = 31% M = 69%
# Barplot of proportiion of male and female athletes 
barplot(prop.table(sex_count), 
        main = "How many Olympic athletes are male and female?", 
        xlab = "Sex",
        ylab = "Number of athletes",
        ylim = c(0,1),
        col = c("red", "blue"))
# Ex 9 Hypothesis Testing
DiagrammeR::grViz("
digraph {
  graph []
  node [shape = plaintext]
    A [label = 'Netherlands']
    Y [label = 'Switzerland']
    C [label = 'Height']
  edge []
    A->Y
    C->A
    C->Y
  { rank = same; A; Y }
}
")
# The goal was to try to visualize the hypothesis of height differences 
# between the Netherlands and Switzerland but that didn't quite work 

countries_hyp <- athletes %>%
  filter(NOC %in% c("NED", "SUI")) # creating a filtered data set that include the Netherlands and Switzerland
# fun fact: Why Switzerland abbreviation is SUI and not SWI? 
# Because the International Olympic Committee official language is French, so the abbreviation SUI is short for Suisse
head(countries_hyp)
tail(countries_hyp)
n_distinct(countries_hyp$NOC) # NED and SUI
summary(countries_hyp$Height) #mean = 177.2cm
summary(athletes$Height)  # mean for the original athletes data-set 175.2cm
# hypothesis testing for height differences 

#H0:The average height for athletes from the Netherlands is equal to the average
#height for athletes from Switzerland.

#H1: The average height for athletes from the Netherlands is different from the
#average height for athletes from Switzerland 

#testing for normality
qqnorm(countries_hyp$Height)
qqline(countries_hyp$Height) 

shapiro.test(countries_hyp$Height) # failed the normality test according to the Shapiro test
# more normality tests
hist(countries_hyp$Height)
boxplot(countries_hyp$Height)

countries_hyp %>% 
  levene_test(Height ~ NOC) # Failed to assume equality of variance

wilcox.test(Height ~ NOC, data = countries_hyp)
#The p-value is extremely small indicating strong evidence against the null hypothesis.

countries_hyp %>%
  group_by(Sex) %>%
  identify_outliers(Height) # 17 outliers but no extreme outliers 
#T-test 
countries_hyp$NOC <- factor(countries_hyp$NOC)
country_ttest <- t.test(Height ~ NOC, data = countries_hyp)

print(country_ttest)
#The p-value is extremely small indicating strong evidence against the null hypothesis.

ggplot(countries_hyp, aes(x = NOC, y = Height, fill = NOC)) +
  geom_boxplot() +
  labs(title = "Distribution of Heights for Netherlands and Switzerland",
       x = "Country",
       y = "Height" )
#The sample estimates suggest that, on average, athletes from the Netherlands (NED)
#have a height of 180.1327, while athletes from Switzerland (SUI) have a height of 174.9507.

0.0328*180.1327 #5'91ft  for athletes from the Netherlands, *this include males and females. 
0.0328*174.9507 #5'74ft  for athletes from the Switzerland, *this include males and females.

#Plotting the height diff between NED and SUI by sex
ggplot(countries_hyp, aes(x = NOC, y = Height, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Height Differences Between Netherlands and Switzerland by Sex",
       x = "Country",
       y = "Height",
       fill = "Sex") +
  scale_y_continuous(labels = scales::number_format(scale = 1), breaks = seq(0, max(countries_hyp$Height), 5)) +
  theme_minimal()
# Median height of sex by country
median_heights <- countries_hyp %>%
  group_by(NOC, Sex) %>%
  summarize(median_height = median(Height))


print(median_heights)
0.0328*185  
0.0328*174
# NED = female are 174cm and male are 185cm or 5'7ft and 6ft
0.0328*168
0.0328*177
# SUI = female are 168cm and male are 177cm or 5'5ft and 5'8ft

# Scientists attribute this growth in height to 2 factors, 1) a diet rich in calcium, 2)
#natural selection - tall individuals have more reproductive success than others. 
#Regardless their growth has been very rapid  “The average male height in the Netherlands has gained 20 cm (eight inches) 
#in the last 150 years, according to military records. By comparison, the height of the average American man has risen a 
#mere six centimeters over the same period" (The Guardian).

https://www.theguardian.com/world/2015/apr/08/scientists-try-to-answer-why-dutch-people-are-so-tall