library(MASS)
library(corrplot)
library(readxl)
library(writexl)
library(tidyverse)
library(nnet)
library(psych)
library(grid)
library(gridExtra)
library(ggtext)
library(ggExtra)
library(e1071)
library(caret)

###############################################################################
###############################################################################

setwd("~/Desktop/rahman")

#Loading in Excel dataset for intersections

df <- read_excel("dfr.xlsx", range = "a1:t795", sheet = "rpf",)
df_sev <- read_excel("dfr.xlsx", range = "a1:k1134", sheet = "sev")

names(df)
names(df_sev)

#Factoring variables
df_sev$injury_level_cyclist <- factor(df_sev$injury_level_cyclist)
df$bike_marking <-  factor(df$bike_marking, labels = c("No bike lanes/markings", "Bike lanes/markings"))
df$signalization <-  factor(df$signalization, labels = c("No signalization", "Signalized"))
df$trees <-  factor(df$trees, labels = c("No trees", "Trees"))
df$taxi_stop <-  factor(df$taxi_stop, labels = c("No taxi stop", "Taxi stop"))
df$side_parking <-  factor(df$side_parking)
df_sev$season <-  factor(df_sev$season, levels = c("Spring", "Summer", "Autumn", "Winter"))
df_sev$day <-  factor(df_sev$day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
df$side_walk <-  factor(df$side_walk)
df$bus_stop <-  factor(df$bus_stop, labels = c("No bus stop", "Bus stop"))
df$ent_park_lot <-  factor(df$ent_park_lot, labels = c("No parking lot", "Parking Lot"))
df_sev$holiday <-  factor(df_sev$holiday,labels = c("Non-holidays", "Holidays"))
df_sev$at_cond <-  factor(df_sev$at_cond)
df_sev$p_day <-  factor(df_sev$p_day)
df_sev$day_cat <-  factor(df_sev$day_cat)
df$street_lighting <- factor(df$street_lighting, labels = c("No streetlight", "Streetlight present"))
df$road_class <- factor(df$road_class)
df$median_seperator_on_street_1 <- factor(df$median_seperator_on_street_1, labels = c("No median", "Median"))
df$median_seperator_on_street_2 <- factor(df$median_seperator_on_street_2, labels = c("No median", "Median"))
df$side_walk <- factor(df$side_walk, labels = c("No side walk", "Side walk"))
df$seperate_bike_lane <-  factor(df$seperate_bike_lane, labels = c("Bike lane not separate", "Bike lane separate"))
str(df)
str(df_sev)






# Section 2: Descriptive plots -------------------------------------------------------

plot1 <- df_sev %>% 
  ggplot(aes(x = fct_infreq(injury_level_cyclist))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Injury level of cyclist") +
  scale_y_continuous(limits = c(0,1000))
plot1


plot2 <- df_sev %>% 
  ggplot(aes(x = fct_infreq(season))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per Season of year") +
  scale_y_continuous(limits = c(0,400))
plot2


plot3 <- df_sev %>% 
  ggplot(aes(x = fct_infreq(day))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per day of week") +
  scale_y_continuous(limits = c(0,225))
plot3


plot4 <- df_sev %>% 
  ggplot(aes(x = fct_infreq(day_cat))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per day category") +
  scale_y_continuous(limits = c(0,1000))
plot4


plot5 <- df_sev %>% 
  ggplot(aes(x = fct_infreq(p_day))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per period of day") +
  scale_y_continuous(limits = c(0,1000))
plot5


plot6 <- df_sev %>% 
  ggplot(aes(x = fct_infreq(at_cond))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per atmospheric condition") +
  scale_y_continuous(limits = c(0,720)) +
  coord_flip()
plot6


plot7 <- df_sev %>% 
  ggplot(aes(x = fct_infreq(holiday))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency on holidays") +
  scale_y_continuous(limits = c(0,1150)) 
plot7



plot8 <- df %>% 
  ggplot(aes(x = fct_infreq(`bike_marking`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency where bike lanes/markings were present") +
  scale_y_continuous(limits = c(0,700)) 
plot8



plot9 <- df %>% 
  ggplot(aes(x = fct_infreq(`signalization`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per signalization") +
  scale_y_continuous(limits = c(0,450)) 
plot9


plot10 <- df %>% 
  ggplot(aes(x = fct_infreq(`trees`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency involving tree occlusion") +
  scale_y_continuous(limits = c(0,600)) 
plot10


plot11 <- df %>% 
  ggplot(aes(x = fct_infreq(`road_class`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per road category") +
  scale_y_continuous(limits = c(0,525)) +
  coord_flip()
plot11



plot12 <- df %>% 
  ggplot(aes(x = fct_infreq(`bus_stop`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency within proximity of bus stop") +
  scale_y_continuous(limits = c(0,700)) 
plot12


plot13 <- df %>% 
  ggplot(aes(x = fct_infreq(`taxi_stop`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency within proximity of taxi stop") +
  scale_y_continuous(limits = c(0,800)) 
plot13


plot14 <- df %>% 
  ggplot(aes(x = fct_infreq(`ent_park_lot`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency at entrance of parking lot") +
  scale_y_continuous(limits = c(0,850)) 
plot14



plot15 <- df %>% 
  ggplot(aes(x = fct_infreq(`street_lighting`))) +
  geom_bar(width = 0.3)+
  labs(x = "",
       y = "Frequency")+
  theme_bw() +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size = 11)) +
  ggtitle("Accident frequency per streetlight illumination") +
  scale_y_continuous(limits = c(0,760)) 
plot15



plot16 <- df |> select(c(3:6, 14)) |> pairs.panels(cex.cor = 0.8) 

