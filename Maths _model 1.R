
library(MASS)
library(corrplot)
library(readxl)
library(writexl)
library(tidyverse)
library(nnet)
library(psych)
library(grid)
library(gridExtra)
library(e1071)
library(caret)
###############################################################################
###############################################################################

setwd("~/Desktop/rahman")





# Section 2: Modelling Risk Provoking factors---------------------------------------------------------------
#Factorization

rpf <-  read_excel("dfr.xlsx", sheet = "rpf")

rpf$polygon_id <- factor(rpf$polygon_id)
rpf$road_class <- factor(rpf$road_class)
rpf$seperate_bike_lane <-  factor(rpf$seperate_bike_lane, labels = c("Bike lane not separated", "Bike lane separated"))
rpf$bike_marking <-  factor(rpf$bike_marking, labels = c("No bike lanes/markings", "Bike lanes/markings"))
rpf$side_walk <-  factor(rpf$side_walk, labels = c("No side walk", "Side walk"))
rpf$signalization <-  factor(rpf$signalization, labels = c("No signalization", "Signalized"))
rpf$median_seperator_on_street_1 <-  factor(rpf$median_seperator_on_street_1, labels = c("No median on st.1", "Median on st.1"))
rpf$median_seperator_on_street_2 <-  factor(rpf$median_seperator_on_street_2, labels = c("No median on st.2", "Median on st.2"))
rpf$bus_stop <- factor(rpf$bus_stop, labels = c("No bus stop", "bus stop"))
rpf$trees <- factor(rpf$trees, labels = c("No trees", "Trees"))
rpf$ent_park_lot <- factor(rpf$ent_park_lot, labels = c("No parking lot entrance", "Parking lot entrance"))
rpf$taxi_stop <- factor(rpf$taxi_stop, labels = c("No taxi stop", "Taxi stop"))
rpf$street_lighting <- factor(rpf$street_lighting, labels = c("No streetlighting", "Streetlight"))
rpf$side_parking <- factor(rpf$side_parking, labels = c("No side parking", "Side parking"))
str(rpf)




#Splitting loaded dataset into Training and Testing datasets
set.seed(123)
rpf$number <- 1:nrow(rpf)
training <- rpf[sample(rpf$number,round(nrow(rpf)*0.7), replace = FALSE,),]

testing <- rpf[!rpf$number %in% training$number,]
nrow(training) + nrow(testing) == nrow(rpf)





##Model 1: linear regression model (OLS).......................................................

#General form of equation >>  accident_count = a + b * approach_bike_vol + c * AADT_of_intersection + d * signalization........
OLS <-  lm(data = training, formula = accident_count ~ -1 +
               approach_bike_vol +
#                AADT_of_intersection  +
#                 tot_no_app_ln       +
#                 dist_btw_car_cyclist +
                road_class +
#                seperate_bike_lane +
#                bike_marking +
#                side_walk  +
                signalization  ,
#              median_seperator_on_street_1  +
#              median_seperator_on_street_2  +
#              no_lanes_acc_spot +
#              bus_stop  +
#              trees +
#              ent_park_lot +
#              taxi_stop +
#              street_lighting +
#              side_parking 
            ) 
summary(OLS)
# Model1 Crash prediction equation  = 6.281 + 0.0001431 * approach_vol 
testing$OLS <- ceiling(predict.lm(OLS, newdata = testing))






##Model 2: Poisson regression model....................................................

##tot_Crashes = exp(a + b * AADT_of_intersection + c * signalized + d * tot_no_app_ln + e * bike_marking........)
poisson <-  glm(data = training, formula = accident_count ~ 
                          approach_bike_vol +
#                          AADT_of_intersection  +
#                          tot_no_app_ln       +
#                         dist_btw_car_cyclist +
#                         road_class +
#                          seperate_bike_lane +
#                          bike_marking +
#                          side_walk  +
                          signalization  
#                        median_seperator_on_street_1  +
#                          median_seperator_on_street_2  +
#                          no_lanes_acc_spot +
#                          bus_stop  +
#                          trees +
#                          ent_park_lot +
#                          taxi_stop +
#                          street_lighting 
#                          side_parking 
               , family = poisson(link = "log"))
summary(poisson)
# Model2 Crash prediction equation = exp(2.032 + 0.00001108 * approach_vol)
testing$poisson <- ceiling(predict.glm(poisson, newdata = testing, type = "response"))







#Model 3: Safety Performance Function regression model..........................................

## accident_count = exp(-1.22240 + 0.15240 * log(approach_vol * approach_bike_vol) + 0.21610*signalization(Signalized))
spf <-  glm(data =  training, formula = accident_count ~ 
                log(AADT_of_intersection * approach_bike_vol)  +
#                log(approach_bike_vol)  +
#                approach_bike_vol +
#                AADT_of_intersection  +
#                tot_no_app_ln       +
#                dist_btw_car_cyclist +
#                road_class +
#                seperate_bike_lane +
#                bike_marking +
#                side_walk  +
                signalization  
#              median_seperator_on_street_1  +
#                median_seperator_on_street_2  +
#                no_lanes_acc_spot +
#                bus_stop  +
#                trees +
#                ent_park_lot +
#                taxi_stop +
#                street_lighting +
#                side_parking 
                                )
summary(spf)
# Model3 Crash prediction equation = exp(-2.2438) * approach_vol^0.4515
# (could vary due to randomnization of test/training sampling)
testing$spf <- ceiling(predict.glm(spf, newdata = testing, type = "response"))






#Goodness of fit tests ....................................................................
(MSE <- testing %>% 
  select(polygon_id, accident_count, OLS, poisson, spf) %>% 
  mutate(OLS_sq_error = (OLS - accident_count)^2,
         poisson_sq_error = (poisson - accident_count)^2,
         spf_sq_error = (spf - accident_count)^2) %>% 
  summarize(MSE_OLS = mean(OLS_sq_error),
            MSE_poisson = mean(poisson_sq_error),
            MSE_SPF = mean(spf_sq_error)))


