#data

ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")

#installed.packages("lubridate")
#install.packages("forecast")
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)

#Linear regression in R
ghg$log.ch4 <- log(ghg$ch4+1)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.SA <- log(ghg$surface.area)

ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)


mod.full <- lm(log.ch4 ~ log.age + log.DIP + BorealV + airTemp +
                 log.precip + mean.depth + log.SA, data = ghg)
summary(mod.full)

full.step <- ols_step_forward_aic(mod.full)
full.step

### Homework Questions ###

# Question 1 #

ghg$trans.co2 <- ((1)/(ghg$co2 + 1000))
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0) 
mod.fully <- lm (trans.co2 ~ airTemp +
                   log.age + 
                   mean.depth+
                   log.precip +
                   log.DIP + 
                   log.ch4,
                   data=ghg)
summary(mod.fully)


# Question 2 #
ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")

#Almond
almond <- ETdat %>% 
  filter(crop == "Almonds") %>% 
  group_by(date) %>% 
  summarise(Et.in = mean(Ensemble.ET, na.rm=TRUE))

almond_ts <- na.omit(ts(almond$Et.in, 
                start = c(2016,1), 
                frequency= 12))

almond_dec <- decompose(almond_ts)

plot(almond_dec)

#Pistachios
Pistachios <- ETdat %>% 
  filter(crop == "Pistachios") %>% 
  group_by(date) %>% 
  summarise(Et.in = mean(Ensemble.ET, na.rm=TRUE))

Pistachios_ts <- na.omit(ts(Pistachios$Et.in, 
                start = c(2016,1), 
                frequency= 12))

Pistachios_dec <- decompose(Pistachios_ts)

plot(Pistachios_dec)

#Fallow/Idle Cropland
Fallow.Idle <- ETdat %>% 
  filter(crop == "Fallow/Idle Cropland") %>% 
  group_by(date) %>% 
  summarise(Et.in= mean(Ensemble.ET, na.rm=TRUE))

Fallow.Idle_ts <- na.omit(ts(Fallow.Idle$Et.in, 
                    start = c(2016,1), 
                    frequency= 12))

Fallow.Idle_dec <- decompose(Fallow.Idle_ts)

plot(Fallow.Idle_dec)

#corn
Corn <- ETdat %>% 
  filter(crop == "Corn") %>% 
  group_by(date) %>% 
  summarise(Et.in = mean(Ensemble.ET, na.rm=TRUE))

Corn_ts <- na.omit(ts(Corn$Et.in, 
                             start = c(2016,1), 
                             frequency= 12))

Corn_dec <- decompose(Corn_ts)

plot(Corn_dec)

#	Grapes
Grapes <- ETdat %>% 
  filter(crop == "Grapes (Table/Raisin)") %>% 
  group_by(date) %>% 
  summarise(Et.in = mean(Ensemble.ET, na.rm=TRUE))

Grapes_ts <- na.omit(ts(Grapes$Et.in, 
                      start = c(2016,1), 
                      frequency= 12))

Grapes_dec <- decompose(Grapes_ts)

plot(Grapes_dec)

# Question 3 #

#pistachios
Pista_y <- na.omit(Pistachios_ts)
modelPista <- arima(Pista_y , 
                    order = c(1,0,0))
modelPista

modelP <- arima(Pista_y , # data
                order = c(4,0,0))
modelP

#new pista
newPista <- forecast(modelP)
newPista

#make dataframe for plotting
newPistaF <- data.frame(newPista)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistaF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Pistachios, aes(x = ymd(date), y = Et.in))+
  xlim(ymd(Pistachios$date[1]),newPistaF$dateF[24])+  # Plotting original data
  geom_line(data = newPistaF, aes(x = dateF, y = Point.Forecast),
            col="red") +
  geom_ribbon(data=newPistaF,
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#Corn fields
Corn_y <- na.omit(Corn_ts)
modelCorn <- arima(Corn_y , # data
                     order = c(1,0,0))
modelCorn

modelC <- arima(Corn_y ,
                order = c(4,0,0))
modelC

#Fallow
newCorn <- forecast(modelC)
newCorn

#make dataframe for plotting
newCornF <- data.frame(newCorn)

#dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newCornF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot
ggplot() +
  geom_line(data = Corn, aes(x = ymd(date), y = Et.in))+
  xlim(ymd(Corn$date[1]),newCornF$dateF[24])+
  geom_line(data = newCornF, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=newCornF,
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

Pista_y <- na.omit(Pistachios_ts)
modelPista <- arima(Pista_y , 
                    order = c(1,0,0))
modelPista

modelP <- arima(Pista_y , # data
                order = c(4,0,0))
modelP

#new pista
newPista <- forecast(modelP)
newPista

#make dataframe for plotting
newPistaF <- data.frame(newPista)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistaF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Pistachios, aes(x = ymd(date), y = Et.in))+
  xlim(ymd(Pistachios$date[1]),newPistaF$dateF[24])+  # Plotting original data
  geom_line(data = newPistaF, aes(x = dateF, y = Point.Forecast),
            col="red") +
  geom_ribbon(data=newPistaF,
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

