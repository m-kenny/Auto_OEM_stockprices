
#install.packages("Quandl")
library(Quandl)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotly)
library(data.table)

Sys.setlocale("LC_TIME")

#import API data
DAI <- Quandl('FSE/DAI_X', api_key="godguysGzuHZEMGkx9am")
BMW <- Quandl('FSE/BMW_X', api_key="godguysGzuHZEMGkx9am")
VW <-  Quandl('FSE/VOW3_X', api_key="godguysGzuHZEMGkx9am")

#setting column names 
names(BMW) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Change', 'Volume', 'Turnover', 'LastPriceofDay', 'DailyTradedUnits', 'DailyTurnover')
names(DAI) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Change', 'Volume', 'Turnover', 'LastPriceofDay', 'DailyTradedUnits', 'DailyTurnover')
names(VW) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Change', 'Volume', 'Turnover', 'LastPriceofDay', 'DailyTradedUnits', 'DailyTurnover')

#OEM overall graph

#Analyze BMW Stock
volume <-  ggplot()+
  geom_bar(data = DAI, aes(x= Date, y= Volume, fill = 'DAI'), stat = 'identity')+
  geom_bar(data = BMW, aes(x= Date, y= Volume, fill = 'BMW'), stat = 'identity')+
  geom_bar(data = VW, aes(x= Date, y= Volume, fill = 'VW'), stat = 'identity')
volume

  OEM <- ggplot(DAI, aes(Date, Close))+
          geom_line(aes(colour = 'DAI'))+
          geom_line(data = BMW, aes(colour = 'BMW')) +
          geom_line(data = VW, aes(colour = 'VW'))+
          xlab('Date')+
          ylab('Closing Price')+
          labs(colour= "Legend")+
          ggtitle("Closing Stock Prices: Daimler, BMW, VW")+
          theme(plot.title = element_text(lineheight = 0.7, face= 'bold'))

OEM

ggplotly(OEM, dynamicTicks = TRUE) %>%
  rangeslider()


#Anaylze Daimler Stock basics
DAIM <- ggplot()+
    geom_line(data= DAI, aes(x= Date, y= Close,color = 'Close', frame = Date))+
    geom_line(data= DAI, aes(x=Date, y=High,color= "High",frame = Date) ,lty = 3)+
    geom_line(data= DAI, aes(x= Date, y= Low, color = 'Low',frame = Date), lty = 3)+
    labs(title = 'Daimler', x= "Date", y= "Value" )
DAIM
ggplotly(DAIM)

#Daimler
View(DAI)

DAIM <- ggplot(data = DAI, aes(x= Date, y= Close), colour = 'black')+
  geom_line()+
  labs(title = "Daimler daily returns")
DAIM


#the Max Day
max(DAI$Close)
DAI[which.max(DAI$Close),]

#the Max Day
min(DAI$Close)
DAI[which.min(DAI$Close),]

#highest fluctuation day
DAI <-DAI %>% 
      mutate(flucP = Open - Close)

ggplot(DAI, aes(Date, flucP))+
  geom_line()


#Fluction of day
max(DAI$flucP)
DAI[which.max(DAI$flucP),]

min(DAI$flucP)
DAI[which.min(DAI$flucP),]

ggplot(DAI, aes(Date, Turnover))+
  geom_line()


#Fridays for DAIMLER
Fri <- DAI %>%
  filter(wday(Date) == 5) %>%
  arrange(Date)

View(Fri)
#checks
FriNA <- Fri %>%
  filter(is.na(Fri$Close))
nrow(FriNA)

#Graph of week ending of DAIMLER
Friday <- ggplot(Fri, aes(Date, Close))+
  geom_line()+
  labs(title = 'Week ending of DAIMLER')

ggplotly(Friday)


require(data.table)
setDT(Fri)
maxDiff <- Fri[, diff := Close - shift(Close)]

Fri <- as.data.frame(Fri)
#View(Fri)

#maxs
max(Fri$diff, na.rm= T)
Fri[which.max(Fri$diff),]  

Fri %>%
  filter(between( Date, as.Date('2001-09-20'), as.Date('2001-10-04')))

#Mins 
min(Fri$diff, na.rm= T) 
Fri[which.min(Fri$diff),]  

Fri %>%
  filter(between( Date, as.Date('2015-09-15'), as.Date('2015-10-11')))

#month
Months <- DAI %>%
  group_by(Month) %>%
  filter(month(Date)) %>%
  arrange(Date)

setDT(DAI)

DAI[, LastFridayInMonth := wday(Date) == 5 & Date == max(Date), by = .(year(Date), month(Date), weekdays(Date))]
DAI$Month <- month(DAI$Date)
View(DAI)
#Last Friday of the month analysis.
LastFriDAI<- DAI %>%
            filter(LastFridayInMonth =='TRUE')

View(LastFriDAI)
ggplot(LastFriDAI, aes(Date, Close))+
  geom_line()+
  ggtitle("Monthly Daimler")

require(data.table)
LastFriDAI <- data.table(LastFriDAI)
maxDiff <- LastFriDAI[, diff := Close - shift(Close)]


#max and mins
max(LastFriDAI$diff, na.rm= T)
LastFriDAI[which.max(LastFriDAI$diff),] 
LastFriDAI[which.min(LastFriDAI$diff),] 



#BMW
BMW <- BMW  %>%
  arrange(Date)

BMWG <- ggplot(data = BMW, aes(x= Date, y= Close), colour = 'black')+
  geom_line()+
  labs(title = "BMW Daily returns")
ggplotly(BMWG)



#the Max Day
max(BMW$Close)
BMW[which.max(BMW$Close),]

#the Max Day
min(BMW$Close)
BMW[which.min(BMW$Close),]

#highest fluctuation day
BMW <-BMW %>% 
  mutate(flucP = Open - Close)


#Fluction of day
max(BMW$flucP)
BMW[which.max(BMW$flucP),]

min(BMW$flucP)
BMW[which.min(BMW$flucP),]

#ggplot(BMW, aes(Date, Turnover))+
 # geom_line()


#Fridays 

Fri <- BMW %>%
  filter(wday(Date) == 5) %>%
  arrange(Date)

FriNA <- Fri %>%
  filter(is.na(Fri$Close))

nrow(FriNA)

Friday <- ggplot(Fri, aes(Date, Close))+
  geom_line()+
  labs(title = "Weekly Stock return BMW")

ggplotly(Friday)


require(data.table)
Fri <- data.table(Fri)
maxDiff <- Fri[, diff := Close - shift(Close)]

Fri <- as.data.frame(Fri)
#View(Fri)

#maxs
max(Fri$diff, na.rm= T)
Fri[which.max(Fri$diff),]  

Fri %>%
  filter(between( Date, as.Date('2016-02-20'), as.Date('2016-03-03')))

#Mins 
min(Fri$diff, na.rm= T) 
Fri[which.min(Fri$diff),]  

Fri %>%
  filter(between( Date, as.Date('2015-09-15'), as.Date('2015-09-24')))


#month
Months <- BMW %>%
  group_by(Month) %>%
  filter(month(Date)) %>%
  arrange(Date)

setDT(BMW)
BMW[, LastFridayInMonth := wday(Date) == 5 & Date == max(Date), by = .(year(Date), month(Date), weekdays(Date))]
BMW$Month <- month(BMW$Date)

#Last Friday of the month analysis.
LastFriBMW <- BMW %>%
  filter(LastFridayInMonth =='TRUE')


ggplot(LastFriBMW , aes(Date, Close))+
  geom_line()+
  ggtitle("Monthly BMW")


require(data.table)
setDT(LastFriBMW)
maxDiff <- LastFriBMW[, diff := Close - shift(Close)]
head(LastFriM)

#max and mins
max(LastFriBMW $diff, na.rm= T)
LastFriBMW[which.max(LastFriBMW$diff),] 
LastFriBMW[which.min(LastFriBMW$diff),] 

LastFriBMW%>%
  filter(between( Date, as.Date('2015-08-15'), as.Date('2015-11-01')))

#VW
VW <- VW  %>%
  arrange(Date)
VWG <- ggplot(data = VW, aes(x= Date, y= Close), colour = 'black')+
  geom_line()+
  labs(title = "Daily retruns VW")
VWG

ggplotly(VWG)


#the Max Day
max(VW$Close)
VW[which.max(VW$Close),]

#the Max Day
min(VW$Close)
VW[which.min(VW$Close),]

#highest fluctuation day
VW <-VW %>% 
  mutate(flucP = Open - Close)


#Fluction of day
max(VW$flucP)
VW[which.max(VW$flucP),]

min(VW$flucP)
VW[which.min(VW$flucP),]

ggplot(VW, aes(Date, Turnover))+
  geom_line()+
  labs(title = "Turnover of stock per day_ VW")


#Fridays 

FriVW <- VW %>%
  filter(wday(Date) == 5) %>%
  arrange(Date)

FriNA <- FriVW %>%
  filter(is.na(Fri$Close))

nrow(FriNA)

FridayVW <- ggplot(FriVW, aes(Date, Close))+
  geom_line()+
  labs(title = "Weekly returns VW")
ggplotly(FridayVW)


require(data.table)
FriVW<- data.table(FriVW)
maxDiff <- FriVW[, diff := Close - shift(Close)]

FriVW <- as.data.frame(FriVW)
View(FriVW)

#maxs
max(FriVW$diff, na.rm= T)
FriVW[which.max(FriVW$diff),]  

FriVW %>%
  filter(between( Date, as.Date('2015-11-18'), as.Date('2015-11-26')))

#Mins 
min(FriVW$diff, na.rm= T) 
FriVW[which.min(FriVW$diff),]  

FriVW %>%
  filter(between( Date, as.Date('2015-09-15'), as.Date('2015-09-24')))


#month
setDT(VW)
VW[, LastFridayInMonth := wday(Date) == 5 & Date == max(Date), by = .(year(Date), month(Date), weekdays(Date))]
VW$Month <- month(VW$Date)

#Last Friday of the month analysis.
LastFriMVW <- VW %>%
  filter(LastFridayInMonth =='TRUE')


ggplot(LastFriMVW, aes(Date, Close))+
  geom_line()+
  ggtitle("Monthly VW")

require(data.table)
LastFriMVW <- data.table(LastFriMVW)
maxDiff <- LastFriMVW[, diff := Close - shift(Close)]
head(LastFriMVW)

#max and mins
max(LastFriMVW$diff, na.rm= T)
LastFriMVW[which.max(LastFriMVW$diff),] 
LastFriMVW[which.min(LastFriMVW$diff),] 

names(BMW)
names(VW)
names(DAI)
BMW$company <- 'BMW'
VW$company <- 'VW'
DAI$company <- 'DAI'
OEM <- rbind(BMW, VW, DAI)
OEM$MY <- format(as.Date(OEM$Date), "%Y-%m")


OEMplot <- ggplot()+
  geom_line(data= LastFriDAI, aes(x=Date, y=Close, colour='DAI'))+
  geom_line(data=LastFriBMW, aes(x=Date, y=Close, colour= 'BMW'))+
  geom_line(data=LastFriMVW, aes(x=Date, y=Close, colour = 'VW'))+
  labs(title = "Weekly Closing Prices for FSE")

OEMplot
      

         