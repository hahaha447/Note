library(ggplot2)
library(tidyverse)
library(ggmap)
library(dplyr)
library(forcats)
crime <- read.table("crime.csv",sep = ",",header = T)
mean(crime$Latitude)
mean(crime$Longitude)

register_google("API-key")
googlemap <- get_map(location =c(lon = -118.34, lat = 34.06), zoom = 10,
                     scale = "auto",  maptype = c("terrain"))
a <- ggmap(googlemap)
b <- a+stat_density_2d(data = crime, aes(Longitude,Latitude,fill = ..level.., alpha = ..level..)
                       ,size = 0.05, bins = 70, geom = "polygon") +
  scale_fill_gradient2(low = "white",mid="yellow", high = "red")+
  scale_alpha(range = c(0.1, 0.4), guide = FALSE)
c <- b+labs(title="Crime rate")+
  theme(plot.title = element_text(hjust = 0.5))
c

filtered_crimes <-select(crime,c(Victim.Sex,Victim.Age)) %>% 
  filter(Victim.Sex=="M"|Victim.Sex=="F")

ggplot(data = filtered_crimes,aes(x=Victim.Age,fill=Victim.Sex))+
  geom_histogram(position ="identity",alpha=0.7,col="grey")+
  labs(title="histogram for Victim's age for different sex")+
  theme(plot.title = element_text(hjust=0.5))

ggplot(data = filtered_crimes,aes(y=Victim.Age,x=Victim.Sex,fill=Victim.Sex))+
  geom_violin(scale = "count",col="black")

ggplot(data =filtered_crimes,aes(x="", y=stat(count),fill=Victim.Sex))+
  geom_histogram(stat = "count",  width=1,col="black") +
  coord_polar(theta = "y",start = 0)+
  labs(title="Pie plot for victim's sex")+theme(plot.title=element_text(hjust=0.5))


library(xts)
Timeseries <-select(crime,Date.Occurred)%>%
  table(dnn =  "Date.Occurred")%>%
  as.data.frame()

Timeseries$Date.Occurred <- as.Date(Timeseries$Date.Occurred,format ="%m/%d/%Y")
Timeseries <- xts(x = Timeseries, order.by = Timeseries$Date.Occurred)

ggplot(Timeseries, aes(x=as.POSIXct(Date.Occurred), as.numeric(Freq)))+
  geom_point()+
  stat_smooth(colour="salmon",size=2)+
  labs(title="How many crimes in each day")+theme(plot.title=element_text(hjust=0.5))

