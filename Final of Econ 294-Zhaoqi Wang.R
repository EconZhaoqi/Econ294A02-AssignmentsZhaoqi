### Econ 294 Final ###
### Zhaoqi Wang ###
### 03/16/2016 ###

library("dplyr")
library("RSQLite")
library("nycflights13")
library("data.table")
library("mfx")
library("ggplot2")
my_db <- nycflights13_sqlite()
flights1<-flights%>% left_join(weather)
flights2<-flights1 %>% left_join(airports, c("dest" = "faa"))
flights3<-flights2%>%left_join(planes, by = "tailnum")
flights_sqlite <- copy_to(my_db, flights3, temporary = FALSE, indexes = list(
  c("year.x", "month", "day"), "carrier", "tailnum"))
flights = tbl(my_db, "flights3") %>% 
  collect() %>%
  mutate(canceled = is.na(arr_time))
# The above code is trying to merge several different dataframs into one,
# We have flights data, weather data, airports data and planes data.
# By careating a data base my_db, we could put all those different datas into this base.

#####
flightsnew <- dplyr::select(flights, year.x:day,dep_delay,dest,temp:visib,year.y:engine,canceled)
# The above code is trying to select theo variables we need.
reg1<-lm(dep_delay~temp,flightsnew)                   
summary(reg1)

graph1<-ggplot(
  data = flightsnew,
  aes(y=dep_delay,x = temp,size=wind_speed)
)+
  geom_point(aes(colour=visib), alpha=0.2)
graph1


# From the above regression result, we see that the coefficient of tempreture is 
# just about 0.18, which is pretty small, even though it is statistically significant,
# we can't conclude that tempreture will affect departure delay significantly.
# Plus, in the graph we could see that the variance of depature delay based on tempreture
# is not that huge, actually, it is pretty stable.
# Thus, we conclude that tempreture can't affect departure delay significantlly.
####################
reg2<-lm(dep_delay~wind_speed,flightsnew)                   
summary(reg2)
# From the regression above, we get the result that the coefficient of wind_speed is 
# about 0.751, which is pretty large, and it is statistically significant.
# When we are trying to plot the graph between dep_delay and wind_speed directly, 
# the graph are accumulated within a small range of wind_speed, which range within 250,
# when the wind_speed excesses 250, the line looks very flat, which apparently doesn't make 
# any sense. It is clear that if wind_speed is too large, the flight will be canceled.
# Thus, we should remove those outliers of wind_speed so that we could see a more clear
# relationship between dep_delay and wind_speed.

########################
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(flightsnew, "wind_speed", which(flightsnew$wind_speed > 100), NA)

graph2<-ggplot(
  data = flightsnew,
  aes(y=dep_delay,x = wind_speed,size=wind_speed)
)+
  geom_point(aes(colour=visib), alpha=0.2)
graph2
# After we remove those outliers, the graph looks way better than the original one.
# This time, compared with the one of tempreture, the variance of dep_delay is large,
# meaning that wind_speed does affect dep_delay significaintly.

##########################

reg3<-lm(dep_delay~humid,flightsnew)                   
summary(reg3)

qplot(humid,dep_delay, data = flightsnew,geom = "line",color = "red")

# The similar regression could be used to humid. The coefficient of humid is about 0.03,
# which is pretty small as well and the graph also shows us that the variance of dep_delay
# is relatively small, meaning that humid doesn't affect dep_delay that much.

###################
# Now we consider the size of the plane.

reg4<-lm(dep_delay~seats,flightsnew)                   
summary(reg4)

graph3<-ggplot(
  data = flightsnew,
  aes(y=dep_delay,x = seats)
)+
  geom_point(aes(colour=visib), alpha=0.2)
graph3
# Seats, as we could see from the regression result and the graph, won't affect dep_delay 
# that much. 

##################
probitmfx(canceled~temp+dewp+humid+wind_speed+visib+engines+seats,data=flightsnew,robust=T)

# Also we are interested in the relationship among canceled flights and all those variables.
# Since canceled is a dummy variable, we should use probit model to analize this relationship.
# We install mfx package and run the regression above.
# From the result, we see that dewp, wind_speed and engines affect canelation very much.
# They are all significant and the coefficients are all pretty large.



