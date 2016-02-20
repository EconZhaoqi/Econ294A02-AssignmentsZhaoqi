### 294B Assignment#4 ###
print("Zhaoqi") #First name#
print("Wang") #Last name#
print("1504938") #Student ID#
print("02/19/2016") #Time#
### Part 1 ###
?read.csv



airports<- read.csv(
  file = "C:/Users/Zhaoqi/Desktop/2016WINTER/Econ294B/Assignment4/airports.csv", stringsAsFactors = FALSE
)

flights<- read.csv(
  file = "C:/Users/Zhaoqi/Desktop/2016WINTER/Econ294B/Assignment4/flights.csv", stringsAsFactors = FALSE
)

planes<- read.csv(
  file = "C:/Users/Zhaoqi/Desktop/2016WINTER/Econ294B/Assignment4/planes.csv", stringsAsFactors = FALSE
)

weather<- read.csv(
  file = "C:/Users/Zhaoqi/Desktop/2016WINTER/Econ294B/Assignment4/weather.csv", stringsAsFactors = FALSE
)

### part 2 ###
flights$date<-as.Date(flights$date)
weather$date<-as.Date(weather$date)

### part 3 ###
flights.2a<-subset(flights,(dest=="SFO" | dest=="OAK"))
print(nrow(flights.2a))

flights.2b<-subset(flights,(dep_delay>="1" | arr_delay>="1"))
print(nrow(flights.2b))

flights.2c<-subset(flights,(arr_delay>=2*dep_delay))
print(nrow(flights.2c))

### part 4 ###


### part 5 ###
require(dplyr)
flights.5a<-flights%>%select(dep_delay)%>%arrange(desc(dep_delay))%>%head(5)

flights.5b<-flights%>%mutate(catchuptime=(dep_delay-arr_delay))%>%arrange(desc(catchuptime))%>%head(5)

### part 6 ###


### part 7 ###



