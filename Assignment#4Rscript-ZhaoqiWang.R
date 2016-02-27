### 294B Assignment#4 ###
print("Zhaoqi") #First name#
print("Wang") #Last name#
print("1504938") #Student ID#
print("02/20/2016") #Time#
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
?select.list
require(dplyr)
flights.4b.delay3<-select(flights,contains("del"))

flights.4b.delay2<-select(flights,ends_with("delay"))

### part 5 ###
require(dplyr)
flights.5a<-flights%>%select(dep_delay)%>%arrange(desc(dep_delay))%>%head(5)

flights.5b<-flights%>%mutate(catchuptime=(dep_delay-arr_delay))%>%arrange(desc(catchuptime))%>%head(5)


### part 6 ###
## 6a ##
print(flights %>% dplyr::mutate(speed = dist / (time/60))%>%arrange(desc(speed)) %>%head(5))

## 6b ##
print(flights %>% mutate(delta=dep_delay-arr_delay)%>%arrange(desc(delta))%>%head(5))

## 6c ##
print(flights %>% mutate(delta=dep_delay-arr_delay)%>%arrange(delta)%>%head(1))

### part 7 ###
## 7a ##
flights.7a<-flights %>% mutate(delta=dep_delay-arr_delay) %>% group_by(carrier) %>%
  summarise (
    cancelled = sum(cancelled),
    total_flights = n(),
    percent_cancelled= (cancelled/total_flights),
    mindelta= min(delta, na.rm = T),
    quantile1 = quantile(delta, .25, na.rm = T),
    quantile2 = quantile(delta, .75, na.rm = T),
    meandelta= mean(delta, na.rm = T),
    median = median(delta, na.rm = T),
    quantile90 = quantile(delta, .90, na.rm = T),
    max = max(delta, na.rm = T)
  )
print(flights.7a%>%arrange(desc(percent_cancelled))%>%head(1))

## 7b ##
day_delay <- dplyr::filter(
  summarize(
    group_by(
      dplyr::filter(
        flights,
        !is.na(dep_delay)
      ),
      date
    ),
    delay = mean(dep_delay),
    n = n()
  ),
  n > 10
)
print("What we could get from this code is the mean of depature delay on each date.")


### part 8 ###

print(mutate(day_delay, difference_in_delay = delay-lag(delay)) 
      %>%arrange(desc(difference_in_delay))%>%head(5))

### part 9 ###

## 9a ##
dest_delay<-flights %>% group_by(dest) %>%
  summarise (mean = mean(arr_delay, na.rm = T),number_flights=n())

airports<-select(airports,dest = iata, name = airport , city,state, lat, long)

dest_delay %>% tbl_df

df.9a<- airports%>% left_join(dest_delay,by="dest")
df.9a %>% arrange(desc(mean)) %>% head(5)

## 9b ##
df.9b<- airports%>%inner_join(dest_delay,by="dest")
print("The number of observations via the left_join do not match those of the inner_join?")

## 9c ##
df.9c<- airports%>%right_join(dest_delay,by="dest")
print("116 observations.No NA")

## 9d ##
df.9d<- airports%>%full_join(dest_delay,by="dest")
print("there are 3387 observations in total.")

### part 10 ###

hourly_delay <- flights %>%filter(!is.na(dep_delay)) %>%
  group_by(date,hour) %>%summarize(delay=mean(dep_delay),n= n()) %>%filter(n>10)

question10<-hourly_delay %>%inner_join(weather,by=c("date","hour"))%>%arrange(desc(delay)) %>%
  tbl_df

print(question10$conditions[1:5])


### part 11 ###
## 11a ##
require(tidyr)
require(dplyr)


df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df

df %>% gather(subject, value, -treatment) %>% mutate(subject = subject %>% substr(8,9)) %>% select(subject, treatment, value)

## 11b ##
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df

df %>% spread( key = subject, value = value) %>%rename(subject1 = `1`, subject2 = `2`)






