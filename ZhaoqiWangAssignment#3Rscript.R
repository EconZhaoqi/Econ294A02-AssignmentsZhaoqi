### 0 ###
print("Zhaoqi Wang")
print(1504938)
print("zwang153@ucsc.edu")

### 1 ###

# load the following data and name it as "df.ex" #

library(foreign)
df.ex<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

install.packages("dplyr")

### 2 ###

# filter the data to just the last month of 2013

require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )

print(nrow(df.ex.2))

# filter the data to just the summer of 2013


df.ex.2prime <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2prime))


### 3 ###

# Arrange a new data with year and month ascending

df.ex.3a<-dplyr::arrange(df.ex, year, month)

### 4 ###

# Create a new data with columns from year through age

df.ex.4a<-dplyr::select(df.ex, year:age)

# Create a new data with columns with year month and those who starts with i

df.ex.4b<-dplyr::select(df.ex, year, month, starts_with("i"))

# print the distinct set of values in the original df.ex

distinct(select(df.ex,state))

### 5 ###

# Create a function stndz

stndz <- function(x){
  (x - mean(x, na.rm = T))  /  sd(x, na.rm = T)
}

# Create a function nrmlz

nrmlz<-function(x){
  (x-min(x, na.rm=T)) / (max(x, na.rm=T)-min(x, na.rm=T))
}

# Create a new data called df.ex.5a

df.ex.5a<-dplyr::mutate(df.ex, rw.stndz=stndz(rw),
                        rw_nrmlz=nrmlz(rw))

# Create a new data called df.ex.5b

df.ex.5B<-group_by(df.ex,year,month)
df.ex.5b<-mutate(df.ex.5B,rw.stndz=stndz(rw),rw_nrmlz=nrmlz(rw),count=n())

### 6 ###

# Create a new data called df.ex.6 and summarize

df.ex.6<-df.ex %>%
  dplyr::group_by(year,month,state) %>%
  summarise(min_rw=min(rw,na.rm=T),
            max_rw=max(rw,na.rm=T),
            mean_rw= mean(rw,na.rm=T),
            median_rw=median(rw,na.rm=T),
            rw_1stQnt=quantile(rw,0.25,na.rm=T),
            rw_3rdQnt=quantile(rw,0.75,na.rm=T),
            count=n())

# Use dplyr to find the year, month, state combination #
# with the highest mean real wage

high_meanofrw<-df.ex %>% dplyr :: group_by (year,month,state) %>% dplyr::summarise(
  rw_mean = mean(rw, na.rm=T))

highestmeanofrw<-dplyr::filter(
  high_meanofrw,
  rw_mean==max(high_meanofrw$rw_mean))

# Print

print(highestmeanofrw)


### 7 ###

# Create a new data called df.ex.7a with some ascending and 
# descending columns 

df.ex$state.char<- as.character(df.ex$state)
df.ex.7a<-arrange(df.ex,year,month, desc(state.char))


