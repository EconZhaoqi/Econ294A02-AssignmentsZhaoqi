# Econ 294 Homework#5 #
# By Zhaoqi Wang
# 02/26/2016

# Problem #1 


# a
library(ggplot2)

parta<-ggplot(diamonds,aes(x*y*z,price))+geom_point(aes(size=carat, colour=clarity), alpha=0.15)+scale_x_log10()+scale_y_log10()

parta
# b
partb<-ggplot(diamonds, aes(carat, ..density..))+geom_histogram(aes(fill=clarity),bins=25)

partb+facet_grid(cut ~ .)
+stat_bin()


# c

partc<-ggplot(diamonds,aes(cut,price))+geom_violin(trim=T)+geom_jitter(alpha=0.025)
partc


# Problem 2
# a 
library(foreign)
df.ex<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")


require(dplyr)

df.ex.plotprep<-df.ex %>%
  group_by(year,month) %>%
  summarize(
    rw.med = median(rw, na.rm = T),
    rw.90 = quantile(rw, 0.9,na.rm = T ),
    rw.10 = quantile(rw, 0.1,na.rm = T ),
    rw.25 = quantile(rw, 0.25,na.rm = T ),
    rw.75 = quantile(rw, 0.75,na.rm = T )
  ) %>%
  mutate(
    date = paste (year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )
    

ggplot(
  df.ex.plotprep,
  aes(
    x=date, y =rw.med
  )
)+
  geom_line()+
  geom_ribbon(aes(ymin=rw.10, ymax=rw.90),alpha=0.2)+
  geom_ribbon(aes(ymin=rw.25, ymax=rw.75),alpha=0.5)

view(df.ex.plotprep)
df.ex.plotprep


# b

df.ex3<-df.ex %>%
  group_by(year,month,educ) %>%
  summarise(
    rw.med=median(rw, na.rm=T),
    rw.90=quantile(rw,0.9,na.rm=T),
    rw.10=quantile(rw,0.1,na.rm=T),
    rw.25=quantile(rw,0.25,na.rm=T),
    rw.75=quantile(rw,0.75,na.rm=T)
  ) %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )

ggplot(
  df.ex3,
  aes(x=date,y=rw.med,colour=educ)
) +
  geom_line()

