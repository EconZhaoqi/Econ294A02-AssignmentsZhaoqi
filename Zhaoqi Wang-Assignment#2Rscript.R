#######Econ 294 Homework Assignment#2  ##############
######By Zhaoqi Wang##############
######### 01/22/2016#############

##########Part#0 Identifying Information#############
ZhaoqiWangAssignment2<-list(
  firstname = "Zhaoqi",
  lastname = "Wang",
  email = "zwang153@ucsc.edu",
  studentID = 1504938
  
) 

load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData"))

###Part#1#######

ZhaoqiWangAssignment2$s1a<-nrow(diamonds)
print(ZhaoqiWangAssignment2$s1a)

ZhaoqiWangAssignment2$s1b<-ncol(diamonds)
print(ZhaoqiWangAssignment2$s1b)

ZhaoqiWangAssignment2$s1c<-names(diamonds)
print(ZhaoqiWangAssignment2$s1c)

ZhaoqiWangAssignment2$s1d<-summary(diamonds$price)
print(ZhaoqiWangAssignment2$s1d)

##########Part#2###############

df.td <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt", header=T
)

ZhaoqiWangAssignment2$s2a<-nrow(df.td)
print(ZhaoqiWangAssignment2$s2a)

ZhaoqiWangAssignment2$s2b<-ncol(df.td)
print(ZhaoqiWangAssignment2$s2b)

ZhaoqiWangAssignment2$s2c<-names(df.td)
print(ZhaoqiWangAssignment2$s2c)

ZhaoqiWangAssignment2$s2d<-mean(df.td$weight)
print(ZhaoqiWangAssignment2$s2d)

ZhaoqiWangAssignment2$s2e<-median(df.td$weight)
print(ZhaoqiWangAssignment2$s2e)
                                  
hist(df.td$weight)

table(df.td$weight)

df.td$adjweight<-ifelse(df.td$weight>=996&df.td$weight<=999,NA,df.td$weight)

ZhaoqiWangAssignment2$s2f<-mean(df.td$adjweight,na.rm=T)
print(ZhaoqiWangAssignment2$s2f)

ZhaoqiWangAssignment2$s2g<-median(df.td$adjweight,na.rm=T)
print(ZhaoqiWangAssignment2$s2g)

ZhaoqiWangAssignment2$s2h<-summary(df.td$adjweight,df.td$SEX==1)
print(ZhaoqiWangAssignment2$s2h)

ZhaoqiWangAssignment2$s2i<-summary(df.td$adjweight,df.td$SEX==2)
print(ZhaoqiWangAssignment2$s2i)


###########part#3##################

vec<-c(letters,LETTERS)
vec
ZhaoqiWangAssignment2$s3a<-vec[seq(2,52,2)]
print(ZhaoqiWangAssignment2$s3a)

ZhaoqiWangAssignment2$s3b<-paste(vec[c(52,8,1)],collapse="")
print(ZhaoqiWangAssignment2$s3b)


arr<-array(c(letters,LETTERS), dim = c(3,3,3))
arr

ZhaoqiWangAssignment2$s3c<-arr[1:3,1,2]
print(ZhaoqiWangAssignment2$s3c)

ZhaoqiWangAssignment2$s3d<-arr[2,2,1:3]
print(ZhaoqiWangAssignment2$s3d)

ZhaoqiWangAssignment2$s3e<-paste(arr[2,3,3],arr[2,3,1],arr[1,1,1],sep="")
print(ZhaoqiWangAssignment2$s3e)

ZhaoqiWangAssignment2

