###294B Assignment#1############
print("Zhaoqi") #First name#
print("Wang") #Last name#
print("1504938") #Student ID#
print("01/14/2016") #Time#
###Load Data#############
###data1#####
library(foreign) 
df.dta <- read.dta(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta"
)

####data2#####
df.csv <- read.csv(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv"
)

####data3####
df.td <- read.table(
  file = "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt"
)

####data4####
load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))
#######
print("The name of RData file is NHIS_2007_RData. dta is 452.3125KB, CSVdata is 188.52343KB, td is 506.390KB, Rdata is 188.52343KB. 
      The smallest data is CSVdata and Rdata. Finally, becides .dta file,
      in df.td file, the name of each column is Vi rather than real names")
###3###
rdata<-NHIS_2007_RData

print(typeof(rdata))
print(class(rdata))
print("as we could see from the result,
      the type of rdata is 'list', and the class of rdata is 'data.frame'")
print(length(rdata))
print(dim(rdata))
print(nrow(rdata))
print(ncol(rdata))

summary(rdata)

print("the length of rdata is 9, the dim of rdata is 4785 & 9,
      the nrow of rdata is 4785 and the ncol of rdata is 9,
      and finally we could also see the summary of rdata after running the above code")


###4###
library(foreign) 
df.dta <- read.dta(
  file = "http://people.ucsc.edu/~aspearot/Econ_217_Data/org_example.dta"
)

df<-df.dta

str(df)
print("there are 1119754 observations and 30 variables.")
summary(df$rw)
print("the min of rw is 1.8, the mean of rw is 19.8,
       the median of rw is 15.9,and the max of rw is 354.8")


####5#####

?vector

c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)

vector<-c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)

print(length(vector))

print("the length of the vector is 9, because the 'null'
      won't match, it will be automatically canceled out,
      therefore, the number won't match the length")


as.numeric(vector)

?mean
mean(
  as.numeric(vector)
  ,na.rm=TRUE)
print("the mean ignoring the NA is 4")

####6############

?matrix

M<-matrix(1:9, ncol=3, nrow=3, byrow=TRUE)
print(M)

###Transpose###
Mtranspose<-matrix(1:9, ncol=3, nrow=3, byrow=FALSE)
print(Mtranspose)


print(eigen(M))
print("after we run the code above, we could see
      the eigenvalues and eigenvectors")

####Matrix-Y###

Y<-matrix(c(1,2,3,3,2,1,2,3,0), ncol=3, nrow=3, byrow=TRUE)
print(Y)


Yinverse<-solve(Y)

print(Yinverse)

I<-Yinverse %*% Y

print(I)

print("In algebra, the new matrix is called 'identity matrix'")


#################7##################
###1###
diamonds<-data.frame(carat=c(5,2,0.5,1.5,5,"NA",3), 
              cut=c("fair","good","very good","good","fair","Ideal","fair"),
              clarity=c("SI1","I1","VI1","VS1","IF","VVS2","NA"),
              price=c(850,450,450,NA,750,980,420))

print(diamonds)

mean(diamonds$price,na.rm = T)
print("because there is 'NULL'(NA is this case) in price,
      we must remove it, and get the mean, which is 650")

###2###
diamonds2<-subset(diamonds,(cut=="fair"))

print(diamonds2)

mean(diamonds2$price)
print("I created a subset of the original frame and only put those who 
      has 'fair'cut into it, and then calculate the mean, which is 673.3333")

###3###
diamonds3<-subset(diamonds,(cut!="fair"))

print(diamonds3)

mean(diamonds3$price,na.rm=T)
print("I created another subset which satisfies the requirement,
      of the question, and calculated the mean, which is 626.6667")

###4###
diamonds4<-subset(diamonds,(carat>2 & cut=="Ideal"& cut=="very good"))

print(diamonds4)

print ("From the original frame, there are only one 'very good',
      and only one 'Ideal';however, those two diamonds only have
       carat 0.5 and 'NA', which means we can't find a subset
       satisfies the requirement of the problem. Therefore, there
       is no median price in this case.")






