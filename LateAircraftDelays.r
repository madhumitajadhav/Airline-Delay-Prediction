Apr_data<-read.csv("D:/R/Airline Data/Pushkar sir Airline data/2015/Apr2015.csv")
Apr_data<-Apr_data[which(Apr_data$CARRIER_DELAY!=""),]
library(dplyr)
Apr_data<-Apr_data[,c(4,6,7,8,13,21,26:35,39:47)]

ex<-Apr_data %>% group_by(DAY_OF_MONTH,TAIL_NUM) %>% arrange(CRS_DEP_TIME)%>% arrange(DAY_OF_MONTH,TAIL_NUM)
ex<-as.data.frame(ex)

names(ex)
table(ex$LATE_AIRCRAFT_DELAY)
table(ex$CARRIER_DELAY)
table(ex$WEATHER_DELAY)
table(ex$NAS_DELAY)
table(ex$SECURITY_DELAY)

###############################
## Group of 1

dim(ex[which(ex$SECURITY_DELAY!=0),])
dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0),])
dim(ex[which(ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$NAS_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0),])

## Group of 2

dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$NAS_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$SECURITY_DELAY!=0),])

dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0 & ex$WEATHER_DELAY!=0 ),])
dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 ),])
dim(ex[which(ex$LATE_AIRCRAFT_DELAY!=0 & ex$SECURITY_DELAY!=0 ),])

dim(ex[which(ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0),])

dim(ex[which(ex$SECURITY_DELAY!=0 & ex$WEATHER_DELAY!=0),])

## Group of 3

dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$SECURITY_DELAY!=0),])


dim(ex[which( ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0),])
dim(ex[which( ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])

dim(ex[which( ex$SECURITY_DELAY!=0 & ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])


## Group of 4
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$WEATHER_DELAY!=0),])
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0),])

## Group of 5
dim(ex[which(ex$CARRIER_DELAY!=0 & ex$LATE_AIRCRAFT_DELAY!=0 & ex$NAS_DELAY!=0 & ex$SECURITY_DELAY!=0 &ex$WEATHER_DELAY!=0),])
#####################

ex$CRS_DEP_TIME <- substr(as.POSIXct(sprintf("%04.0f", ex$CRS_DEP_TIME), format='%H%M'), 12, 16)
ex$ARR_TIME <- substr(as.POSIXct(sprintf("%04.0f", ex$ARR_TIME), format='%H%M'), 12, 16)

ex$a<-as.POSIXlt(paste0(ex$FL_DATE," ",ex$CRS_DEP_TIME,":00.000"))
ex$b<-as.POSIXlt(paste0(ex$FL_DATE," ",ex$ARR_TIME,":00.000"))
ex$c[1]<-0

for(i in seq(1:(length(ex$a)-1)))
{
  subtraction<-difftime(ex$a[i+1],ex$b[i],units="mins")
  print(difftime(ex$a[i+1],ex$b[i],units="mins"))
  ex$c[i+1]<-subtraction
}

k=0
l=0
ex$flag[1]<-"Null"
for(i in seq(1:(length(ex$c)-1)))
{
  if(ex$DAY_OF_MONTH[i]==ex$DAY_OF_MONTH[i+1] & ex$TAIL_NUM[i]==ex$TAIL_NUM[i+1])
  {
    k<-k+1
    ex$flag[i+1]<-"Yes"
  }
  else
  {
    l<-l+1
    ex$flag[i+1]<-"No"
  }
}

print(k)
print(l)

head(ex)
head(ex[,c(1,4,15,7,28,29,25,21:23)],20)

names(ex)
