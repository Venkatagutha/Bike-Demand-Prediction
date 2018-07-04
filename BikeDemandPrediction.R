test<- read.csv("test.csv")
train<-read.csv("tr.csv")
#Combining both the dataset

test$casual<-0
test$registered<-0
test$count<-0

data<-rbind(train,test) # now we have the complete data set to work on.
#data preperation
data$season<-as.factor(data$season)
data$holiday<-as.factor(data$holiday)
data$workingday<-as.factor(data$workingday)
data$weather<- as.factor(data$weather)

#extracting hour from date time
data$hour<- substr(data$datetime,12,13)
data$hour<-as.factor(data$hour)
data$hour<-as.numeric(data$hour)

#extracting days of the week
date<-substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day<-days
data$day<-as.factor(data$day)
data$day<-as.numeric(data$day)
# quarter
quarter<- as.yearqtr(as.Date(date))
data$quarter<-quarter
data$quarter<-as.factor(data$quarter)
data$quarter<-as.numeric(data$quarter)

# assigning holidays and weekends
data$weekend<-0
data$weekend[data$day=="Sunday" | data$day=="Saturday"]<-1
data$weekend<-as.factor(data$weekend)
data$weekend<-as.numeric(data$weekend)
# seperating day type
data$day_type<-0
data$day_type[data$workingday==1 & data$holiday==0]<-"Working Day"
data$day_type[data$holiday==1]<-"Holiday"
data$day_type[data$holiday==0 & data$workingday==0]<-"Weekend"
data$day_type<-as.factor(data$day_type)
data$day_type<-as.numeric(data$day_type)
#assigning years
data$year<-format(as.Date(data$datetime),"%y")
data$year[data$year==11]<- 2011
data$year[data$year==12]<-2012
data$year<-as.factor(data$year)
data$year<-as.numeric(data$year)
#assigning months

data$month<-format(as.Date(data$datetime),"%m")
data$month<- revalue(data$month, c("01"= "01-January", "02"= "02-February",
                                   "03"="03-March", "04"="04-April", "05"="05-May",
                                   "06"="06-June","07"="07-July", "08"="08-August",
                                   "09"="09-September", "10"="10-October",
                                   "11"="11-November", "12"="12-December"))
data$month<-as.factor(data$month)
data$month<-as.numeric(data$month)

train1<-data[as.integer(substr(data$datetime,9,10))<20,]
test1<-data[as.integer(substr(data$datetime,9,10))>19,]

train1$hour<-as.integer(train1$hour)#converting to integer
test1$hour<-as.integer(test1$hour)


train1$temp<-as.integer(train1$temp)#converting to integer
test1$temp<-as.integer(test1$temp)

library(rpart)
library(rpart.plot)

tree_reg<-  rpart(registered ~ hour, data = train1)#reg
rpart.plot(tree_reg)

tree_cas<-  rpart(casual ~ hour, data = train1)#cas
rpart.plot(tree_cas)


data<-rbind(train1,test1)#-combined the data set again

#bins for regular-time
data$time_reg<-0
data$time_reg[data$hour<8]=1
data$time_reg[data$hour>=22]=2
data$time_reg[data$hour>9 & data$hour<18]=3
data$time_reg[data$hour==8]=4
data$time_reg[data$hour==9]=5
data$time_reg[data$hour==20 | data$hour==21]=6
data$time_reg[data$hour==19 | data$hour==18]=7
#bins for casual-time
data$time_cas<-0
data$time_cas[data$hour<9]<-1
data$time_cas[data$hour==9]<-2
data$time_cas[data$hour>=10 & data$hour<20]<-3
data$time_cas[data$hour>=20]<-4

# -----temperature
tree_t_reg<- rpart(registered~ temp, data = train1)#reg
rpart.plot(tree_t_reg)

tree_t_cas<- rpart(casual ~ temp, data = train1)#cas
rpart.plot(tree_t_cas)

data<-rbind(train1,test1)#-combined the data set again
#bin creation for regular-temp
data$temp_reg[data$temp<12]<-1
data$temp_reg[data$temp>=12 & data$temp<20]<-2
data$temp_reg[data$temp>=20 & data$temp<28]<-3
data$temp_reg[data$temp>=28]<-4  
#bin creation for casual-temp
data$temp_cas[data$temp<14]<-1
data$temp_cas[data$temp>=14 & data$temp<22]<-2
data$temp_cas[data$temp>=22 & data$temp<30]<-3
data$temp_cas[data$temp>=30]<-4
#-----as.factor
train1$hour<-as.factor(train1$hour)
test1$hour<-as.factor(test1$hour)

train1$weather<-as.factor(train1$weather)
test1$weather<-as.factor(test1$weather)

train1$season<-as.factor(train1$season)
test1$season<-as.factor(test1$season)

train1$holiday<-as.factor(train1$holiday)
test1$holiday<-as.factor(test1$holiday)

train1$workingday<-as.factor(train1$workingday)
test1$workingday<-as.factor(test1$workingday)

train1$month<-as.factor(train1$month)
test1$month<-as.factor(test1$month)

train1$day<-as.factor(train1$day)
test1$day<-as.factor(test1$day)

train1$day_type<-as.factor(train1$day)
test1$day_type<-as.factor(test1$day_type)
#-building the model
reg<- randomForest(registered~ hour + workingday +day + weather+ season+ holiday+
                     month+ day_type+ atemp + humidity + windspeed+
                     time_reg + temp_reg,data = train1, ntree=250, importance=T)
reg_pred<- predict(reg,test1)
test1$pred1<-reg_pred
#-casual
cas<- randomForest(casual~ hour + workingday +day + weather+ season+ holiday+
                     month+ day_type+ atemp + humidity + windspeed+
                     time_cas + temp_cas,data = train1, ntree=250, importance=T)
cas_pred<-predict(cas, test1)
test1$pred2<-cas_pred
test1$count= test1$pred1+test1$pred2
submit<-data.frame(datetime=test1$datetime,count=test1$count)
write.csv(submit,file = "submit.csv",row.names = F)
