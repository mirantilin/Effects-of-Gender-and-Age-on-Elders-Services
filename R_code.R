{\rtf1\ansi\ansicpg1252\cocoartf1561\cocoasubrtf400
{\fonttbl\f0\froman\fcharset0 Times-Roman;\f1\froman\fcharset0 TimesNewRomanPSMT;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\ri720\partightenfactor0

\f0\fs24 \cf0 setwd("/Users/Linkin/Desktop/Spring 2016/Math 336/Project")\
c4<-read.delim("ClientVitalStat1314.csv", header=TRUE, sep=",")\
c5<-read.delim("ClientVitalStat1415.csv", header=TRUE, sep=",")\
c6<-read.delim("ClientVitalStat1516.csv", header=TRUE, sep=",")\
View(c4)\
stc<-read.delim("STCClientVitalStat1214v2.csv", header=TRUE, sep=",")\
inds<-c4$LC3SAID\
inds<-union(inds,c5$LC3SAID)\
inds<-union(inds,c6$LC3SAID)\
inds<-union(inds,stc$LC3SAID)\
c4$entime<-strptime(c4$Enter, "%m/%d/%y")\
c4$extime<-strptime(c4$Exit, "%m/%d/%y")\
View(c4)\
View(stc)\
class("Mood.Behavior")\
level("Mood.Behavior")\
stc$sex<-sapply(stc$Sex,function(x)\{ifelse(x=="cf","F",ifelse(x=="Cf","F",ifelse(x=="F","F",ifelse(x=="f","F",ifelse(x=="cm","M",ifelse(x=="CM","M",ifelse(x=="m","M",ifelse(x=="M","M",NA))))))))\})\
levels("Sex")\
evels(stc$sex)\
levels(as.factor(stc$sex))\
levels(as.factor(stc$Mood.Behavior))\
stc$mood.behavior<-sapply(stc$Mood.Behavior,function(x)\{ifelse(x=="no","N",ifelse(x=="n","N",ifelse(x=="No","N",ifelse(x=="N","N",ifelse(x=="y","Y",ifelse(x=="yes","Y",ifelse(x=="YES","Y",ifelse(x=="Y","Y",NA))))))))\})\
levels(as.factor(stc$Mood.Behavior))\
levels(as.factor(stc$mood.behavior))\
Table1 <-table(stc$mood.behavior,stc$sex)\
mosaicplot(Table1)\
table(stc$mood.behavior,stc$sex)\
class(stc$sex)\
class(stc$mood.behavior)\
levels(as.factor(stc$sex))\
table(as.factor(stc$mood.behavior),as.factor(stc$sex))\
table(as.factor(stc$Mood.Behavior),as.factor(stc$Sex))\
mosaicplot(Table1,beside=T)\
mosaicplot(Table1,color=NULL,shade=FALSE)\
mosaicplot(Table1,color=TRUE)\
mosaicplot(Table1,color=NULL,shade=FALSE)\
mosaicplot(Table1,color=TRUE,shade=FALSE)\
mosaicplot(Table1,color=TRUE,shade=FALSE,xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,color=TRUE,xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(2,4),xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(2,3),xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(5,3),xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(2,3,4,5),xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(3,4),xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(4,3),xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(4,5),xlab="Mood/Behavior",ylab="Sex")\
stc$mood.behavior<-sapply(stc$Mood.Behavior,function(x)\{ifelse(x=="no","NO",ifelse(x=="n","N0",ifelse(x=="N","NO",ifelse(x=="NO","NO",ifelse(x=="y","YES",ifelse(x=="yes","YES",ifelse(x=="Y","YES",ifelse(x=="YES","YES",NA))))))))\})\
levels(as.factor(stc$mood.behavior))\
stc$mood.behavior<-sapply(stc$Mood.Behavior,function(x)\{ifelse(x=="no","NO",ifelse(x=="n","NO",ifelse(x=="N","NO",ifelse(x=="NO","NO",ifelse(x=="y","YES",ifelse(x=="yes","YES",ifelse(x=="Y","YES",ifelse(x=="YES","YES",NA))))))))\})\
levels(as.factor(stc$mood.behavior))\
table(stc$mood.behavior,stc$sex)\
mosaicplot(Table1,col=c(4,5),xlab="Mood/Behavior",ylab="Sex")\
Table1 <-table(stc$mood.behavior,stc$sex)\
mosaicplot(Table1,col=c(4,5),xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(4,5),main="Mosaic Plots of Mood/Behavioral change for males/females", xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(4,5),main="Mosaic Plot of Mood/Behavioral of males/females", xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(4,5),main="Mosaic Plot of Mood/Behavioral for M/F", xlab="Mood/Behavior",ylab="Sex")\
mosaicplot(Table1,col=c(4,5),main="Mosaic Plot of Mood/Behavioral Change For Genders", xlab="Mood/Behavior",ylab="Sex")\
data3[is.na(data3$daysofcare),]\
data3<-data3[!is.na(data3$daysofcare),]\
save(data3,file="Gender and days of care.RData")\
\
\
#or\
\
data4<-na.omit(data4)
\f1 \

\f0 #scatter plot length vs age\
\
plot(length$age,length$lengthofservice,xlab="AGE",ylab="Length of Services",main="Scatter plot of length of services vs age")\
abline(lm(length$lengthofservice[length$sex=="M"]~length$age[length$sex=="M"]),col="blue")\
abline(lm(length$lengthofservice[length$sex=="F"]~length$age[length$sex=="F"]),col="red")\
abline(lm(length$lengthofservice~length$age))\
\
#t test \
t.test(length$lengthofservice[length$sex=="M"],length$lengthofservice[length$sex=="F"],alternative="less",conf.level=0.95)
\f1 \
\
\

\f0 #For female\
plot(length$age[length$sex=="F"],length$lengthofservice[length$sex=="F"],xlab="AGE",ylab="Length of Services",main="Scatter plot of length of services vs age (F)")
\f1 \

\f0 abline(lm(length$lengthofservice[length$sex=="F"]~length$age[length$sex=="F"]))\
\
#Days of care per week\
histogram:\
hist(length$lengthofservice,breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000),main="Histogram of Length of services",xlab="length of services")
\f1 \
\

\f0 #scatter plot \
#male \
plot(days$age[days$sex=="M"],days$daysofcare[days$sex=="M"],xlab="AGE",ylab="Days per week attend",main="Scatter plot of days per week attend vs age (M)")
\f1 \

\f0 #female\
\
#total \
plot(days$age,days$daysofcare,xlab="AGE",ylab="Days per week attend",main="Scatter plot of days per week attend vs age")\
abline(lm(days$daysofcare~days$age))\
abline(lm(days$daysofcare[days$sex=="M"]~days$age[days$sex=="M"]),col="blue")
\f1 \
\
\
\

\f0 #number of caregives\
\
hist(caregivers$numofcaregivers,breaks=c(0,1,2,3,4),main="Histogram of number of care givers",xlab="number of care givers",col="grey")\
\
plot(caregivers$age,caregivers$numofcaregivers,xlab="AGE",ylab="number of care givers",main="Scatter plot of number of caregivers vs age")\
\
\
abline(lm(caregivers$numofcaregivers[caregivers$sex=="M"]~caregivers$age[caregivers$sex=="M"]),col="blue")\
\
abline(lm(caregivers$numofcaregivers[caregivers$sex=="F"]~caregivers$age[caregivers$sex=="F"]),col="red")\
abline(lm(caregivers$numofcaregivers~caregivers$age))
\f1 \
\
\
\
\
\

\f0 #Replace NA by a new number\
\
na.omit(stc$Number.of.care.givers)\
stc$Number.of.care.givers\
test<-stc\
stc1<-stc\
stc2<-stc\
test<-stc$Number.of.care.givers\
is.na(test)\
\
which(is.na(test))\
test[which(is.na(test))]<-0\
care<-data.frame(age,s,test)\
save(care,file="number of caregiver.RData")\
#Save:\
grades<-na.omit(care)
\f1 \

\f0 care<-grades
\f1 \
\
\
\pard\pardeftab720\li720\ri720\partightenfactor0
\cf0 \
\
}