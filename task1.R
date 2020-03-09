#Task1
#Identification Number:334

#I used the data you uploaded (until February 17th)

#Pretreatment
library(lubridate)
corona<-read.csv("https://raw.githubusercontent.com/vzemlys/cda_course/master/data/time_series_19-covid-Confirmed.csv",header=TRUE)

a<-length(names(corona))

cnm<-colnames(corona)[5:a]
cnm<-sapply(strsplit(gsub("X", "" ,colnames(corona)[5:a]), ".", fixed = TRUE), function(x)
  paste(c(x[1:2],paste0("20",x[3])), collapse = "."))
tm <- mdy(cnm)
tm1<-as.character.Date(tm)
colnames(corona)[5:a]<-tm1

#1.

#I set random size as 5.
#I used for loop and function rbind() to get data frame.

china_region<-corona[corona$Country.Region=="Mainland China",]
i<-sample(1:length(china_region$Province.State),5)
step1Output<-data.frame()
for (j in i){
  step1Output<-rbind(step1Output,china_region[j,])
}
step1Output


#2.

#I used function reshape() to convert the data to long format.
#I changed date to chr at pretreatment, because if I don't change it, the date come out weird(different format like 18223).
#So, I changed chr to date again here.

long_corona<-reshape(corona,direction="long",varying=names(corona[5:a]),v.names="case", timevar="date",idvar="id",times=names(corona[5:a]))
long_corona[,5]<-as.Date.character(long_corona[,5])
task2Output<-long_corona[1:6]

#task2Output
#str(task2Output)
head(task2Output)


#3.

#I picked the 1st, 5th and 7th based on Country.Region.
#Australia has four subgroups so I combined them by date.
#Red - Australia / Green - France / Blue - Egypt

d1<-task2Output[task2Output$Country.Region==levels(task2Output$Country.Region)[1],]
#d1
d1<-d1[order(d1$Province.State),]

m<-0
sum1=0
b<-d1[1,5]

for (i in 0:(a-5)){
  sum1<-0
  for (j in 1:nrow(d1)){
    if (d1[j,5]==b+i) sum1<-sum1+d1[j,6]
  }
  m<-append(m,sum1)
}


for (i in 1:(a-4)){
  d1[i,1]<-""
  d1[i,6]<-m[i+1]
}

d1<-d1[1:(a-4),]
#d1
d2<-task2Output[task2Output$Country.Region==levels(task2Output$Country.Region)[5],]
#d2
d3<-task2Output[task2Output$Country.Region==levels(task2Output$Country.Region)[7],]
#d3

plot(d1$date,d1$case,type="l",col="red",xlab="date",ylab="case",main="Step3 - Case by Date")
points(d2$date,d2$case,type="l",col="blue")
points(d3$date,d3$case,type="l",col="green")

#4.

#I think growths are difference between the day and the day before.
#I used function rbind() to combine three different data frames.

m2<-0
for (i in 0:(a-5)){
  gr<-d1[1+i,6]-d1[i,6]
  m2<-append(m2,gr)
}

for (i in 1:(a-4)){
  d1[i,7]<-m2[i]
}


m3<-0
for (i in 0:(a-5)){
  gr2<-d2[1+i,6]-d2[i,6]
  m3<-append(m3,gr2)
}

for (i in 1:(a-4)){
  d2[i,7]<-m3[i]
}


m4<-0
for (i in 0:(a-5)){
  gr3<-d3[1+i,6]-d3[i,6]
  m4<-append(m4,gr3)
}

for (i in 1:(a-4)){
  d3[i,7]<-m4[i]
}


step4Output<-rbind(d1,d2,d3)
colnames(step4Output)[7]<-"growth"
head(step4Output)
plot(step4Output$date,step4Output$growth,col=as.factor(step4Output$Country.Region),xlab="date",ylab="growth",main="Step4 - Growth by Date")

