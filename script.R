#check to see if unzip datafile exists and load it
if (file.exists('activity.csv')){
  a1<-read.csv('activity.csv')
} else {
  unzip('./activity.zip',exdir='./')
  a1<-read.csv('activity.csv')
}

#total number of steps taken per day
#apply the sum function after grouping data by date
a2<-tapply(a1$steps,a1$date,sum,na.rm=T)

#make a data frame from the list
a4<-data.frame(names = row.names(a2), a2)
row.names(a4)<-NULL
names(a4)<-c("dates","steps")

#make/display histogram of total number of steps taken per day

#sequence for tickmarks for y axis
s<-seq(0,28,by=2) 

#make the histogram 
hist(a4$steps,xlab="Total number of steps taken per day",
              ylab="Number of days (frequency in days)", col="blue",
              main="Total number of steps taken measured by frequency in days",
              yaxt="n")
axis(2,at=s,labels = as.character(s),cex.axis=0.8)

#mean and median of total number of steps
a41<-round(mean(a4$steps,na.rm=T))
a42<-round(median(a4$steps,na.rm=T))
print(paste("Mean number of steps per day is", a41))
print(paste("Median number of steps per day is", a42))

#calculate mean of steps after grouping by 5 min. intervals 
#over all days
a3<-tapply(a1$steps,a1$interval,mean,na.rm=T)

#make a data frame of the list 
a5<-data.frame(names = row.names(a3), a3)
row.names(a5)<-NULL
names(a5)<-c("interval","steps")
a5$steps<-round(a5$steps)

#make a sequence for tick marks and labels
s<-seq(0,288,by=12) 

#plot the line chart
plot(a5$steps,type="l",lty=1,lwd=2,ylab="Average No. of Steps",
     main="Average number of steps over two months at 5 minute intervals ",
     xaxt='n', xlab="Hours")
axis(1,at=s,labels = as.character(s/12),cex=0.85)

#get the position of the point where the max happens
a12<-which.max(a5$steps)
a13<-a5[a12,]

#place the point
points(x=as.numeric(row.names(a13)),y=a13[1,2])

#place the label for the point
textLabel<-paste("Max. of Avg. number of steps is", as.character(a13[1,2]), 
                 " and happens in interval", row.names(a13))
text(x=as.numeric(row.names(a13)),y=a13[1,2],labels=textLabel,cex=0.85,pos=4)

#imputation of missing data

#index of missing values
a6<-which(is.na(a1$steps))

#subset rows with missing values from main data set without the steps col.
a7<-a1[a6,2:3]

#print the number of missing values using dimension of the data frame
print(paste("Number of missing values is", dim(a7)[1]))

# join the table of missing values with mean values of intervals generated
# earlier and stored in a5 i.e average values of steps grouped by  
# 5 minute invervals over two months
a71<-join(a5,a7,by="interval",type="inner")

#put the data in the right order to re-merge with original data 
#frame
a72<-a71[with(a71,order(date,as.numeric(as.character(interval)))),]

#merge with copy of original data frame
a11<-a1
a11[a6,1]<-a72[,2]

#sum up steps after grouping by date as required
a24<-tapply(a11$steps,a11$date,sum,na.rm=T)

#make a data frame from the list
a43<-data.frame(names = row.names(a24), a24)
row.names(a43)<-NULL
names(a43)<-c("dates","steps")

#sequence for tickmarks for y axis
s<-seq(0,36,by=2) 

#make/display histogram of total number of steps taken per day
hist(a43$steps,xlab="Total number of steps taken per day",
     ylab="Number of days (frequency in days)", col="magenta",
     main=paste("Total number of steps taken measured by frequency in days",
                "with NA values imputed"),
     yaxt="n")
axis(2,at=s,labels = as.character(s),cex.axis=0.8)

#mean and median of total number of steps
a44<-round(mean(a43$steps,na.rm=T))
a45<-round(median(a43$steps,na.rm=T))
print(paste("Mean number of steps (imputed values) per day is", a44))
print(paste("Median number of steps (imputed values) per day is", a45))


#subset data for when day is on a weekday
a8<-which(weekdays(as.Date(a1$date))!="Saturday" 
          & weekdays(as.Date(a1$date))!="Sunday")
a9<-a1[a8,]

#set the device and plot width and height, will be different 
#for different platforms
x11(width=6,height=6)

#place the two plot one above the other i.e 2 rows and 1 column
par(mfrow=c(2,1))

#calculate mean of steps after grouping by 5 min. intervals 
#over all days
a31<-tapply(a9$steps,a9$interval,mean,na.rm=T)

#make a data frame of the list 
a51<-data.frame(names = row.names(a31), a31)
row.names(a51)<-NULL
names(a51)<-c("interval","steps")
a51$steps<-round(a5$steps)

#make a sequence for tick marks and labels
s<-seq(0,288,by=12) 

#plot the line chart
plot(a51$steps,type="l",lty=1,lwd=2,col="blue", cex.main=1, 
     cex.lab=0.8, cex.axis=0.8, ylab="Average No. of Steps",
     main=paste("Average number of steps taken on weekdays",
                  "over two months in 5 minute intervals"),
     xaxt='n', xlab="Hours")
axis(1,at=s,labels = as.character(s/12),cex.axis=0.7) 

#position of point at which max value happens
a15<-which.max(a51$steps)
a16<-a5[a15,]

#place the point
points(x=as.numeric(row.names(a16)),y=a16[1,2],col="blue")

#place the label
textLabel<-paste("Max. of avg. number of steps is", as.character(a16[1,2]), 
                 " and happens in interval", row.names(a16))
text(x=as.numeric(row.names(a16)),y=a16[1,2],col="blue", 
                  labels=textLabel,cex=0.7,pos=4)


#subset data for when day is on a weekend
a10<-which(!(weekdays(as.Date(a1$date))!="Saturday" 
             & weekdays(as.Date(a1$date))!="Sunday"))
a14<-a1[a10,]

#calculate mean of steps after grouping by 5 min. intervals 
#over all days
a32<-tapply(a14$steps,a14$interval,mean,na.rm=T)

#make a data frame of the list 
a52<-data.frame(names = row.names(a32), a32)
row.names(a52)<-NULL
names(a52)<-c("interval","steps")
a52$steps<-round(a52$steps)

#make a sequence for tick marks and labels
s<-seq(0,288,by=12) 

#plot the line chart
plot(a52$steps,type="l",lty=1,lwd=2,col="red", cex.main=1, cex.lab=0.8,
     cex.axis=0.8, 
     ylab="Average no. of steps",
     main=paste("Average number of steps taken on weekends",
                "over two months in 5 minute intervals"),
     xaxt='n', xlab="Hour")
axis(1,at=s,labels = as.character(s/12),cex.axis=0.7)

#position of point at which max value happens
a17<-which.max(a52$steps)
a18<-a52[a17,]
#place the point
points(x=as.numeric(row.names(a18)),y=a18[1,2],pch=3,col="red")
textLabel<-paste("Max. of avg. number of steps is", as.character(a18[1,2]), 
                 " and happens in interval", row.names(a18))
#place the label
text(x=as.numeric(row.names(a18)),y=a18[1,2],col="red",labels=textLabel,
     cex=0.7,pos=4)

