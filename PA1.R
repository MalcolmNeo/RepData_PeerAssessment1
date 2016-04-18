
# Read dataset from csv file
activity<-read.csv('C:/Users/User/Documents/R/RepData_PeerAssessment1/activity.csv')

# Total number of steps taken each day
sum_step_by_day = aggregate(activity$steps, by = list(activity$date), sum,na.rm=TRUE)

names(sum_step_by_day) <- c("Date", "steps")

# Histogram of the total number of steps taken each day
sum_step_by_day
hist(sum_step_by_day$steps)

# Mean and Median number of steps
mean(sum_step_by_day$steps)
median(sum_step_by_day$steps)

# Time series plot of the 5-minute interval
steps_Interval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
steps_Interval
plot(steps~interval,data=steps_Interval,type="l")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
max(steps_Interval$steps)
steps_Interval[which.max(steps_Interval$steps),]$interval

CalStep<-function(interval){
  steps_Interval[steps_Interval$interval==interval,]$steps
}
# Calculate and report the total nmber of missing values in the dataset
sum(is.na(activity$steps))

# Filling the missing values in the dataset
activity_populated<-activity

count=0
for(i in 1:nrow(activity_populated)){
  if(is.na(activity_populated[i,]$steps)){
    activity_populated[i,]$steps<-CalStep(activity_populated[i,]$interval)
    count=count+1
  }
}

# Sum of populated dataset
sum_step_by_day2<-aggregate(steps~date,data=activity_populated,sum)
sum_step_by_day2

# Histogram, Mean and Median of new dataset
hist(sum_step_by_day2$steps)
mean(sum_step_by_day2$steps)
median(sum_step_by_day2$steps)

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity_populated$day=ifelse(as.POSIXlt(as.Date(activity_populated$date))$wday%%6==0,
                          "weekend","weekday")
activity_populated$day=factor(activity_populated$day,levels=c("weekday","weekend"))

steps_Interval2=aggregate(steps~interval+day,activity_populated,mean)

library(lattice)
#xyplot(steps~interval|factor(day),data=steps_Interval2,aspect=1/2,type="l")

xyplot(steps~interval | day, data = steps_Interval2,
       type = 'l',
       xlab = 'Interval',
       ylab = 'Number of Steps',
       layout = c(1,2))
