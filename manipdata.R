#calculating the mean per day
sumperday <- with(raw_data,tapply(steps,date, sum,na.rm=TRUE))
#histogram per day
sumperday <- with(raw_data,tapply(steps,date, sum,na.rm=TRUE))
hist(sumperday, col="gold", main = "sum per day", xlim = c(0,25000))
# calcul of the mean
mean(sumperday)
#calcul of the median
median(sumperday)
## What is the average daily activity pattern?
#removing missing values
omitted_NA_data <- na.omit(raw_data)
#using the by() function to split the data
int_mean <- by(simplify = FALSE,omitted_NA_data,INDICES = omitted_NA_data$interval,function(x){mean(x$steps)})
#reforming the data to a data frame
res <- do.call("rbind",int_mean)
nw_res <- data.frame(mean=res,interval=as.numeric(row.names(res)))
par(mar=c(2,2,2,2))
plot(nw_res$interval,nw_res$mean,type = "l",xlab = "Interval",ylab = "mean of steps",main = "average daily activity pattern")
#return interval with the max of mean
subset(nw_res,mean==max(mean),select = "interval")
#Calculate and report the total number of missing values in the dataset.  
sum(is.na(raw_data$steps))
#Imputing the NA
# first we determise the intervals where steps is NA
na_interval <- raw_data[is.na(raw_data$steps),"interval"]
#we give index to na_interval of the match in the second vector, which are intervals in the nw_res data frame
index <- match(na_interval,nw_res$interval)
searched_mean <- nw_res[index,"mean"]
#fill the NAs values in the original data
raw_data[is.na(raw_data$steps),"steps"] <- searched_mean
#Creating a new data frame
new_df <- raw_data
head(new_df)
#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total 
#number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
splt_new_df <- by(new_df,new_df$date,function(x){sum(x$step)},simplify = FALSE)
nw_sumperday <- do.call("rbind",splt_new_df)
#plotting the histogram
hist(nw_sumperday,col="gold",main="total steps per day after imputing the NAs",xlab = "days")
#Calculate and report the mean and median total number of steps taken per day.
mean(tapply(new_df$steps, new_df$date, sum))
median(tapply(new_df$steps, new_df$date, sum))
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

ind <- match(raw_data$date,c("Saturday","Sunday"))
my_dy <- sapply(raw_data$date, function(x){if (weekdays(x)=="Saturday"|weekdays(x)=="Sunday"){
    day_vect <- "weekend"
}
    else {
        day_vect <- "weekday"
    }
    day_vect
})
raw_data$day_type <- factor(my_dy)
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
#let's split the data first based on the interval and day_type
spl_dt <- aggregate(steps~interval+day_type,data=raw_data,FUN = mean, na.rm=TRUE)
#create the subsets
weekdaydt <- subset(spl_dt,day_type=="weekday" ,select = c("interval","steps"))
weekendt <- subset(spl_dt,day_type=="weekend",c(1,3))
par(mfrow=c(2,1),mar=c(3,3,3,4))
plot(weekdaydt$interval,weekdaydt$steps,type = 'l',col="darkblue",xlab = "Interval",ylab = "mean of the steps",main = "Weekdays")
plot(weekendt$interval,weekendt$steps,type = 'l',col="red",xlab = "Interval",ylab = "mean of steps",main = "Weekend")