library(dplyr)
library(lubridate)
library(ggplot2)

df<-read.csv("activity.csv")

head(df)

df$date<-ymd(df$date)

df<- df %>% mutate(weekday=weekdays(df$date))




sum.day<-df %>% group_by(date) %>%summarise(steps=sum(steps,na.rm = TRUE))

hist(sum.day$steps, breaks = 10,xlim = c(0,25000), ylim=c(0,20), main="Sum of steps by day", xlab = "Steps by day", col="purple")




mean.day<-mean(sum.day$steps)

median.day<-median(sum.day$steps)




mean.interval<-df %>% group_by(interval) %>%summarise(steps=mean(steps,na.rm = TRUE))

with(mean.interval,plot(interval,steps,main="Average steps by interval", type="l", col="purple" , lwd=2))



max.interval<-mean.interval[which.max(mean.interval$steps),]$interval


na.df<-sum(is.na(df$steps))


df2<-df
df2$steps[is.na(df2$steps)]<-mean.interval$steps[match(df2$interval[is.na(df2$steps)],mean.interval$interval)]
sum.day2<-df2 %>% group_by(date) %>%summarise(steps=sum(steps,na.rm = TRUE))
hist(sum.day2$steps, breaks = 10,xlim = c(0,25000), ylim=c(0,20), main="Sum of steps by day, NA corrected", xlab = "Steps by day", col="purple")
mean.day2<-mean(sum.day2$steps)
median.day2<-median(sum.day2$steps)







df<-df %>% mutate(type.wd=ifelse(weekday=="Saturday"| weekday=="Sunday","Weekend","Weekday"))

mean.interval2<-df %>% group_by(type.wd,interval) %>%summarise(steps=mean(steps,na.rm = TRUE))

ggplot(mean.interval2,aes(interval,steps,col=type.wd))+ geom_line()+facet_wrap(~type.wd,nrow = 2,ncol = 1)+theme(legend.position = "none")

