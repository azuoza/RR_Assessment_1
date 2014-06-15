library(psych)

dt <- read.csv("./data/activity.csv")

# susumuoti kiekvienos dienos zingsnius
dt.aggr <- aggregate(.~date, data=dt, sum)

hist(
    x=dt.aggr$steps, 
    main="Histogram of steps per day",
    xlab="Steps per day"
    )

s.mean <- mean(dt.aggr$steps)
s.median <- median(dt.aggr$steps)

dt.aggr.2 <- aggregate(.~interval, data=dt, mean)
time.int <- c("0:00", "3:00", "6:00", "9:00", "12:00",
              "15:00", "18:00", "21:00", "24:00")
plot(
    x=dt.aggr.2$interval, 
    y=dt.aggr.2$steps, 
    type="l", 
    axes=F,
    xlab="time interval",
    ylab="number of steps",
    main="mean")
axis(1, at=c(0,300,600,900,1200,1500,1800,2100,2400), lab=time.int)
axis(2, at=c(0,50,100,150,200))

day.peak <- as.character(dt.aggr.2$interval[dt.aggr.2$steps==max(dt.aggr.2$steps)])

day.peak <- sub( '(?<=.{1})', ':', day.peak, perl=TRUE )

sum.na <- sum(is.na(dt$steps))

dt.no.na <- dt 
for(i in 1:dim(dt.no.na)[1]){
    if(is.na(dt.no.na$steps[i])){
        dt.no.na$steps[i] <- dt.aggr.2$steps[dt.aggr.2$interval==dt.no.na$interval[i]]
    }
}

dt2.aggr <- aggregate(.~date, data=dt.no.na, sum)

par(mfrow=c(1,2))
    hist(
        x=dt2.aggr$steps, 
        main="Histogram of steps per day removed NA",
        xlab="Steps per day"
    )

    hist(
        x=dt.aggr$steps, 
        main="Histogram of steps per day",
        xlab="Steps per day"
    )

par(mfrow=c(1,1))

step.mean.NA <- as.integer(mean(dt2.aggr$steps))
step.median.NA <- median(dt2.aggr$steps)
describe(dt2.aggr$steps)

# weekdays
dt.no.na$days <- NA
for(i in 1:dim(dt.no.na)[1]){
    if(weekdays(as.Date(dt.no.na$date[i])) == "Sunday" | 
       weekdays(as.Date(dt.no.na$date[i])) == "Saturday"){
        dt.no.na$days[i] <- "Weekend"
    } else {
        dt.no.na$days[i] <- "Weekday"
    }
}
dt.no.na$days <- as.factor(dt.no.na$days)

table(dt.no.na$days)

dt3.aggr <- aggregate(.~interval, data=dt.no.na, mean)

# suskaiciuoti aggregate darbo dienom ir savaitgaliams
# sujungti (melt) duomenis su id = interrvals
# pagaryti grafika su facets ir ggplot

library(reshape2)
library(ggplot2)
aggr.days <- aggregate(.~interval, 
                       data=subset(x=dt.no.na, subset=dt.no.na$days == "Weekday"), 
                       mean)
aggr.days$days <- "Weekdays"

aggr.ends <- aggregate(.~interval, 
                       data=subset(x=dt.no.na, subset=dt.no.na$days == "Weekend"), 
                       mean)
aggr.ends$days <- "Weekends"

# aggr.melt <- merge(aggr.days, aggr.ends, by="interval")
# aggr.melt <- aggr.melt[, c(-3, -6)]

aggr.melt <- data.frame(interval = rep(NA, times=576), steps=NA, date=NA, days=NA)
for(i in 1:dim(aggr.days)[1]){
    aggr.melt[2*i-1, ] <- aggr.days[i, ]
    aggr.melt[2*i, ] <- aggr.ends[i, ]
}

aggr.melt$days <- as.factor(aggr.melt$days)

fig1 <- ggplot(data=aggr.melt, aes(x=interval, y=steps, facets=days))
fig1 +geom_line()

plot(
    x=aggr.days$interval, 
    y=aggr.days$steps, 
    type="l", 
    axes=F,
    xlab="time interval",
    ylab="number of steps"
)
axis(1, at=c(0,300,600,900,1200,1500,1800,2100,2400), lab=time.int)
axis(2, at=c(0,50,100,150,200))
lines(x=aggr.ends$interval, y=aggr.ends$steps, col="red")
box()
legend("topright", 
       legend=c("Weekday", "Weekend"), 
       col=1:2, 
       lty=1)

fig2 <- ggplot(data=aggr.melt, aes(x=interval, y=steps)) + geom_line()
fig2 + facet_grid(days ~ .)
