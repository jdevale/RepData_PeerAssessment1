library(ggplot2)
library(grid)
library(gridExtra)
#Read data in
filename <- "activity.zip"
datafilename <- "activity.csv"

activityData <- read.csv(unz(filename,datafilename),header=T)
activityData$steps <- as.numeric(activityData$steps)
activityData$interval <- as.numeric(activityData$interval)
activityData$date <- as.Date(activityData$date)

#get clean set of data
completeCaseActivity <- activityData[complete.cases(activityData),]

#### Histogram with NAs removed
#get sum of daily steps for histogram
dailyStepSum <- aggregate(completeCaseActivity$steps,list(date = completeCaseActivity$date),sum)

dailySumGraph=ggplot(data=dailyStepSum, aes(dailyStepSum$x)) + 
  geom_histogram( 
                 col="black", 
                 fill="green", 
                 alpha = .4) + 
  labs(title="Histogram for sum of steps per day (NAs omitted") +
  labs(x="Number of Steps", y="Count") +
  ylim(c(0,10))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))+
  geom_vline(aes(xintercept=mean(dailyStepSum$x)),  
             color="red", linetype="dashed", size=1) +
  geom_text(aes(mean(dailyStepSum$x)+500,9,label = "mean"),angle=270)

print(dailySumGraph)
print(mean(dailyStepSum$x))
print(median(dailyStepSum$x))
#### End histo with NAs removed

fiveMinMean <- aggregate (completeCaseActivity$steps,list(interval=completeCaseActivity$interval),mean)
names(fiveMinMean) <- c("interval","steps")

fiveMinMeanGraph = ggplot(fiveMinMean, aes(x = interval, y = steps)) + 
  geom_line(color = "black") + 
  labs(title = "Mean number of steps by Interval", x = "Interval", y = "Number of steps")
print(fiveMinMeanGraph)

#find interval with the max number of steps
maxSteps <- which.max(fiveMinMean$steps)
print(fiveMinMean[maxSteps, ])

## Get index of NAs
missingList <- which(is.na(activityData$steps))
print(length(missingList))

completedData <- activityData

## Replace missing values 
for(i in missingList) {
  completedData$steps[i] <- round(fiveMinMean[which(fiveMinMean$interval == completedData[i, "interval"]), "steps"])
}

#### Histogram with NAs FILLED
#get sum of daily steps for histogram
dailyStepSum <- aggregate(completedData$steps,list(date = completedData$date),sum)

dailySumFilledGraph=ggplot(data=dailyStepSum, aes(dailyStepSum$x)) + 
  geom_histogram( 
    col="black", 
    fill="green", 
    alpha = .4) + 
  labs(title="Histogram for sum of steps per day (NAs filled)") +
  labs(x="Number of Steps", y="Count") +
  ylim(c(0,10))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))+
  geom_vline(aes(xintercept=mean(dailyStepSum$x)),  
             color="red", linetype="dashed", size=1) +
  geom_text(aes(mean(dailyStepSum$x)+500,9,label = "mean"),angle=270)

print(dailySumFilledGraph)
print(mean(dailyStepSum$x))
print(median(dailyStepSum$x))

#### End histo with NAs FILLED

completedData[as.POSIXlt(completedData$date)$wday %in% c(0,6),"dayType"]="weekend"
completedData[as.POSIXlt(completedData$date)$wday %in% c(1,2,3,4,5),"dayType"]="weekday"
completedData$dayType <- as.factor(completedData$dayType)


completedDataByInterval <- aggregate(completedData$steps,
                                      by = list(completedData$interval, completedData$dayType),
                                      FUN = mean)

## Name columns
names(completedDataByInterval) <- c("interval","dayType","steps")

g<-ggplot(completedDataByInterval,aes(interval,steps))
p<-g+geom_line()+geom_point()+facet_grid(dayType~.)
p<-p+labs(title="Average steps in interval for weekend vs weekday")
p<-p+scale_x_continuous(breaks=c(400,800,1200,1600,2000,2400))
print(p)
