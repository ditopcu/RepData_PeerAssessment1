library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)


setwd("C:\\Users\\ditop\\mygit\\RepData_PeerAssessment1")
unzip(zipfile="activity.zip")


rawdata <- tbl_df( read.csv("activity.csv", stringsAsFactors = FALSE) )

dt <- rawdata %>% mutate(date = ymd(date))


steps.per.day <- dt %>%
                 filter(!is.na(steps)) %>%
                 group_by(date) %>%
                 summarise(total.steps = sum(steps))



ggplot (steps.per.day, aes(x=total.steps)) + 
  geom_histogram( binwidth = 1000, fill = "blue") +
  labs(x= "Total Steps")





average.interval <- dt %>%
                    filter(!is.na(steps)) %>%
                    group_by(interval) %>%
                    summarise(average.steps = mean(steps))


ggplot(average.interval, aes(x = interval, y = average.steps)) +
  geom_line() +
  labs(x="Interval", y="Average Steps")


arrange(average.interval, desc(average.steps))[1,]$interval
#average.interval[average.interval$average.steps==max(average.interval$average.steps),]


missing.values<- sum(is.na(activity$steps))

missing <- is.na(dt$steps)
# How many missing
table(missing)


dt.replaceNA <-     dt %>%
                    group_by(interval)  %>%
                    mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))

steps.per.day.wo.nas <- dt.replaceNA %>%
                        filter(!is.na(steps)) %>%
                        group_by(date) %>%
                        summarise(total.steps = sum(steps))




ggplot (steps.per.day.wo.nas, aes(x=total.steps)) + 
  geom_histogram( binwidth = 1000)






mean.wo.nas <- mean(steps.per.day.wo.nas$total.steps)
median.wo.nas <- median(steps.per.day.wo.nas$total.steps)

paste(mean.org, " " , mean.wo.nas)
paste(median.org, " " , median.wo.nas)



dt.replaceNA <- dt.replaceNA %>%
                mutate( day = wday(date, label =TRUE), day.type = day )

levels(dt.replaceNA$day.type) <- list(  weekday = c("Mon", "Tues", "Wed", "Thurs", "Fri"),   weekend = c("Sun", "Sat") )


average.interval.wo.nas <- dt.replaceNA %>%
                            group_by(interval,day.type) %>%
                            summarise(average.steps = mean(steps)) %>%
                            ungroup()


ggplot(average.interval.wo.nas, aes(x = interval, y = average.steps)) +
  geom_line() +
  labs(x="Interval", y="Average Steps")+
  facet_grid(day.type~.)



