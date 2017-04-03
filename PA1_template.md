Coursera, reproducible research, week 2
---------------------------------------

Step 1: Read csv and convert to convenient format
-------------------------------------------------

``` r
mdata <- read.csv("./data/activity.csv", stringsAsFactors = FALSE, header = TRUE) 
mdata$steps <- as.numeric(mdata$steps)
mdata$date <- as.Date(mdata$date, "%Y-%m-%d")

# group data by days
mdata.group_by_date <- mdata%>%
  group_by(date)%>% 
  summarise(steps = sum(steps, na.rm = TRUE))
```

Step 2: histogram of the total number of steps per day
------------------------------------------------------

``` r
p <- ggplot(mdata.group_by_date, aes(date, steps)) + geom_histogram(stat = "identity", na.rm = TRUE) + 
      geom_hline(aes(yintercept = mean(mdata.group_by_date$steps), colour = "red")) + 
      guides(color = guide_legend(title = "mean"))
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

``` r
plot(p)
```

![](PA1_template_files/figure-markdown_github/step2-1.png)

Step 3: Mean and median number of steps
---------------------------------------

``` r
mmean <- mean(mdata.group_by_date$steps, na.rm = TRUE)
median <- median(mdata.group_by_date$steps, na.rm = TRUE)
mmean
```

    ## [1] 9354.23

``` r
median
```

    ## [1] 10395

Step 4: Time series plot of the average number of steps taken
-------------------------------------------------------------

``` r
mdata.group_by_interval <- mdata%>%
  group_by(interval)%>%
  summarise(steps = mean(steps, na.rm = TRUE))

p <- ggplot(mdata.group_by_interval, 
            aes(interval, steps), 
            type = "l") +  geom_line(aes(y = steps)) 
plot(p)
```

![](PA1_template_files/figure-markdown_github/step4-1.png)

Step 5: The 5-minute interval that, on average, contains the maximum number of steps
------------------------------------------------------------------------------------

``` r
max.int <- which(mdata.group_by_interval$steps == max(mdata.group_by_interval$steps))
mdata.group_by_interval[max.int,]
```

    ## # A tibble: 1 <U+00D7> 2
    ##   interval    steps
    ##      <int>    <dbl>
    ## 1      835 206.1698

We could see that interval num 835 has max steps (~206)

Step 6: Code to describe and show a strategy for imputing missing data
----------------------------------------------------------------------

Missing values are replaced by average values from intervals

``` r
count.na <- sum(is.na(mdata$steps))

# New dataset, without NA - mean by intervals
newdata <- merge(mdata, mdata.group_by_interval, by = "interval")
nas <- which(is.na(newdata$steps.x))
newdata[nas, "steps.x"] <- newdata[nas, "steps.y"]
colnames(newdata)[2] <- "steps"
# Remove unnessesary column
newdata <- newdata[, !names(newdata) == "steps.y"]
newdata <- newdata[with(newdata, order(date, interval)), ]
```

Step 7: Histogram of the total number of steps taken each day after missing values are imputed
----------------------------------------------------------------------------------------------

``` r
newdata.group_by_date <- newdata%>%
  group_by(date)%>% 
  summarise(steps = sum(steps))

p <- ggplot(newdata.group_by_date, aes(date, steps)) + geom_histogram(stat = "identity", na.rm = TRUE) + 
  geom_hline(aes(yintercept = mean(newdata.group_by_date$steps), colour = "red")) + 
  guides(color = guide_legend(title = "mean"))
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

``` r
plot(p)
```

![](PA1_template_files/figure-markdown_github/step7-1.png)

Step 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
-----------------------------------------------------------------------------------------------------------------

``` r
Sys.setlocale("LC_TIME", "English")
```

    ## [1] "English_United States.1252"

``` r
newdata$weekdesc <- as.factor(ifelse(weekdays(newdata[,"date"]) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
table(newdata$weekdesc)
```

    ## 
    ## weekday weekend 
    ##   12960    4608

``` r
newdata.group_by_interval <- newdata%>%
  group_by(interval, weekdesc)%>%
  summarise(steps = mean(steps))

p <- ggplot(newdata.group_by_interval, 
    aes(interval, steps, colour = weekdesc), type = "l") + 
    geom_line(aes(y = steps)) + 
    facet_grid(weekdesc ~ .)
plot(p)
```

![](PA1_template_files/figure-markdown_github/step8-1.png)
