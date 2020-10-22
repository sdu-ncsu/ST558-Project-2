Daily Bicycle Analysis
================
Todd Idol
10/10/2020

  - [Introduction](#introduction)
  - [Data](#data)
      - [Summary Data](#summary-data)
      - [Modeling Data](#modeling-data)
  - [Summary Analysis](#summary-analysis)
      - [Categorical Data](#categorical-data)
      - [Qunatitative Summaries](#qunatitative-summaries)
  - [Modeling](#modeling)
      - [Tree Based Model
        (non-ensemble)](#tree-based-model-non-ensemble)
      - [Boosted Tree Model](#boosted-tree-model)
      - [Prediction Discussion](#prediction-discussion)

# Introduction

The data for the bicycle rental analysis and prediction consists of
hourly rental information each day for a two year period (2011 & 2012)
totaling \> 17k observations. The goal of the project is to predict
number of rentals for each day based on a set of predictors.

The data is split into 70% training & 30% test sets. The training set is
used for an initial summary analysis to determine optimal predictors and
suitable predictive models. Categorical variables are converted from
numbers to factors for the summary analysis and renamed for more
relevant interpretation. Those were:

  - season (“winter”, “spring”, “summer”, "fall)

  - hour (divided into: “late night”, “AM commute”, “mid day”, “PM
    commute”, “night”)

  - weather (“clear”, “mist”, “light precip”, “heavy pricip”)

  - workday (“no”, “yes”)

  - dteday

Numerical predictors considered in the summary analysis are: temp,
humidity and windspeed

Categorical summaries include a table for rental mean and rental total
for each factor and boxplots for each factor.

Numeric summaries include a 5 number summary, pairwise correlation table
and scatter plots + lm faceted by season, hour & weather for each
numeric variable.

Based on analysis of these variables final training / test sets were
developed on season, hour, weather, temp, and humidity. These sets were
used to conduct Regression Tree and Boosted Regression tree training and
prediction models. More info about predictor considerations and final
model criteria with test results are discussed in the “Modeling”
section.

# Data

``` r
# read in csv data
bikeData <- read.csv("hour.csv")

# remove casual & registered vars since they will not be used for modeling
bikeData <- bikeData  %>% select(everything(), -c("casual", "registered"))

# filter for individual weekday reporting
bikeData <- bikeData  %>% filter(weekday == params$day)

# split data 70% train & 30% test
set.seed(100)
trainIndex <- createDataPartition(bikeData$cnt, p = .7, list = FALSE)

bikeTrain <- bikeData[trainIndex, ]
bikeTest <- bikeData[-trainIndex, ]
```

### Summary Data

``` r
# select vars for summary analysis from the bike data training set
sumBikeTrain <- bikeTrain %>% select(dteday, season, hr, workingday, weathersit, temp, hum, windspeed, cnt)

# create practical factors for season, hour(time of day)y and weather
sumBikeTrain <- mutate(sumBikeTrain, workday = as.factor(ifelse(sumBikeTrain$workingday == 0, "no", "yes")),
                       
                                     season = as.factor(ifelse(sumBikeTrain$season == 1, "winter",
                                                        ifelse(sumBikeTrain$season == 2, "spring",
                                                        ifelse(sumBikeTrain$season == 3, "summer", "fall")
                                                       ))),
                       
                                     hour = as.factor(ifelse((sumBikeTrain$hr >= 6) & (sumBikeTrain$hr < 10), 
                                                            "AM commute", 
                                                      ifelse((sumBikeTrain$hr >= 10) & (sumBikeTrain$hr < 16),
                                                           "mid day", 
                                                      ifelse((sumBikeTrain$hr >= 16 )& (sumBikeTrain$hr < 20), 
                                                           "PM commute", 
                                                      ifelse((sumBikeTrain$hr >= 20) & (sumBikeTrain$hr <= 23),
                                                           "night", "late night")
                                                     )))),
                       
                                     weather = as.factor(ifelse(sumBikeTrain$weathersit == 1, "clear", 
                                                         ifelse(sumBikeTrain$weathersit == 2, "mist",
                                                         ifelse(sumBikeTrain$weathersit == 3, "light precip", 
                                                                                           "heavy precip")
                                                       ))))
                                  
# select final vars for analysis 
sumBikeTrain <- sumBikeTrain %>% select(dteday, season, weather, temp, hum, windspeed, workingday, hour, cnt) %>% 
                                rename("humidity" = hum, "rentals" = cnt, "workday" = workingday)
```

### Modeling Data

``` r
# select vars for regression tree / boosted tree model training
bikeTrain1 <- sumBikeTrain %>% select(c(season, hour, weather, temp, humidity, rentals))

# create test factors to match training set
bikeTest1 <- bikeTest %>% select(c(season, hr, weathersit, temp, hum, cnt)) %>%
                          mutate(season = as.factor(ifelse(bikeTest$season == 1, "winter",
                                                    ifelse(bikeTest$season == 2, "spring",
                                                    ifelse(bikeTest$season == 3, "summer", "fall")
                                                   ))),
                       
                                 hour = as.factor(ifelse((bikeTest$hr >= 6) & (bikeTest$hr < 10), "AM commute", 
                                                  ifelse((bikeTest$hr >= 10) & (bikeTest$hr < 16), "mid day", 
                                                  ifelse((bikeTest$hr >= 16 )& (bikeTest$hr < 20), "PM commute", 
                                                  ifelse((bikeTest$hr >= 20) & (bikeTest$hr <= 23), "night", "late night")
                                                 )))),
                       
                                 weather = as.factor(ifelse(bikeTest$weathersit == 1, "clear", 
                                                     ifelse(bikeTest$weathersit == 2, "mist",
                                                     ifelse(bikeTest$weathersit == 3, "light precip", "heavy precip")
                                                    )))) %>%
                           rename("humidity" = hum, "rentals" = cnt)

# select vars to test models
bikeTest1 <- bikeTest1 %>% select(c(season, hour, weather, temp, humidity, rentals))        
```

# Summary Analysis

## Categorical Data

Mean rentals(count) and total rentals are produced for each factor
(“season”, “weather” & “hour”) to complement the box plots generated
for each factor.

``` r
# function to return rental mean and rental totals for weather, season and hour
sum_meanFx <- function(cat, ...) {
   
   # verify data is valid
   if ((!is.null(cat) & (!is.character(cat)) & ((cat %in% sumBikeTrain[2]) |
                                                (cat %in% sumBikeTrain[3]) |
                                                (cat %in% sumBikeTrain[8]))))
    stop("invalid input")
   
   # return values for season                                                                  
   if (cat == "season") { 
     seasonSum_Mean <- sumBikeTrain %>% group_by(season) %>%
                                       mutate(rental_mean = round(mean(rentals),
                                                                 digits = 2), 
                                              rental_sum = round(sum(rentals),
                                                                digits = 2)) %>% 
                                       select(season, rental_mean, rental_sum)
     seasonSum_Mean <- as_tibble(unique(seasonSum_Mean))
     return(seasonSum_Mean)
   
   # return values for weather  
   } else if (cat == "weather") {
      weatherSum_Mean <- sumBikeTrain %>% group_by(weather) %>%
                                         mutate(rental_mean = round(mean(rentals),
                                                                   digits = 2), 
                                                rental_sum = round(sum(rentals),
                                                                   digits = 2)) %>% 
                                         select(weather, rental_mean, rental_sum) 
      weatherSum_Mean <- as_tibble(unique(weatherSum_Mean))
      return(weatherSum_Mean)
   
   # return values for hour    
   } else if (cat == "hour") {
      hourSum_Mean <- sumBikeTrain %>% group_by(hour) %>% 
                                       mutate(rental_mean = round(mean(rentals),
                                                                 digits = 2), 
                                              rental_sum = round(sum(rentals),
                                                                digits = 2)) %>% 
                                       select(hour, rental_mean, rental_sum)
       hourSum_Mean <- as_tibble(unique(hourSum_Mean))
       return(hourSum_Mean)
       
   } else {
       return(null)
   }
}
```

### Season Summary & Box Plot

``` r
# call sum_mean function and output season summary
kable(sum_meanFx("season"), caption = "Rentals by Season")
```

| season | rental\_mean | rental\_sum |
| :----- | -----------: | ----------: |
| winter |       106.22 |       47904 |
| spring |       227.89 |       97765 |
| summer |       232.43 |      106685 |
| fall   |       200.30 |       84527 |

Rentals by Season

``` r
# box plot for season
seasonBox <- ggplot(sumBikeTrain, aes(x = rentals, y = season, color = season))
seasonBox + geom_boxplot() + labs(title = "Rentals by Season")
```

![](SaturdayAnalysis_files/figure-gfm/season%20statbox-1.png)<!-- -->

### Weather Summary & Box plot

``` r
# call sum_mean function and output summary
kable(sum_meanFx("weather"), caption = "Rentals by Weather Condition")
```

| weather      | rental\_mean | rental\_sum |
| :----------- | -----------: | ----------: |
| clear        |       206.04 |      243751 |
| mist         |       178.75 |       79009 |
| light precip |       104.43 |       14098 |
| heavy precip |        23.00 |          23 |

Rentals by Weather Condition

``` r
# weather boxplot
weatherBox <- ggplot(sumBikeTrain, aes(x = rentals, y = weather, color = weather))
weatherBox + geom_boxplot() + labs(title = "Rentals by Weather Condiion")
```

![](SaturdayAnalysis_files/figure-gfm/weather%20statbox-1.png)<!-- -->

### Hour Summary & Box Plot

``` r
# call sum_mean function & output summary
kable(sum_meanFx("hour"), caption = "Rentals by Hour")
```

| hour       | rental\_mean | rental\_sum |
| :--------- | -----------: | ----------: |
| late night |        42.15 |       18799 |
| AM commute |        88.72 |       25107 |
| mid day    |       351.12 |      154141 |
| PM commute |       317.08 |       95125 |
| night      |       149.18 |       43709 |

Rentals by Hour

``` r
# hour boxplot
hourBox <- ggplot(sumBikeTrain, aes(x = rentals, y = hour, color = hour))
hourBox + geom_boxplot() + labs(title = "Rentals by Hour")
```

![](SaturdayAnalysis_files/figure-gfm/hour%20statbox-1.png)<!-- -->

``` r
# hour summary for days off
dayOff <- sumBikeTrain %>% filter(workday == 0)
dayOffStat <- dayOff %>% group_by(hour) %>% mutate(rental_mean = round(mean(rentals),
                                                                        digits = 2), 
                                                    rental_sum = round(sum(rentals),
                                                                       digits = 2)) %>% 
                                            select(hour, rental_mean, rental_sum)

# create day off summary tibble and output summary
dayOffStat <- as_tibble(unique(dayOffStat))
kable(dayOffStat, caption = "Rentals by Hour (Day Off")
```

| hour       | rental\_mean | rental\_sum |
| :--------- | -----------: | ----------: |
| late night |        42.15 |       18799 |
| AM commute |        88.72 |       25107 |
| mid day    |       351.12 |      154141 |
| PM commute |       317.08 |       95125 |
| night      |       149.18 |       43709 |

Rentals by Hour (Day Off

``` r
# days off boxplot
dayOffBox <- ggplot(dayOff, aes(x = rentals, y = hour, color = hour))
dayOffBox + geom_boxplot() + labs(title = "Rentals by Hour (Day Off)")
```

![](SaturdayAnalysis_files/figure-gfm/hour%20statbox-2.png)<!-- -->

``` r
# hour summary for workdays
dayOn <- sumBikeTrain %>% filter(workday == 1)
dayOnStat <- dayOn %>% group_by(hour) %>% mutate(rental_mean = round(mean(rentals),
                                                                      digits = 2), 
                                                  rental_sum = round(sum(rentals),
                                                                     digits = 2)) %>%
                                          select(hour, rental_mean, rental_sum)

# create work day summary tibble and output summary
dayOnStat <- as_tibble(unique(dayOnStat))
kable(dayOnStat, caption = "Rentals by Time of Day (Work Day)")
```

| hour | rental\_mean | rental\_sum |
| :--- | -----------: | ----------: |

Rentals by Time of Day (Work Day)

``` r
# workday boxplot
dayOnBox <- ggplot(dayOn, aes(x = rentals, y = hour, color = hour))
dayOnBox + geom_boxplot() + labs(title = "Rentals by Time of Day (Work Day)")
```

![](SaturdayAnalysis_files/figure-gfm/hour%20statbox-3.png)<!-- -->

## Qunatitative Summaries

### Numeric Data & Plots

Five number summaries + mean and a correlation matrix are produced for
“temp”, “humidity”, “windspeed” and “rentals” (count). Point plots
with regression lines are produced for each categorical variable
(“season”, “weather”, “hour” and “workday”)

``` r
# summary for numeric dat
quantStats <- sumBikeTrain %>% select(c(4, 5, 6, 9)) %>% apply(2, summary)

# create correlations for numeric vars
rentalCor <- select(sumBikeTrain, c(temp, humidity, windspeed, rentals))
# correlations as data frame
rentalCor <- as.data.frame(round(cor(rentalCor), digits = 2))

# output 5 number summary + mean and pw correlations
kable(round((quantStats), 2), caption = "Summary: Quantitative Varibles")
```

|         | temp | humidity | windspeed | rentals |
| :------ | ---: | -------: | --------: | ------: |
| Min.    | 0.02 |     0.12 |      0.00 |     1.0 |
| 1st Qu. | 0.32 |     0.45 |      0.10 |    42.0 |
| Median  | 0.48 |     0.62 |      0.19 |   129.0 |
| Mean    | 0.48 |     0.62 |      0.20 |   191.3 |
| 3rd Qu. | 0.64 |     0.79 |      0.28 |   300.0 |
| Max.    | 0.96 |     1.00 |      0.66 |   783.0 |

Summary: Quantitative Varibles

``` r
kable(rentalCor, caption = "Rental Correlations")
```

|           |   temp | humidity | windspeed | rentals |
| :-------- | -----: | -------: | --------: | ------: |
| temp      |   1.00 |   \-0.12 |    \-0.01 |    0.51 |
| humidity  | \-0.12 |     1.00 |    \-0.26 |  \-0.42 |
| windspeed | \-0.01 |   \-0.26 |      1.00 |    0.09 |
| rentals   |   0.51 |   \-0.42 |      0.09 |    1.00 |

Rental Correlations

### Rentals by Temp Plots by Category

``` r
# base aesthetic for rentals by temp for season
tempPoint_season <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = season))

# rentals by temp by season point plot
tempPoint_season + geom_point() + geom_smooth(aes(group = season,), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Season Rentals by Temperature") +
            facet_wrap(~ season)
```

![](SaturdayAnalysis_files/figure-gfm/temp%20plots-1.png)<!-- -->

``` r
# base aesthetic for rentals by temp for weather
tempPoint_weather <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = weather))

# rentals by temp by weather point plot
tempPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Weather Rentals by Temperature") +
            facet_wrap(~ weather)
```

![](SaturdayAnalysis_files/figure-gfm/temp%20plots-2.png)<!-- -->

``` r
# base aesthetic for rentals by temp for hour
tempPoint_hour <- ggplot(sumBikeTrain, aes(x = temp, y = rentals, color = hour))

# rentals by temp by hour point plot
tempPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Time of Day Rentals by Temperature") +
            facet_wrap(~ hour)
```

![](SaturdayAnalysis_files/figure-gfm/temp%20plots-3.png)<!-- -->

### Rentals by Humidity Plots by Category

``` r
# ase aesthetic for rentals by humidity for season
humPoint_season <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = season))

# rentals by humidity by season point plot
humPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Season Rentals by Humidity") +
            facet_wrap(~ season)
```

![](SaturdayAnalysis_files/figure-gfm/humidity%20plots-1.png)<!-- -->

``` r
 # base aesthetic for rentals by humidity for weather
humPoint_weather <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = weather))

# rentals by humidity by weather point plot
humPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Weather Rentals by Humidity") +
            facet_wrap(~ weather)
```

![](SaturdayAnalysis_files/figure-gfm/humidity%20plots-2.png)<!-- -->

``` r
# base aeshtetic for rentals by humidity for hour
humPoint_hour <- ggplot(sumBikeTrain, aes(x = humidity, y = rentals, color = hour))

# rentals by humidity by hour point plot
humPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Time of Day Rentals by Humidity") +
            facet_wrap(~ hour)
```

![](SaturdayAnalysis_files/figure-gfm/humidity%20plots-3.png)<!-- -->

### Rentals by Windspeed Plots by Category

``` r
# base aesthetic for rentals by windspeed for season
windPoint_season <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = season))

# rentals by windspeed by season point plot
windPoint_season + geom_point() + geom_smooth(aes(group = season), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Season Rentals by Wind Speed") +
            facet_wrap(~ season)
```

![](SaturdayAnalysis_files/figure-gfm/wind%20plots-1.png)<!-- -->

``` r
# base aesthetic for rentals by windspeed for weather
windPoint_weather <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = weather))

# rentals by windsped by weather point plot
windPoint_weather + geom_point() + geom_smooth(aes(group = weather), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Weather Rentals by Wind Speed") +
            facet_wrap(~ weather)
```

![](SaturdayAnalysis_files/figure-gfm/wind%20plots-2.png)<!-- -->

``` r
# base aesthetic for rentals by windspeed for hour
windPoint_hour <- ggplot(sumBikeTrain, aes(x = windspeed, y = rentals, color = hour))

# rentals by windsped by hour point plot
windPoint_hour + geom_point() + geom_smooth(aes(group = hour), method = lm, col = "black") + 
            scale_fill_continuous() + labs(title =  "Time of Day Rentals by Wind Speed") +
            facet_wrap(~ hour)
```

![](SaturdayAnalysis_files/figure-gfm/wind%20plots-3.png)<!-- -->

# Modeling

Based on the summary analysis the “workday” variable and “windspeed”
variables are removed for regression tree and boosted tree modeling.
There were few observations for days off (only holidays) for weekdays
and saturday and sunday are all days off. So the assessment for each day
absorbs the “workday” variable. Windspeed had small correlation to
“rentals” (cnt) and this was depicted in the point plots with near
zero regression slopes per category. The final variables for R-Tree and
Boosted Tree analysis are: “hour” (seems strong per summary data and box
plots), “temp” (also strong summary data with highest positive
correlation to rentals - born out in regression line by category),
“humidity” (relatively strong correlation to rentals and distinct
lines by category in point plots) “season”, and “weather”.

## Tree Based Model (non-ensemble)

A Regression Tree model is used for the initial prediction. An iterative
approach using “Leave One Out Cross Validation” is employed to determine
the best variables and complexity parameter for lowest RMSE for a given
computational time (unscientifically measured) were used to determine
the final model. All predictors are initially trained with variable
importance plot produced. The top 3 and bottom 3 (non-zero) most/least
important factors were compared to the “all fit”. “Top 3 Fit” predictors
had the the RMSE for the optimal computer generated CP as the “All Fit”.
Adding additional predictors did not lower RMSE. Removing predictors
increased RMSE. “Top 3 Fit” was chosen for tuning based on a vector of
CP’s. The highest CP that did not significantly lower RMSE was chosen as
the best Final Model. Prediction using the final model was run against
the test data set with final results output

### Linear Model added by Shuang Du

``` r
lmfit <- train (rentals ~ ., 
                data=bikeTrain1, 
                method="lm", 
                preProcess = c("center", "scale"), 
                trControl = trainControl(method = "LOOCV"))
lmfit$results
```

### Prediction with Linear Model added by Shuang Du

``` r
lmPred <- predict(lmfit, newdata = bikeTest1)

# create tibble
lmPred_results <- as_tibble(postResample(lmPred, bikeTest1$rentals))

# round results
lmPred_results$value <- round(lmPred_results$value, digits = 2)

# rename rows for output
setattr(lmPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))

# output prediction results
kable(lmPred_results, caption = "Boosted Tree Prediction Results")
```

|          | value |
| :------- | ----: |
| RMSE     | 97.67 |
| Rsquared |  0.70 |
| MAE      | 74.32 |

Boosted Tree Prediction Results

### All 5 Predictor Fit - Program Defined CP

``` r
# train all predictors
rfitAll <- train(rentals ~ ., data = bikeTrain1,
                  method = "rpart", preProcess = 
                  c("center", "scale"), trControl = 
                  trainControl(method = "LOOCV"))

# plot var importance
plot(varImp(rfitAll))
```

![](SaturdayAnalysis_files/figure-gfm/rtree%20all-1.png)<!-- -->

``` r
# output results
round(rfitAll$results[, c(1,2)], digits = 2)
```

### Top 3 Predictor Fit - Program Defined CP

``` r
# train top 3 predictors
rfitTop3 <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                         method = "rpart", preProcess = 
                         c("center", "scale"), trControl = 
                         trainControl(method = "LOOCV"))
 
# output results                        
kable(round(rfitTop3$results[, c(1,2)], digits = 2))
```

|   cp |   RMSE |
| ---: | -----: |
| 0.10 | 138.26 |
| 0.22 | 166.04 |
| 0.26 | 199.36 |

### Top 3 Fit - User Tuned

``` r
# create CP tuning vector
tune <- c(0, .01, .015, .02)
 
# train top 3 predictors - tuned
ufitTop3X<- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                         method = "rpart", preProcess = 
                         c("center", "scale"), trControl = 
                         trainControl(method = "LOOCV"),
                         tuneGrid = expand.grid(cp = tune))

# output results
kable(round(ufitTop3X$results[, c(1,2)], digits = 2))
```

|   cp |   RMSE |
| ---: | -----: |
| 0.00 |  88.28 |
| 0.01 |  97.13 |
| 0.02 |  99.25 |
| 0.02 | 107.34 |

### Final Model

``` r
# train final model with best tuning parameter
ufitFinal <- train(rentals ~ temp + humidity + hour, data = bikeTrain1,
                         method = "rpart", 
                         preProcess = c("center", "scale"),
                         trControl = trainControl(method = "LOOCV"), 
                         tuneGrid = expand.grid(cp = .02))

# ouput results
plot(ufitFinal$finalModel)

# output final model
text(ufitFinal$finalModel, pretty = 1, cex = .8)
```

![](SaturdayAnalysis_files/figure-gfm/rtree%20final%20model-1.png)<!-- -->

### Prediction

``` r
# predict using final model on test data
ufitPred <- predict(ufitFinal, newdata = bikeTest1)

# create tibble of prediction results
ufitPred_results <- as_tibble(postResample(ufitPred, bikeTest1$rentals))

# round resulting values
ufitPred_results$value <- round(ufitPred_results$value, digits = 2)

# create relevant row names & output results
setattr(ufitPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))
kable(ufitPred_results, caption = "Regression Tree Prediction Results")
```

|          |  value |
| :------- | -----: |
| RMSE     | 107.35 |
| Rsquared |   0.64 |
| MAE      |  80.60 |

Regression Tree Prediction Results

## Boosted Tree Model

An ensemble Boosted Tree model is used for the second prediction. All
variables are considered for the training fits using a 50 fold cross
validation control. Folds of 10, 100, 170 were also used and 50 provided
the best intersection of lower RMSE at reasonable computational
cost(time). An initial training fit with system generated controls for
shrinkage, n-trees, and interaction depth was run. A tuning grid was
created to see if these controls could be improved and a 2nd fit was
trained. The 2nd fit did not improve over the computer generated “best
model” so that model was used as the final model for prediction on the
test data set.

### Initial Boosted Fit

``` r
# Train program defined tuning controls
boostedFit <- train(rentals ~ ., data = bikeTrain1,
                   method = "gbm", preProcess =
                   c("center", "scale"), trControl = 
                   trainControl(method = "cv", 
                   number = 50), verbose = FALSE)

# output the results                
kable(round(boostedFit$results, digits = 2))
```

|   | shrinkage | interaction.depth | n.minobsinnode | n.trees |   RMSE | Rsquared |   MAE | RMSESD | RsquaredSD | MAESD |
| :- | --------: | ----------------: | -------------: | ------: | -----: | -------: | ----: | -----: | ---------: | ----: |
| 1 |       0.1 |                 1 |             10 |      50 | 106.24 |     0.70 | 80.73 |  19.41 |       0.06 | 14.22 |
| 4 |       0.1 |                 2 |             10 |      50 |  88.84 |     0.77 | 66.78 |  14.50 |       0.05 | 10.79 |
| 7 |       0.1 |                 3 |             10 |      50 |  83.52 |     0.80 | 62.63 |  11.98 |       0.05 |  8.86 |
| 2 |       0.1 |                 1 |             10 |     100 |  98.51 |     0.72 | 76.55 |  16.66 |       0.06 | 12.85 |
| 5 |       0.1 |                 2 |             10 |     100 |  84.36 |     0.79 | 63.40 |  11.75 |       0.05 |  8.91 |
| 8 |       0.1 |                 3 |             10 |     100 |  80.44 |     0.81 | 60.03 |  10.34 |       0.05 |  7.82 |
| 3 |       0.1 |                 1 |             10 |     150 |  96.37 |     0.73 | 75.61 |  15.04 |       0.05 | 11.98 |
| 6 |       0.1 |                 2 |             10 |     150 |  82.58 |     0.80 | 61.96 |  10.84 |       0.04 |  8.30 |
| 9 |       0.1 |                 3 |             10 |     150 |  79.87 |     0.81 | 59.63 |  10.23 |       0.05 |  7.70 |

### Tuned Boosted Fit

``` r
# grid of tuning parameters
gbm_grid <- expand.grid(n.trees = c(200, 250),
                        interaction.depth = (4),
                        shrinkage = c(.1, .2), 
                        n.minobsinnode = 10)

# train user defined tuning controls using grid
boostedFitX <- train(rentals ~ ., data = bikeTrain1,
                   method = "gbm", preProcess =
                   c("center", "scale"), trControl = 
                   trainControl(method = "cv", 
                   number = 50), verbose = FALSE,
                   tuneGrid = expand.grid(gbm_grid))

# output results
kable(round(boostedFitX$results, digits = 2))
```

|   | shrinkage | interaction.depth | n.minobsinnode | n.trees |  RMSE | Rsquared |   MAE | RMSESD | RsquaredSD | MAESD |
| :- | --------: | ----------------: | -------------: | ------: | ----: | -------: | ----: | -----: | ---------: | ----: |
| 1 |       0.1 |                 4 |             10 |     200 | 78.88 |     0.81 | 58.76 |  10.79 |       0.05 |  7.73 |
| 3 |       0.2 |                 4 |             10 |     200 | 79.78 |     0.81 | 59.09 |  10.59 |       0.05 |  7.17 |
| 2 |       0.1 |                 4 |             10 |     250 | 78.91 |     0.81 | 58.56 |  10.65 |       0.05 |  7.55 |
| 4 |       0.2 |                 4 |             10 |     250 | 80.46 |     0.80 | 59.60 |  11.40 |       0.05 |  7.58 |

### Final Boosted Model

``` r
# grid of final model parameters
gbm_grid1 <- expand.grid(n.trees = 150,
                        interaction.depth = 3,
                        shrinkage = .1, 
                        n.minobsinnode = 10)

# train final model
boostedFinal <- train(rentals ~ ., data = bikeTrain1,
                   method = "gbm", preProcess =
                   c("center", "scale"), trControl = 
                   trainControl(method = "cv", 
                   number = 50), verbose = FALSE,
                   tuneGrid = expand.grid(gbm_grid1))

# output results
boostedFinal$finalModel
```

    ## A gradient boosted model with gaussian loss function.
    ## 150 iterations were performed.
    ## There were 12 predictors of which 11 had non-zero influence.

### Prediction

``` r
# predict using final model on test data
boostedPred <- predict(boostedFinal, newdata = bikeTest1)

# create tibble of prediction results
boostedPred_results <- as_tibble(postResample(boostedPred, bikeTest1$rentals))

# round results
boostedPred_results$value <- round(boostedPred_results$value, digits = 2)

# rename rows for output
setattr(boostedPred_results, "row.names", c("RMSE", "Rsquared", "MAE"))

# output prediction results
kable(boostedPred_results, caption = "Boosted Tree Prediction Results")
```

|          | value |
| :------- | ----: |
| RMSE     | 77.70 |
| Rsquared |  0.81 |
| MAE      | 56.98 |

Boosted Tree Prediction Results

## Prediction Discussion

Both models produced similar RMSE results with the Boosted Tree model
slightly outperforming the Regression Tree model. Advantages of
Regression Tree model - fewer trees/less complex. Disadvantage of
Regression Tree - more computational intensive using LOOC and multiple
model variations had to be run to determine the best fit. Advantages of
Boosted Tree model - less computational expensive and produced a better
result for a relatively “light” fold count. Disadvantage - more coding
to set up tuning grid options and final model is more complex than the
R-Tree.
