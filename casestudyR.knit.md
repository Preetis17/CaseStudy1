

```r
setwd("C:/Users/preeti.swaminathan/Desktop/TO DO/Data Science/work/week 7")

#Clean up FEDSTATS_Country
## Data is organized and clean, directly import file
FEDSTATS_Country <-
  read.csv("C:/Projects/RGroupProject/Data Directory/getdata%2Fdata%2FEDSTATS_Country.csv")


### Clean up GDP
## Pick up only top 190 columns (with country code), Remove 3rd column with no data, Name columns
GDP <-
  read.csv(
    "C:/Projects/RGroupProject/Data Directory/getdata%2Fdata%2FGDP.csv",
    skip = 4,
    nrow = 190
  )
GDP <- GDP[, c(1, 2, 4, 5)]
colnames(GDP) <- c("CountryCode", "Rank", "CountryName", "GDP")


##1 Merge the data based on the country shortcode. How many of the IDs match?
```

```r
Country_stats <-
  merge(GDP, FEDSTATS_Country, by.x = "CountryCode", by.y = "CountryCode")

## Number of matches
print(dim(Country_stats)[1])
```

```
## [1] 189
```

```r
## 2. Sort the data frame in ascending order by GDP (so United States is last). What is the 13th
##country in the resulting data frame?
```

```r
#Convert GDP to numeric
Country_stats$GDP_n <- as.numeric(gsub(",", "", Country_stats$GDP))


#two ways to do
#create a rank based on numeric value of GDP.
rank_GDP <- Country_stats[order(Country_stats$GDP_n), ]

#Option 1
#Display 13 position country in an ordered GDP for first 4 columns
rank_GDP[13, 1:4]
```

```
##    CountryCode Rank         CountryName   GDP
## 93         KNA  178 St. Kitts and Nevis  767
```

```r
#option 2 since we need rank for next question
rank <-
  order(Country_stats$GDP_n) #This rank/position is needed later
arrange(Country_stats, desc(rank))[13, 3]
```

```
## [1] Mexico
## 190 Levels: Afghanistan Albania Algeria Angola ... Zimbabwe
```

```r
### 3. What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
```

```r
## Subset "High income: OECD" and calculate the mean GDP Rank
mean(subset(
  Country_stats,
  Income.Group %in% "High income: OECD",
  select = c(Rank)
)$Rank)
```

```
## [1] 32.96667
```

```r
## Subset "High income: nonOECD"
mean(subset(
  Country_stats,
  Income.Group %in% "High income: nonOECD",
  select = c(Rank)
)$Rank)
```

```
## [1] 91.91304
```

```r
### 4 Show the distribution of GDP value for all the countries and color plots by income group. Use
### ggplot2 to create your plot.

ggplot(data = Country_stats, aes(y = GDP_n, x = CountryName, color = Income.Group)) + geom_density(alpha =
                                                                                                     0.1) + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess'
```

<img src="casestudyR_files/figure-html/unnamed-chunk-4-1.png" width="672" />

```r
scale_x_log10()
```

```
## <ScaleContinuousPosition>
##  Range:  
##  Limits:    0 --    1
```

```r
### 5 Provide summary statistics of GDP by income groups.
summary(Country_stats$GDP_n ~ Country_stats$Income.Group)
```

```
## Country_stats$GDP_n     N= 189 
## 
## +--------------------------+--------------------+---+-------------------+
## |                          |                    |N  |Country_stats$GDP_n|
## +--------------------------+--------------------+---+-------------------+
## |Country_stats$Income.Group|                    |  0|                   |
## |                          |High income: nonOECD| 23| 104349.83         |
## |                          |High income: OECD   | 30|1483917.13         |
## |                          |Low income          | 37|  14410.78         |
## |                          |Lower middle income | 54| 256663.48         |
## |                          |Upper middle income | 45| 231847.84         |
## +--------------------------+--------------------+---+-------------------+
## |Overall                   |                    |189| 379596.51         |
## +--------------------------+--------------------+---+-------------------+
```

```r
### Additional answer, distribution of GDP by income group
ggplot(Country_stats, aes(x = GDP_n, fill = Income.Group)) +
  geom_density(alpha = 0.3) +
  scale_x_log10()
```

<img src="casestudyR_files/figure-html/unnamed-chunk-4-2.png" width="672" />

```r
### 6 Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries are Lower middle income but among the 38 nations with highest GDP?

library(Hmisc)
## Cut Ranks into 5 groups and store as factor variable
Country_stats$Rank.Groups = cut2(Country_stats$Rank, g = 5)

## Build a table of Income Groups across Rank Groups
table(Country_stats$Income.Group, Country_stats$Rank.Groups)
```

```
##                       
##                        [  1, 39) [ 39, 77) [ 77,115) [115,154) [154,190]
##                                0         0         0         0         0
##   High income: nonOECD         4         5         8         5         1
##   High income: OECD           18        10         1         1         0
##   Low income                   0         1         9        16        11
##   Lower middle income          5        13        12         8        16
##   Upper middle income         11         9         8         8         9
```

```r
result <-
  table(Country_stats$Income.Group, Country_stats$Rank.Groups)

### Lower income group with high rank (1-38 rank)
result["Lower middle income", 1]
```

```
## [1] 5
```


---
title: "casestudyR.R"
author: "preeti.swaminathan"
date: "Tue Mar 21 21:10:28 2017"
---
