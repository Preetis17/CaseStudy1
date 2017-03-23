### ###########################################################################################################
###                                           World GDP Analysis: Case Study 1
### ###########################################################################################################


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


###### Libraries required
library(ggplot2)
library(Hmisc)
library(rmarkdown)
library(dplyr)

##1 Merge the data based on the country shortcode. How many of the IDs match?
## --------------------------------------------------------------------------------------
Country_stats <-
  merge(GDP, FEDSTATS_Country, by.x = "CountryCode", by.y = "CountryCode")

## Number of matches. Prints dimension of data fram country stats. number of rows and columns.
### [1] gives only rown count.
print(dim(Country_stats)[1])


## 2. Sort the data frame in ascending order by GDP (so United States is last). What is the 13th
##country in the resulting data frame?
## --------------------------------------------------------------------------------------
#Convert GDP to numeric, GDP column has comma which needs to be removed and the number needs to be stored as numeric
Country_stats$GDP_n <- as.numeric(gsub(",", "", Country_stats$GDP))


#two ways to do
#create a rank based on numeric value of GDP.
rank_GDP <- Country_stats[order(Country_stats$GDP_n), ]

#Option 1
#Display 13 position country in an ordered GDP for first 4 columns. 13 is row number 1:4 is columns 1 to 14
rank_GDP[13, 1:4]

#option 2 since we need rank for next question
# Order option creates a rank or position 
rank <-
  order(Country_stats$GDP_n, decreasing = FALSE ) #This rank/position is needed later
arrange(Country_stats, desc(rank))[13, 3]


### 3. What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
## --------------------------------------------------------------------------------------
## Subset "High income: OECD" and calculate the mean GDP Rank
mean(subset(
  Country_stats,
  Income.Group %in% "High income: OECD",
  select = c(Rank)
)$Rank)

## Subset "High income: nonOECD"
mean(subset(
  Country_stats,
  Income.Group %in% "High income: nonOECD",
  select = c(Rank)
)$Rank)

### 4 Show the distribution of GDP value for all the countries and color plots by income group. Use
### ggplot2 to create your plot.


gg <- ggplot(data = Country_stats, aes(y = GDP_n, x = CountryName, color = Income.Group )) + geom_density(alpha = 0.1) 
gg

### 5 Provide summary statistics of GDP by income groups.
SummaryGDP <- summary(Country_stats$GDP_n ~ Country_stats$Income.Group)
print(SummaryGDP)

### Additional answer, distribution of GDP by income group
ggplot(Country_stats, aes(x = GDP_n, fill = Income.Group)) +
  geom_density(alpha = 0.3) +
  scale_x_log10()

### 6 Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries are Lower middle income but among the 38 nations with highest GDP?


## Cut Ranks into 5 groups and store as factor variable
Country_stats$Rank.Groups = cut2(Country_stats$Rank, g = 5)

## Build a table of Income Groups across Rank Groups
table(Country_stats$Income.Group, Country_stats$Rank.Groups)

result <-
  table(Country_stats$Income.Group, Country_stats$Rank.Groups)

### Lower income group with high rank (1-38 rank)
result["Lower middle income", 1]
