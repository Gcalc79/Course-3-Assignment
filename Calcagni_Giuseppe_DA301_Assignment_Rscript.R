## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales1 <- read.csv(file.choose(), header=T)

# Print the data frame.
View(sales1)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales2 <- subset(sales1, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(sales2)

dim(sales2)

# Determine if there are missing values
sum(is.na(sales1))

# Position of missing values by column wise
sapply(sales1, function(x) which(is.na(x)))
# The two missing values are in the release year

# View the descriptive statistics.
summary(sales2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, Genre,
      xlab="Product",
      ylab="Genre", 
      color=Platform, 
      data=sales1)

qplot(Year, Platform,
      xlab="Year",
      ylab="Platform", 
      color=Genre, 
      data=sales1)

qplot(Platform,
      Global_Sales,
      data=sales1,
      geom='col')

qplot(Platform,
      NA_Sales,
      data=sales1,
      geom='col')

qplot(Platform,
      EU_Sales,
      data=sales1,
      geom='col')

## 2b) Histograms
# Create histograms.
# View the genre of games sold by platform
qplot(Platform, 
      xlab="Platform",
      ylab="Count", 
      fill=Genre, 
      data=sales1)

qplot(Genre, 
      xlab="Genre",
      ylab="Count", 
      fill=Platform, 
      data=sales1)

qplot(Year, 
      xlab="Year",
      ylab="Count", 
      fill=Genre,
      binwidth = 0.5,
      data=sales1)

## 2c) Boxplots
# Create boxplots.
qplot(Product, Global_Sales, 
      data=sales1, 
      colour=I('black'), 
      geom='boxplot')

qplot(Product, NA_Sales, 
      data=sales1, 
      colour=I('dark green'), 
      geom='boxplot')

qplot(Product, EU_Sales, 
      data=sales1, 
      colour=I('black'), 
      geom='boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# Some games were produced and sold for platforms that were mainstream in the past.
# Like NES, SNES, GEN, 2600. Similarly, we can see renres that have become 
# lately a mainstream that appeared only at a later stage.
# The 5 platforms that sells most games globally are: Wii, X360, PS3, DA, GB/PS
# The 5 platforms that sells most games in NA are: X360, Wii, PS3, DS, GB
# The 5 platforms that sells most games in EU are: Wii, PS3, X360, DS, PS4
# There is a change in the top 5 by sales variable, but the top 3 are the same.
# They are the most trending platforms, but not the latest, showing that 
# customers still tend to use the latest previous generation ones. 
#
# It would be interesting to see per type of platform what is the sales values 
# over the years, to understand which one had the biggest tenure. 
# It would be interesting to add another column as other_sales, 
# for the rest of the world apart NA and EU.
###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
# View the data frame to sense-check the data set.
View(sales2)

# View the head of the data frame.
head(sales2)

# Check output: Determine the min, max, and mean values.
apply(sales2, 2, max)

apply(sales2, 2, min)

mean(sales2$Global_Sales)
mean(sales2$NA_Sales)
mean(sales2$EU_Sales)

# View the descriptive statistics.
summary(sales2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales3 <- sales2 %>% group_by(Product) %>% 
  summarise(sum_sales_Global=sum(Global_Sales), 
            sum_sales_NA=sum(NA_Sales), 
            sum_sales_EU=sum(EU_Sales),
            .groups='drop')

# View the data frame.
View(sales3)

# Explore the data frame.
dim(sales3)
# The number of observations has gone from 352 to 175, 
# as many games are available in more than one platform.

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
# Set the data source, add mapping elements.
ggplot(data = sales3,
       mapping = aes(x = sum_sales_NA, y = sum_sales_Global)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')

ggplot(data = sales3,
       mapping = aes(x = sum_sales_EU, y = sum_sales_Global)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')

ggplot(data = sales3,
       mapping = aes(x = sum_sales_EU, y = sum_sales_NA)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')

ggplot(data = sales3,
       mapping = aes(x = sum_sales_NA, y = sum_sales_EU, color = sum_sales_Global)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm', se = TRUE, size = 1)


# Create histograms.
qplot(Platform, 
      xlab="Platform",
      ylab="Count", 
      fill=Genre, 
      data=sales1)

qplot(Genre, 
      xlab="Genre",
      ylab="Count", 
      fill=Platform, 
      data=sales1)

qplot(Year, 
      xlab="Year",
      ylab="Count", 
      fill=Genre,
      binwidth = 0.5,
      data=sales1)

# Create boxplots.
qplot(Product, Global_Sales, 
      data=sales1, 
      colour=I('black'), 
      geom='boxplot')

qplot(Product, NA_Sales, 
      data=sales1, 
      colour=I('dark green'), 
      geom='boxplot')

qplot(Product, EU_Sales, 
      data=sales1, 
      colour=I('black'), 
      geom='boxplot')

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

### Global Sales
# Specify qqnorm function (draw a qqplot).
qqnorm(sales3$sum_sales_Global,
       col='blue',
       xlab="Global Sales",
       ylab='#')

# Specify qqline function.
qqline(sales3$sum_sales_Global,
       col='red',
       lwd=2) 


### NA Sales
# Specify qqnorm function (draw a qqplot).
qqnorm(sales3$sum_sales_NA,
       col='dark green',
       xlab="NA Sales",
       ylab='#')

# Specify qqline function.
qqline(sales3$sum_sales_NA,
       col='red',
       lwd=2) 


### EU Sales
# Specify qqnorm function (draw a qqplot).
qqnorm(sales3$sum_sales_EU,
       col='orange',
       xlab="EU Sales",
       ylab='#')

# Specify qqline function.
qqline(sales3$sum_sales_NA,
       col='red',
       lwd=2) 

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)


# Perform Shapiro-Wilk test.
shapiro.test(sales3$sum_sales_Global)
shapiro.test(sales3$sum_sales_NA)
shapiro.test(sales3$sum_sales_EU)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(sales3$sum_sales_Global) 
kurtosis(sales3$sum_sales_Global)

skewness(sales3$sum_sales_NA) 
kurtosis(sales3$sum_sales_NA)

skewness(sales3$sum_sales_EU) 
kurtosis(sales3$sum_sales_EU)

## 3d) Determine correlation
# Determine correlation.
# Subset the dataframe to get only sales numeral variables
salesnum <- subset(sales3, select = -c(Product))

# View the correlation between the sales variables
round(cor(salesnum), digits=2)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

###
# Global vs EU
# Compare all the sales data (columns) for any correlation(s)
plot(sales3$sum_sales_Global, sales3$sum_sales_EU, pch = 19, col = "orange")

# Add the regression line
abline(lm(sales3$sum_sales_EU ~ sales3$sum_sales_Global), col = "red", lwd = 2)

# Add correlation to the plot
text(20, 20, paste("Correlation Global vs EU Sales:", 
                   round(cor(sales3$sum_sales_Global, sales3$sum_sales_EU), 2)))

###
# Global vs NA
# Compare all the sales data (columns) for any correlation(s)
plot(sales3$sum_sales_Global, sales3$sum_sales_NA, pch = 19, col = "dark green")

# Add the regression line
abline(lm(sales3$sum_sales_NA ~ sales3$sum_sales_Global), col = "red", lwd = 2)

# Add correlation to the plot
text(20, 30, paste("Correlation Global vs NA Sales:", 
                   round(cor(sales3$sum_sales_Global, sales3$sum_sales_NA), 2)))

###
# NA vs EU
# Compare all the sales data (columns) for any correlation(s)
plot(sales3$sum_sales_NA, sales3$sum_sales_EU, pch = 19, col = "blue")

# Add the regression line
abline(lm(sales3$sum_sales_EU ~ sales3$sum_sales_NA), col = "red", lwd = 2)

# Add correlation to the plot
text(20, 20, paste("Correlation NA vs EU Sales:", 
                   round(cor(sales3$sum_sales_NA, sales3$sum_sales_EU), 2)))

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The significant skewness and kurtosis clearly indicate that data are not normal,
# and that is the same for Global, EU and NA sales.
# There is a positive correlation between Global vs NA, 
# Global vs EU, NA vs EU sales.
# The correltation is particularly strong for Global vs NA and for  Global vs EU, 
# milder for NA vs EU sales.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
view(sales3)

# Determine a summary of the data frame.
summary(sales3)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
library(forecast)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.

#######
# apply linear regression model between column Sales NA and sales EU
model1 <- lm(sum_sales_NA ~ sum_sales_EU, data = sales3)

# view the lm model
model1

# view a summary of the lm model.
summary(model1)

# Plot the model residuals
plot(model1$residuals)

# See the coefficients of the model.
coefficients(model1)

# Plot the model residuals with best fit line
abline(coefficients(model1))

#######
# apply linear regression model between column Sales Global and sales NA
# apply linear regression model
model2 <- lm(sum_sales_NA ~ sum_sales_Global, data = sales3)

# view the lm model
model2

# view a summary of the lm model.
summary(model2)

# Plot the model residuals
plot(model2$residuals)

# See the coefficients of the model.
coefficients(model2)

# Plot the model residuals with best fit line
abline(coefficients(model2))

#######
# apply linear regression model between column Sales Global and sales EU
# apply linear regression model
model3 <- lm(sum_sales_EU ~ sum_sales_Global, data = sales3)

# view the lm model
model3

# view a summary of the lm model.
summary(model3)

# Plot the model residuals
plot(model3$residuals)

# See the coefficients of the model.
coefficients(model3)

# Plot the model residuals with best fit line
abline(coefficients(model3))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
# 
# Already done for assignment 5.5.d 
# salesnum <- subset(sales3, select = -c(Product))

# Multiple linear regression model.
# Add the variables to a multiple linear regression model on week 5 dataframe
mlr_1 = lm(sum_sales_Global~sum_sales_NA+sum_sales_NA,
           data=salesnum)

# View the model summary
summary(mlr_1)

# Define a new dataframe to host the observed values indicated
obs_df <- data.frame(sum_sales_NA=c(34.02, 3.93, 2.73, 2.26, 22.08), 
                     sum_sales_EU=c(23.80, 1.56, 0.65, 0.97, 0.52))

# Use the mlr_1 model to predict the global sales for the observed values
predict(mlr_1, newdata=obs_df)

# Compare the predicted value vs the observed one:
# Predicted Values:
# 1         2         3         4         5 
# 58.069919  8.882019  6.920388  6.152083 38.551690
# Observed Values:
# 1         2         3         4         5 
# 67.85      N/A      4.32     N/A        23.21

#### NB: the observed values listed in assignment 6.5.a:e cannot be all found
#### on the table salesnum, but can be found on the original dataset. 
#### Therefore, I am going to use a second mlr model (mlr2) on the original 
#### dataset and check if the model applied is better than the 
#### model applied onthe aggregated data (mlr1).

# Add the variables to a multiple linear regression model on the original data
mlr_2 = lm(Global_Sales~NA_Sales + EU_Sales ,
           data=sales1)
# View the model summary
summary(mlr_2)

# Define a new dataframe to host the observed values indicated
obs_df1 <- data.frame(NA_Sales=c(34.02, 3.93, 2.73, 2.26, 22.08), 
                      EU_Sales=c(23.80, 1.56, 0.65, 0.97, 0.52))

# Use the mlr_1 model to predict the global sales for the observed values
predict(mlr_2, newdata=obs_df1)

# Compare the predicted value vs the observed one:
# Predicted Values:
# 1         2         3         4         5 
# 71.468572  6.856083  4.248367  4.134744 26.431567
# Observed Values:
# 1         2         3         4         5 
# 67.85      6.04     4.32     3.53      23.21

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Use the mlr_1 model to predict the global sales for the observed values
predict(mlr_2, newdata=obs_df1)

# Compare the predicted value vs the observed one:
# Predicted Values:
# 1         2         3         4         5 
# 71.468572  6.856083  4.248367  4.134744 26.431567
# Observed Values:
# 1         2         3         4         5 
# 67.85      6.04     4.32     3.53      23.21

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# In case of the simple linear regression models, we have that apart from 
# few outliers the residuals plot shows points that are randomly scattered 
# around a residual value of 0 with no clear pattern, this indicates that 
# the relationship between x and y is linear and a linear regression model 
# can be used.
# Between the models, model1 has the lowest R-squared=0.38 while model 2 has
# an R-squared=0.83 and model 3 0.72. Therefore, model2 based on the relation 
# between sum_sales_NA and sum_sales_Global is the best between the linears. 
# In the case of the multiple linear regression model, we have that the 
# observed values listed in assignment 6.5.a to 6.5.e cannot be all found
# on the table salesnum, but can be found on the original dataset. 
# Therefore, I am going to build a second mlr model (mlr_2) on the original 
# dataset and check if the model applied is better than the 
# model applied onthe aggregated data (mlr_1).
# Which model is the strongest? The mlr_2 model built on the original dataset
# has an R-squared=0.9687 and an Adj R-squared=0.965, making it the strongest.  

###############################################################################
###############################################################################