################################################################################
################################################################################
# Assignment Activity 4: Visualise data to gather insights
################################################################################
################################################################################

##########################
# Setup the environment, load the data
##########################

# Import tidyverse library.
library(tidyverse)

# Import the data set (turtle_sales.csv).
sales1 <- read.csv(file.choose(), header=T)

##########################
# View the data, check for missing values
##########################

# View the data frame.
View(sales1)

dim(sales1)

# Determine if there are missing values
sum(is.na(sales1))

# Position of missing values by column wise
sapply(sales1, function(x) which(is.na(x)))

# The two missing values are in the release year

##########################
# 4. Load and explore the data.
##########################

# Remove redundant columns
sales2 <- subset(sales1, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(sales2)

dim(sales2)

# Summarise the data set.
summary(sales2)

##########################
# 5. Create plots to review and determine insights.
##########################

qplot(Platform, Global_Sales,
      xlab="Platform",
      ylab="Count",
      title="Number of Genre Games sold by Platform",
      fill=Genre, 
      data=sales1,
      geom='col')

qplot(Platform, 
      xlab="Platform",
      ylab="Count", 
      fill=Genre, 
      data=sales1)

# Create a histogram of the platforms
qplot(Platform,
      data=sales2) + labs(title = 'Platforms')

# Create a histogram of the Global_Sales
qplot(Global_Sales,
      data=sales2, binwidth = 1)+
  labs(title = 'Global Sales')

# Create a scatterplot comparing Product (X-variable) and Global_Sales (Y-variable)
qplot(Product,
      Global_Sales,
      data=sales2) + labs(title = 'Product vs Global Sales')

# Create a scatterplot comparing Product (X-variable) and Global_Sales (Y-variable) with Platform as 3rd variable
qplot(Product,
      Global_Sales,
      colour=Platform,
      data=sales2)

# Create a scatterplot comparing Platform (X-variable) and Global_Sales (Y-variable)
qplot(Platform,
      Global_Sales,
      data=sales2)

# Create a barplot comparing Platform (X-variable) and Global_Sales (Y-variable)
qplot(Platform,
      Global_Sales,
      colour=NA_Sales,
      data=sales2,
      geom='col')

# Create a barplot comparing Platform (X-variable) and NA_Sales (Y-variable)
qplot(Platform,
      NA_Sales,
      colour=NA_Sales,
      data=sales2,
      geom='col') + labs(title = 'Product vs NA Sales')

# Create a barplot comparing Platform (X-variable) and EU_Sales (Y-variable)
qplot(Platform,
      EU_Sales,
      data=sales2,
      geom='col') + labs(title = 'Product vs EU Sales')

# View the third plot.
qplot(Global_Sales,
      Product,
      data=sales2,
      geom='boxplot')


##########################
# Conclusions/insights
# 
# The 5 platforms that sells most games globally are: X360, PS3, PC, Wii, DS
# The 5 platforms that sells most games in NA are: X360, Wii, DA, GB, NES
# The 5 platforms that sells most games in EU are: Wii, PS3, X360, DS, PS4
# To order the histograms in descending order, to better evaluate the differences.
# To add a fill color in the sales, to better evaluate the most rewarding platform 
# not just in terms of counts but also value.
# To add another column as other_sales, for the rest of the world apart NA and EU.

################################################################################
################################################################################
# Assignment Activity 5: Clean, manipulate, and visualise the data
################################################################################
################################################################################

##########################
# 2. Load and explore the data and continue to use the data frame that you prepared in Week 4.
##########################

# View the data frame to sense-check the data set.
View(sales2)

# View the head of the data frame.
head(sales2)

# Determine the min, max and mean values of all the sales data
apply(sales2, 2, max)

apply(sales2, 2, min)

mean(sales2$Global_Sales)
mean(sales2$NA_Sales)
mean(sales2$EU_Sales)

# Create a summary of the data frame.
summary(sales2)

##########################
# 3. Determine the impact on sales per product_id.
##########################

# Use the group_by functions to sum the values grouped by product and platform.
sales3 <- sales2 %>% group_by(Product) %>% 
  summarise(sum_sales_Global=sum(Global_Sales), 
            sum_sales_NA=sum(NA_Sales), 
            sum_sales_EU=sum(EU_Sales),
            .groups='drop')

# View the head of the data frame.
dim(sales3)

# The number of observations has gone from 352 to 175, 
# as many games are available in more than one platform.

# View the head of the data frame.
head(sales3)

# Create a summary of the data frame.
summary(sales3)


##########################
# 4. Create plots to review and determine insights into the data set.
##########################

# Create a scatterplot that shows the relationship Product and sum_sales_Global
# Set the data source, add mapping elements.
ggplot(data = sales3,
       mapping = aes(x = Product, y = sum_sales_Global)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')


ggplot(Platform,
             data=sales) + labs(title = 'Platforms')

qplot(Product, fill=sum_sales_Global, data=sales1, geom='bar')

qplot(Product, sum_sales_Global, data=sales1)


ggplot (data = sales1, 
        mapping=aes(x = Product, y = sum_sales_Global)) + 
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) 


##########################
# 5. Determine the normality of the data set (sales data).
##########################

#####
# Global Sales
# Compute descriptive statistics.

# Use the summary() function to return min, max, 1st Qu, 3rd Qu, Mean.
summary(sales3$sum_sales_Global)

# Range = Max - Min.
max(sales3$sum_sales_Global)- min(sales3$sum_sales_Global) 

# Calculate IQR.
IQR(sales3$sum_sales_Global)  

# Determine the variance.
var(sales3$sum_sales_Global)  

# Return the standard deviation.
sd(sales3$sum_sales_Global)  

# Check the distribution of the data.
# With a boxplot function.
boxplot(sales3$sum_sales_Global)

# With a histogram function.
hist(sales3$sum_sales_Global)

# Determine normality of data.
# Specify qqnorm function (draw a qqplot).
qqnorm(sales3$sum_sales_Global,
       col='blue',
       xlab="Global Sales",
       ylab='#')

# Specify qqline function.
qqline(sales3$sum_sales_Global,
       col='red',
       lwd=2) 

## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(sales3$sum_sales_Global)

## Skewness and Kurtosis
# load packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
skewness(sales3$sum_sales_Global) 
kurtosis(sales3$sum_sales_Global)

#####
# NA Sales
# Compute descriptive statistics.

# Use the summary() function to return min, max, 1st Qu, 3rd Qu, Mean.
summary(sales3$sum_sales_NA)

# Range = Max - Min.
max(sales3$sum_sales_NA)- min(sales3$sum_sales_NA) 

# Calculate IQR.
IQR(sales3$sum_sales_NA)  

# Determine the variance.
var(sales3$sum_sales_NA)  

# Return the standard deviation.
sd(sales3$sum_sales_NA)  

# Check the distribution of the data.
# With a boxplot function.
boxplot(sales3$sum_sales_NA)

# With a histogram function.
hist(sales3$sum_sales_NA)

# Determine normality of data.
# Specify qqnorm function (draw a qqplot).
qqnorm(sales3$sum_sales_NA,
       col='dark green',
       xlab="NA Sales",
       ylab='#')

# Specify qqline function.
qqline(sales3$sum_sales_NA,
       col='red',
       lwd=2) 

## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(sales3$sum_sales_NA)

## Skewness and Kurtosis
# Specify the skewness and kurtosis functions.
skewness(sales3$sum_sales_NA) 
kurtosis(sales3$sum_sales_NA)

#####
# EU Sales
# Compute descriptive statistics.

# Use the summary() function to return min, max, 1st Qu, 3rd Qu, Mean.
summary(sales3$sum_sales_EU)

# Range = Max - Min.
max(sales3$sum_sales_EU)- min(sales3$sum_sales_EU) 

# Calculate IQR.
IQR(sales3$sum_sales_EU)  

# Determine the variance.
var(sales3$sum_sales_EU)  

# Return the standard deviation.
sd(sales3$sum_sales_EU)  

# Check the distribution of the data.
# With a boxplot function.
boxplot(sales3$sum_sales_EU)

# With a histogram function.
hist(sales3$sum_sales_EU)

# Determine normality of data.
# Specify qqnorm function (draw a qqplot).
qqnorm(sales3$sum_sales_EU,
       col='orange',
       xlab="EU Sales",
       ylab='#')

# Specify qqline function.
qqline(sales3$sum_sales_NA,
       col='red',
       lwd=2) 

## Shapiro-Wilk test:
# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(sales3$sum_sales_EU)

## Skewness and Kurtosis
# Specify the skewness and kurtosis functions.
skewness(sales3$sum_sales_EU) 
kurtosis(sales3$sum_sales_EU)

# Determine if there is any correlation between the sales data columns

# Subset the dataframe to get only sales numeral variables
salesnum <- subset(sales3, select = -c(Product))

# View the correlation between the sales variables
round(cor(salesnum), digits=2)


##########################
# 6. Create plots to gain insights into the sales data.
##########################

# Compare all the sales data (columns) for any correlation(s)


##########################
# Conclusion
# The significant skewness and kurtosis clearly indicate that data are not normal,
# and that is the same for Global, EU and NA sales.
# There is a positive correlation between Global vs NA, Global vs EU, NA vs EU sales.
# The correltation is particularly strong for Global vs NA, Global vs EU, 
# milder for NA vs EU sales.

################################################################################
################################################################################
# Assignment Activity 6: Making recommendations to the business
################################################################################
################################################################################

# Import the necessary libraries.
library(forecast)
library(tseries)

# apply linear regression model
model_NA_vs_EU <- lm(sum_sales_NA ~ sum_sales_EU, data = sales3)

# view the lm model
model_NA_vs_EU

# view a summary of the lm model.
summary(model_NA_vs_EU)

# Plot the model residuals
plot(model_NA_vs_EU$residuals)

# See the coefficients of the model.
coefficients(model_NA_vs_EU)

# Plot the model residuals with best fit line
abline(coefficients(model_NA_vs_EU))

# Add the new variable (log of the CPI)
sales4 <- mutate(sales3,
                 logGlobalsales = log(sum_sales_Global), 
                 logNAsales = log(sum_sales_NA), 
                 logEUsales = log(sum_sales_EU))

# See the modified dataset.
head(sales4)

#########
# apply linear regression model
model2 <- lm(logNAsales ~ sum_sales_EU, data = sales4)

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

#########
# apply linear regression model
model3 <- lm(logEUsales ~ sum_sales_NA, data = sales4)

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

#########
# apply linear regression model
model4 <- lm(logGlobalsales ~ sum_sales_NA, data = sales4)

# view the lm model
model4

# view a summary of the lm model.
summary(model4)

# Plot the model residuals
plot(model4$residuals)

# See the coefficients of the model.
coefficients(model4)

# Plot the model residuals with best fit line
abline(coefficients(model4))

#########
# apply linear regression model
model5 <- lm(sum_sales_Global ~ sum_sales_NA, data = sales4)

# view the lm model
model5

# view a summary of the lm model.
summary(model5)

# Plot the model residuals
plot(model5$residuals)

# See the coefficients of the model.
coefficients(model5)

# Plot the model residuals with best fit line
abline(coefficients(model5))

#########
# apply linear regression model
model6 <- lm(sum_sales_Global ~ sum_sales_EU, data = sales4)

# view the lm model
model6

# view a summary of the lm model.
summary(model6)

# Plot the model residuals
plot(model6$residuals)

# See the coefficients of the model.
coefficients(model6)

# Plot the model residuals with best fit line
abline(coefficients(model6))

#########
# apply linear regression model
model7 <- lm(sum_sales_EU ~ sum_sales_Global, data = sales3)

# view the lm model
model7

# view a summary of the lm model.
summary(model7)

# Plot the model residuals
plot(model7$residuals)

# See the coefficients of the model.
coefficients(model7)

# Plot the model residuals with best fit line
abline(coefficients(model7))


################################################
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
# 67.85      8.36*     4.32     5.6*      23.21




################################################
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