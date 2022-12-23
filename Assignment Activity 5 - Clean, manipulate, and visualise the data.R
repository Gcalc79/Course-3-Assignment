##########################
# 1. Continue with your R script in RStudio from Assignment activity 4: Visualise data to gather insights.
##########################

# Import tidyverse library.
library(tidyverse)

# Import the data set (turtle_sales.csv).
sales1 <- read.csv(file.choose(), header=T)

# View the data frame.
View(sales1)

dim(sales1)

# Determine if there are missing values
sum(is.na(sales1))

# Position of missing values by column wise
sapply(sales1, function(x) which(is.na(x)))

# The two missing values found are in the column release year, 
# that is not relevant for the analysis and will be dropped. 

# Remove redundant columns
sales2 <- subset(sales1, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(sales2)

dim(sales2)

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

##########################
# Conclusion
# The significant skewness and kurtosis clearly indicate that data are not normal,
# and that is the same for Global, EU and NA sales.
# There is a positive correlation between Global vs NA, Global vs EU, NA vs EU sales.
# The correltation is particularly strong for Global vs NA, Global vs EU, 
# milder for NA vs EU sales.