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