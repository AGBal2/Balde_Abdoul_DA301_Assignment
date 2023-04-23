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
##performance by utilizing customer trends. 

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
## utilizing basic statistics and plots. Note that you will use this data set 
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
install.packages('tidyverse')
library(tidyverse)

# Import the data set.
Sales <- read.csv(file.choose(), header = TRUE)

# Print the data frame.
print(Sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_review <- Sales %>% select(-c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(sales_review)

# Convert the product id column into character
#sales_review$Product <- as.character(sales_review$Product)

# View the descriptive statistics.
summary(sales_review)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(y=NA_Sales, data=sales_review)
qplot(y=EU_Sales, data=sales_review)
qplot(y=Global_Sales, data=sales_review)

## 2b) Histograms
# Create histograms.
qplot(NA_Sales, data = sales_review)
qplot(EU_Sales, data = sales_review)
qplot(Global_Sales, data = sales_review)

## 2c) Boxplots
# Create boxplots.
qplot(Propduct, NA_Sales, data = sales_review,
      geom = 'boxplot')
qplot(Product, Global_Sales, data = sales_review,
      geom = 'boxplot')
qplot(Product, EU_Sales, data = sales_review,
      geom = 'boxplot')

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
## Using the scatter plot to view distribution of the different sales data, we could 
 #clearly notice that the data points are clustered along the X axis.
 #There are outliers that we could spot visually though, a group of high sales outliers 
 #across the different regions and the Global sales.
##When visualizing the sales through histogram the following were spotted: The 
 #distribution of the data is very much skewed to the right, the frequencies of the 
 # sales data are lower than the left side due to fewer extreme values
 #for all the sales across the different regions (EU, NA or Global). The different 
 #histograms provide a better insight on where most of the data points are where in this
 #case many have sales data close to the zero value.



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
head(sales_review)

# Check output: Determine the min, max, and mean values.
# Min of sales
min(sales_review[,3])
min(sales_review[,4])
min(sales_review[,5])

# Max of sales
max(sales_review$EU_Sales)
max(sales_review$NA_Sales)
max(sales_review$Global_Sales)

# Mean of sales
mean(sales_review$EU_Sales)
mean(sales_review$NA_Sales)
mean(sales_review$Global_Sales)

# View the descriptive statistics.
summary(sales_review)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_sum <- sales_review %>% group_by(Product)%>%
  summarise(sum_EU_Sales = sum(EU_Sales),
            sum_NA_Sales = sum(NA_Sales),
            sum_Global_Sales = sum(Global_Sales),
            .groups='drop')

# View the data frame.
sales_sum

# Explore the data frame.
# View the descriptive statistics.
summary(sales_sum)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
ggplot(data = sales_sum,
       mapping = aes(x = Product, y = sum_EU_Sales)) +
  geom_point(alpha = 0.5, size = 1.5) +
  labs(title = "Relationship between product id and EU_Sales")

ggplot(data = sales_sum,
       mapping = aes(x = Product, y = sum_NA_Sales)) +
  geom_point(alpha = 0.5, size = 1.5) +
  labs(title = "Relationship between product id and NA_Sales")

ggplot(data = sales_sum,
       mapping = aes(x = Product, y = sum_Global_Sales)) +
  geom_point(alpha = 0.5, size = 1.5) +
  labs(title = "Relationship between product id and Global_Sales")

# Create histograms.
qplot(sum_EU_Sales, data = sales_sum)
qplot(sum_NA_Sales, data = sales_sum)
qplot(sum_Global_Sales, data = sales_sum)

# Create boxplots.
qplot(sum_EU_Sales, data = sales_sum, geom='boxplot')
qplot(sum_NA_Sales, data = sales_sum, geom='boxplot')
qplot(sum_Global_Sales, data = sales_sum, geom='boxplot')

###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Specify qqline function.
qqnorm(sales_sum$sum_EU_Sales)
qqline(sales_sum$sum_EU_Sales)

qqnorm(sales_sum$sum_NA_Sales)
qqline(sales_sum$sum_NA_Sales)

qqnorm(sales_sum$sum_Global_Sales)
qqline(sales_sum$sum_Global_Sales)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_sum$sum_EU_Sales)
shapiro.test(sales_sum$sum_NA_Sales)
shapiro.test(sales_sum$sum_NA_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_sum$sum_EU_Sales) 
kurtosis(sales_sum$sum_EU_Sales)

skewness(sales_sum$sum_NA_Sales) 
kurtosis(sales_sum$sum_NA_Sales)

skewness(sales_sum$sum_Global_Sales) 
kurtosis(sales_sum$sum_Global_Sales)

## 3d) Determine correlation
# Determine correlation.
sales_review1 <- sales_review %>% select(-c(Platform))

round(cor(sales_review1),
      digits=2)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Scatterplot to indicate relation between EU_Slaes and NA_Sales.
ggplot(data = sales_sum,
       mapping = aes(x = sum_EU_Sales, y = sum_NA_Sales)) +
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm')

# Scatterplot to indicate relation between EU_Slaes and Global_Sales.
ggplot(data = sales_sum,
       mapping = aes(x = sum_EU_Sales, y = sum_Global_Sales)) +
  geom_point(color = 'black', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm')

# Scatterplot to indicate relation between NA_Slaes and Global_Sales.
ggplot(data = sales_sum,
       mapping = aes(x = sum_NA_Sales, y = sum_Global_Sales)) +
  geom_point(color = 'blue', alpha = 0.5, size = 1.5) +
  geom_smooth(method = 'lm')

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

## Exploring the shape of the distribution of the sales, using histogram and
 # and boxplot we could see, more clearly with the histogram though,  that the 
 #sales data are extremely skewed to the right as the right tail is longer than the left one
 # and this quite aligns with what the descriptive statistics showed: for Global sales for instance, 
 #75% of the data have a sales value below ~ 13 units when the maximum value is up to ~ 68 units
 #hence the presence of significant outliers in the dataset.
## Ploting the quantile of the distribution of the different sales to compare with 
 #a normal distribution of the same, the following could be noticed visually:
 #for all the sales data, the points were straight to the line until almost one 
 #standard deviation above the mean of the normal where from there the values tend to progressively
 #further from the line until the extreme data points from +2 above the mean of the normal which suggests a heavier tail.
 # Also, using the  hypothesis test such as the Shapiro.wilk test, the very small p-value,
 #suggests to reject the null hypothesis(the distribution is normal).
 #As kurtosis is way above the normal 3 (excess kurtosis of 13 for EU sales for instance),
 #which suggests that the distribution of the different sales has heavy tail than 
 #the normal and signals the presence of extreme data points,
 #that has a significant impact on the distribution of the dataset.
## As for evaluating any potential relationship between the different sales data,
 #it is clear that the sales in the different regions (EU & NA) correlated with 
 #Global sales, at least visually at this stage.

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
head(sales_sum)

# Determine a summary of the data frame.
summary(sales_sum)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
cor(sales_sum$sum_NA_Sales, sales_sum$sum_EU_Sales)
# Plot the relationship with base R graphics.
plot(sales_sum$sum_NA_Sales, sales_sum$sum_EU_Sales)

cor(sales_sum$sum_NA_Sales, sales_sum$sum_Global_Sales)
# Plot the relationship with base R graphics.
plot(sales_sum$sum_NA_Sales, sales_sum$sum_Global_Sales)

cor(sales_sum$sum_EU_Sales, sales_sum$sum_Global_Sales)
# Plot the relationship with base R graphics.
plot(sales_sum$sum_EU_Sales, sales_sum$sum_Global_Sales)

# Fit simple linear regression models.
# Create a model with Eu_Sales & NA_Sales.
model1 <- lm(sum_EU_Sales ~ sum_NA_Sales,
             data=sales_sum)
# Create a model with Eu_Sales & Global_Sales.
model2 <- lm(sum_Global_Sales ~ sum_EU_Sales,
             data=sales_sum)
# Create a model with Global_Sales & NA_Sales.
model3 <- lm(sum_Global_Sales ~ sum_NA_Sales,
             data=sales_sum)

# View the different output
summary(model1)
summary(model2)
summary(model3)

plot(model1$residuals)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales_sum$sum_EU_Sales, sales_sum$sum_NA_Sales)
abline(coefficients(model1))

plot(sales_sum$sum_EU_Sales, sales_sum$sum_Global_Sales)
abline(coefficients(model2))

plot(sales_sum$sum_NA_Sales, sales_sum$sum_Global_Sales)
abline(coefficients(model3))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
cor(sales_sum[, c('sum_EU_Sales', 'sum_NA_Sales', 'sum_Global_Sales')])

# Multiple linear regression model.
model4 = lm(sum_Global_Sales ~ sum_NA_Sales + sum_EU_Sales, data = sales_sum)

# View the model
summary(model4)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Create new data frame
new_a <- data.frame (sum_NA_Sales = c(34.02), sum_EU_Sales = c(23.80))
new_b <- data.frame (sum_NA_Sales = c(3.93), sum_EU_Sales = c(1.56))
new_c <- data.frame (sum_NA_Sales = c(2.73), sum_EU_Sales = c(0.65))
new_d <- data.frame (sum_NA_Sales = c(2.26), sum_EU_Sales = c(0.97))
new_e <- data.frame (sum_NA_Sales = c(22.08), sum_EU_Sales = c(0.52))

# Predict the global sales using provided values
predict(model4, new_a)
predict(model4, new_b)
predict(model4, new_c)
predict(model4, new_d)
predict(model4, new_e)

# Compare with first 10 records
head(sales_sum, 10)

###############################################################################

# 5. Observations and insights
## Speaking about the correlation between the sales columns, we could conclude that
# they are highly correlated: NA sales for instance, alone could explain the variability
# of the Global sales by ~ 84% and also that for every unit of increase in the Global sales,
# there is a 1.6 increase in the NA sales. Also, the relationship between sales in the EU
#region with Global sales is somewhat linear which explains a best fit of the linear model
#of the two variables and the same applies between sales in the North America region and
#the Global sales.
## Further more, when looking at the multi-linear regression model between the different
#sales columns, there is no doubt that the model is strong with an R² of ~ 97% and 
#the two explanatory variables for the Global sales are very significant to the model as the
#the change in sales in the different region very well explain change in Global sales.



###############################################################################
###############################################################################




