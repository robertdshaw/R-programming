
# Regression modelling ----------------------------------------

# Dataset: blocket.se
# Mission: Create a predictive model with "selling_price" as target value. 

library(readxl)
library(dplyr)
library(ggplot2)
#installed.packages()
#install.packages("stringr")
library(stringr)
#install.packages("VIM") 
library(VIM)
#install.packages("corrplot")
library(corrplot)
#install.packages("caret")
#install.packages("glmnet")
library(caret)
library(glmnet)
#install.packages("car")
library(car)
#install.packages("sandwich")
#install.packages("lmtest")
library(sandwich)
library(lmtest)
#install.packages("orcutt")
library(orcutt)

# 1. Load and inspect the data
file_path <- "C:/Users/rshaw/Desktop/EC Utbildning - Data Science/Kurs 6 - R Programmering/KKontroll/data_insamling_blocket_complete.xls"
blocket_car_data <- read_excel(file_path)

#view(blocket_car_data)
dim(blocket_car_data)
head(blocket_car_data)
summary(blocket_car_data)

# 2. Data Preprocessing 
# Check for missing values
colSums(is.na(blocket_car_data))

# Feature selection is important to ensure we build a model we can interpret properly.Therefore,
# we will remove brand and first_registration_date straight away as the former was used as part
# of our model purpose and so does not factor in and we remove first_registration_date as this is
# easier to replace with year. Also, delete index as we don't need it.

blocket_car_data <- blocket_car_data %>% 
  select(-brand, -first_registration_date, -index)
dim(blocket_car_data)

# Suspected problem w/ horsepower (when converting to numeric (default to NA)), so decided to check it,
# and so removed all types of whitespace characters and converted to numeric.
str(blocket_car_data$horsepower)
blocket_car_data$horsepower <- as.numeric(str_extract(blocket_car_data$horsepower, "\\d+"))
str(blocket_car_data$horsepower)

# Fix color feature so it has just the basic color category and get rid of problem with trailing
# spaces in engine size and convert to numeric.
blocket_car_data$color <- gsub("\\s*\\(.*", "", blocket_car_data$color)
blocket_car_data$engine <- as.numeric(str_extract(blocket_car_data$engine, "\\d+"))
str(blocket_car_data$color)
str(blocket_car_data$engine)
str(blocket_car_data)

# Convert remaining chr features to factor (categorical) data types in preparation for our linear
# regression model
blocket_car_data$seller_type <- as.factor(blocket_car_data$seller_type)
blocket_car_data$fuel <- as.factor(blocket_car_data$fuel)
blocket_car_data$transmission <- as.factor(blocket_car_data$transmission)
blocket_car_data$vehicle_type <- as.factor(blocket_car_data$vehicle_type)
blocket_car_data$drivetrain <- as.factor(blocket_car_data$drivetrain)
blocket_car_data$color <- as.factor(blocket_car_data$color)
blocket_car_data$model <- as.factor(blocket_car_data$model)
dim(blocket_car_data)
str(blocket_car_data)

# Handling Missing Data incl. Categorical Imputation
colSums(is.na(blocket_car_data))
#view(blocket_car_data)

# Subset data where 'engine' is missing
missing_engine_data <- blocket_car_data[is.na(blocket_car_data$engine), ]

# Summarize missing 'engine' counts by 'fuel' type
summary_by_fuel <- missing_engine_data %>%
  group_by(fuel) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(proportion = count / sum(count))  # Adding proportion of each fuel type
print(summary_by_fuel)
ggplot(summary_by_fuel, aes(x = fuel, y = count, fill = fuel)) +
  geom_bar(stat = "identity") +
  labs(title = "Missing Engine Values by Fuel Type", x = "Fuel Type", y = "Count of Missing Engines") +
  theme_minimal()

# Setting 0 for missing values in electric cars (too many to delete observations and important 
# as denotes simply that they don't have an engine, and imputing median values for other missing fuel 
# types.
blocket_car_data$engine[is.na(blocket_car_data$engine)] <- 0  
median_engine <- median(blocket_car_data$engine[blocket_car_data$engine != 0], na.rm = TRUE)
blocket_car_data$engine[is.na(blocket_car_data$engine)] <- median_engine
median_engine
sum(is.na(blocket_car_data$engine))
dim(blocket_car_data)
colSums(is.na(blocket_car_data))

# Next we check which exact observations still have missing values and why
any_missing_indices <- which(apply(blocket_car_data, 1, function(x) any(is.na(x))))
any_missing_data <- blocket_car_data[any_missing_indices, ]
#view(any_missing_data)

# We see that either the seller didn't input values in the add and/or our data team didn't gather the data properly from blocket.
# Either way, given we only have less than 5% missing values, we remove the remaining NA rows from the 'vehicle_type', 'drivetrain',
# 'color', 'transmission' and 'model' columns.
# Filter out rows where any of the specified columns have missing values
blocket_car_data <- blocket_car_data[!is.na(blocket_car_data$vehicle_type) & 
                                       !is.na(blocket_car_data$drivetrain) & 
                                       !is.na(blocket_car_data$color) & 
                                       !is.na(blocket_car_data$transmission) & 
                                       !is.na(blocket_car_data$model), ]
colSums(is.na(blocket_car_data))
dim(blocket_car_data)
str(blocket_car_data)

# Check factor levels as something seems wrong with the transmission category in particular, where
# it should have 2 levels not 3
levels(blocket_car_data$seller_type)
levels(blocket_car_data$transmission)
levels(blocket_car_data$color)
levels(blocket_car_data$fuel)
levels(blocket_car_data$model)

# We see that there seems to be a break included in 153 observations, that caused R to incorrectly 
# create a new level. So we remove them and recreate transmission as a factor with 2 levels.
rows_with_automat_n <- blocket_car_data[blocket_car_data$transmission == "Automat\n", ]
rows_with_automat_n
blocket_car_data$transmission <- gsub("\n", "", blocket_car_data$transmission)
blocket_car_data$transmission <- factor(blocket_car_data$transmission)

str(blocket_car_data)
#view(blocket_car_data)
dim(blocket_car_data)

# Check for duplicates and we see that there are 52 duplicates
blocket_car_data[duplicated(blocket_car_data, fromLast = TRUE), ]

# So we drop them from the dataset using only unique values 
sum(duplicated(blocket_car_data, fromLast = TRUE))
blocket_car_data <- unique(blocket_car_data)
dim(blocket_car_data)

# 3. EDA - explore relationships and check anomalies and outliers 
#We do a preliminary check for outliers for key numerical and categorical features
#summary option
# UNIVARIATE ANALYSIS 
# Numerical features
ggplot(blocket_car_data, aes(x = factor(1), y = selling_price)) +
  geom_boxplot(fill = "red", alpha = 0.5) +
  geom_jitter(width = 0.2, color = "blue", alpha = 0.5) +
  labs(title = "Boxplot of Selling Prices with Data Points", y = "Selling Price") +
  xlab(" ")  

#Here we can see that there appear to be some extreme points in sales but they mostly appear normal
sp_outliers_sell_price <- boxplot.stats(blocket_car_data$selling_price)$out
sp_outliers_indices_sell_price <- which(blocket_car_data$selling_price %in% sp_outliers_sell_price)
sp_outliers_data_sell_price <- blocket_car_data[sp_outliers_indices_sell_price, ]
#view(sp_outliers_data_sell_price)

hist(blocket_car_data$horsepower, main = "Distribution of Horsepower", xlab = "Horsepower")
hist(blocket_car_data$mil_driven, main = "Distribution of Mil Driven", xlab = "Mil Driven")
hist(blocket_car_data$engine, main = "Distribution of Engine Size", xlab = "Engine Size")
hist(blocket_car_data$year, main = "Distribution of Model Year", xlab = "Model Year")
#Categorical Features
barplot(table(blocket_car_data$fuel), main = "Distribution of Fuel Types", xlab = "Fuel Type", ylab = "Frequency")
barplot(table(blocket_car_data$transmission), main = "Distribution of Transmission Types", xlab = "Transmission", ylab = "Frequency")
barplot(table(blocket_car_data$seller_type), main = "Distribution of Seller Types", xlab = "Seller Type", ylab = "Frequency")
barplot(table(blocket_car_data$vehicle_type), main = "Distribution of Vehicle Types", xlab = "Vehicle Type", ylab = "Frequency")
barplot(table(blocket_car_data$drivetrain), main = "Distribution of Drivetrain", xlab = "Drivetrain", ylab = "Frequency")
barplot(table(blocket_car_data$model), main = "Distribution of Model Types", xlab = "Model Type", ylab = "Frequency")

#These distributions are fairly well balanced, except for vehicle types, so we will create a
# function to group these together based on 4 simplified and intuitive sub-categories
# Define function to simplify vehicle type
simplify_vehicle_type <- function(vehicle_type) {
  if (vehicle_type %in% c("Cab", "Coupé")) {
    return("Convertible_Coupé")
  } else if (vehicle_type %in% c("Familjebuss", "SUV")) {
    return("Family")
  } else if (vehicle_type %in% c("Halvkombi", "Kombi")) {
    return("Hatchback_Wagon")
  } else if (vehicle_type == "Sedan") {
    return("Sedan")
  } else if (vehicle_type == "SUV") {
    return("SUV")
  } else {
    return(vehicle_type)  # Retain other categories as is
  }
}

blocket_car_data <- blocket_car_data %>%
  mutate(simplified_vehicle_type = sapply(vehicle_type, simplify_vehicle_type))
blocket_car_data
#view(blocket_car_data$simplified_vehicle_type)
blocket_car_data$simplified_vehicle_type <- as.factor(blocket_car_data$simplified_vehicle_type)
str(blocket_car_data)
# Remove the original vehicle_type column
blocket_car_data <- blocket_car_data %>%
  select(-vehicle_type)
blocket_car_data <- blocket_car_data %>%
  rename(vehicle_type = simplified_vehicle_type)
#view(blocket_car_data)

#BIVARIATE ANALYSIS
# Here we see quite a few petrol cars have lower sales prices and electrical cars tend to
# to marketed in higher price range categories.
ggplot(blocket_car_data, aes(x = selling_price, fill = fuel)) +
  geom_histogram(bins = 50, color = "black") +  
  ggtitle("Histogram of Selling Prices by fuel type") +
  xlab("Selling Price") +
  ylab("Frequency")

# Based on the following two plots, we can see that both color and model appear to be quite 
# complicated to interpret with many subcategories. We decide to remove color from the dataset 
# and replace model with vehicle type.
ggplot(blocket_car_data, aes(x = selling_price, fill = color)) +
  geom_histogram(bins = 50, color = "black") +  
  ggtitle("Histogram of Selling Prices by car color") +
  xlab("Selling Price") +
  ylab("Frequency")

blocket_car_data <- blocket_car_data %>%
  select(-color, -model)  
head(blocket_car_data)
dim(blocket_car_data)

# Here we see sales price goes down as the number of miles driven increases. 
# There is one particularly extreme outlier,which we assume was a mistake by the seller. 
ggplot(blocket_car_data, aes(x = mil_driven, y = selling_price)) +
  geom_point() +  
  ggtitle("Scatter Plot of Selling Prices by Mil Driven") +
  xlab("Mil Driven") +
  ylab("Selling Price")

str(blocket_car_data)
#view(blocket_car_data)

# Looking closer at the outlier, we see that it's a value over 150,000km
boxplot(blocket_car_data$mil_driven,
        main = "Boxplot of Mil Driven",  
        ylab = "Mil Driven")
# We see from the indexed observation that it's a 2020 car which we deem is extremely likely 
# to be an error so we remove it. 
sp_outliers <- boxplot.stats(blocket_car_data$mil_driven)$out
sp_outliers_indices <- which(blocket_car_data$mil_driven %in% sp_outliers)
sp_outliers_data <- blocket_car_data[sp_outliers_indices, ]
#view(sp_outliers_data)

#Removing outliers
dim(blocket_car_data)
blocket_car_data <- blocket_car_data[blocket_car_data$mil_driven < 150000,]
dim(blocket_car_data)

# The scatter plot shows how selling prices vary based on the model
# year. We can see how newer cars sold by dealers tend to have higher selling prices. 
# There are a few outliers but we'll leave these until modeling later.
ggplot(blocket_car_data, aes(x = year, y = selling_price)) +
  geom_point(aes(color = seller_type), alpha = 0.5) +  
  ggtitle("Selling Prices Over Years") +
  xlab("Model Year") +
  ylab("Selling Price")

# Here we see that generally there is an increase in price with an increase in horsepower
# but it is not perfectly linear.
ggplot(blocket_car_data, aes(x = horsepower, y = selling_price)) +
  geom_point(aes(color = horsepower), alpha = 0.5) +
  ggtitle("Selling Price by Horsepower") +
  xlab("Horsepower") +
  ylab("Selling Price")

ggplot(blocket_car_data, aes(x = engine, y = selling_price)) +
  geom_point(aes(color = engine), alpha = 0.5) +
  ggtitle("Selling Price by Engine Size") +
  xlab("Engine Size (cc)") +
  ylab("Selling Price")

ggplot(blocket_car_data, aes(x = selling_price, fill = seller_type)) +
  geom_histogram(bins = 50, color = "black") +  
  ggtitle("Histogram of Selling Prices by seller") +
  xlab("Selling Price") +
  ylab("Frequency")

# Here we take a preliminary look at the relationship between all numeric features and
# the target variable selling_price.
numeric_data <- blocket_car_data[, sapply(blocket_car_data, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(correlation_matrix, method = "circle")
pairs(numeric_data)

str(blocket_car_data)

#4. Set aside reserve data (approx 20%) as test data for final model
set.seed(123) 
reserved_indices <- sample(nrow(blocket_car_data), 150)
test_data <- blocket_car_data[reserved_indices, ]
train_data <- blocket_car_data[-reserved_indices, ]

str(train_data)
dim(train_data)
dim(test_data)

#5. Estimate Regression Models

summary(train_data$selling_price)# First we check the reference ranges that we 
# will use to assess RMSE scoring on cross validation. 
# Given that the median and mean in our dataset are relatively close to each 
# other and the distribution seems to be somewhat symmetric (as inferred from 
# their closeness), using the mean as a reference makes the most 
# sense for typical business or economic analyses in the used car sales market, 
# where volatility and range of pricing is typically quite large. It directly 
# relates the error magnitude to what you can expect for a central or average 
# sales transaction. However, if precise predictions across the full range of 
# data are crucial, or if avoiding risks associated with outliers is important, 
# then evaluating RMSE against the IQR could provide additional valuable insights.

# 1. Using "logic" / Theory and based on correlation matrix/pair plots from point 4 above, we create a simple
# first model. We see that the Durbin-Watson test is close to 2 so while there are positive correlated residuals
# they are close enough to the desired range to leave.# Multicollinearity isn't a problem here with all 3 variables 
# running well under 5.Adjusted R-Squared is reasonable at 88% but could be higher.
lm_1 <- lm(selling_price ~ mil_driven + year + horsepower, data = train_data)
summary(lm_1)
par(mfrow = c(2, 2))
plot(lm_1)
library(lmtest)
dwtest(lm_1)
vif(lm_1)

# We run Cross Validation on model 1 to test generalization and RMSE metric. 
# RMSE is quite high.
set.seed(123) 
train_control <- trainControl(method = "cv", number = 10)
lm_1_cv <- train(selling_price ~ mil_driven + year + horsepower, data = train_data, method = "lm", trControl = train_control)
lm_1_cv

# 2. 
# There is a bit of a problem with non-linearity and heteroskedasticity. 
# We tried to fix this by transforming the mil_driven variable (most likely non-linear independent variable)
# but this didn't work. So we try taking the log of the target variable and the degree 4 polynomial of two independent variables and
# this fixes the issue. The non-linear problem is gone and the homoskedasticity is resumed.
# Transforming the target variable using the natural logarithm helps in stabilizing the variance and making 
# relationships more linear, given we saw earlier in EDA that we had skewed data and that the range of the 
# selling_price is large.

# The model explains a substantial portion of the variability in the data, with an Adjusted R-squared of 0.9346, 
# indicating a very good fit.

# The VIF for polynomial terms of mil_driven and year are relatively high, but the square roots of their generalized 
# VIFs are around 1.5, which, while indicating some multicollinearity, is not extreme. These factors suggest 
# multicollinearity is present but manageable in this context. Horsepower has a VIF close to 1, indicating minimal issues 
# with multicollinearity for this variable.
lm_2 <- lm(log(selling_price) ~ poly(mil_driven, 4) + poly(year, 4) + horsepower, data = train_data)
summary(lm_2)
par(mfrow = c(2, 2))
plot(lm_2)
vif(lm_2)

# Cross-validation on model 2, which shows a much improved RMSE value 
# and a good "unseen data" fit of 90.3%.
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
lm_2_cv <- train(log(selling_price) ~ poly(mil_driven, 4) + poly(year, 4) + horsepower, data = train_data, method = "lm", trControl = train_control)
lm_2_cv

# There are some outliers and high leverage points to investigate.
# View(train_data[261, ]) #high leverage point. This has a very high selling price for a car from 1982. So we will remove it.
# View(train_data[15, ]) #high leverage point. This has a very low selling price for a car from 2013. So we will remove it.
# View(train_data[4, ]) #outlier. This has a very low selling price for a car from 2022. So we will remove it.
# View(train_data[103, ]) #outlier. This has a very low selling price for a car from 2002. So we will remove it.

dim(train_data)
indices_to_remove <- c(261, 15, 4, 103)
train_data <- train_data[-indices_to_remove, ]
dim(train_data)

# Now there are no more high leverage points, the residual clusters have reduced somewhat and the Q-Q residuals
# plot shows a slightly better straight line of errors. The square roots of the generalized VIFs are around 1.5 
# showing some multicollinearity but not extreme so we'll leave them.
# The tails at both ends of the Q-Q plot could mean our model is missing something important or is set up incorrectly. 
# We will create interactions to see if this helps.
lm_2 <- lm(log(selling_price) ~ poly(mil_driven, 4) + poly(year, 4) + horsepower, data = train_data)
summary(lm_2)
par(mfrow = c(2, 2))
plot(lm_2)
vif(lm_2)

set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
lm_2_cv <- train(log(selling_price) ~ poly(mil_driven, 4) + poly(year, 4) + horsepower, data = train_data, method = "lm", trControl = train_control)
lm_2_cv

# 3. Test Interactions
# Seller Type and Mileage (mil_driven):
# The impact of mileage on selling price could vary depending on whether the seller is a 
# private individual or a dealer. For instance, buyers might expect higher mileage cars 
# from dealers to be better maintained than those from private sellers. This interaction seem to be significant
# and VIFs are low. There is some non-linearity and mild heteroskadisticity but we will run earlier transformations on
# this after testing another interaction. 
lm_interact_1 <- lm(selling_price ~ mil_driven*seller_type + year, data = train_data)
summary(lm_interact_1)
par(mfrow = c(2, 2))
plot(lm_interact_1)
vif(lm_interact_1, type = "predictor")

# Engine size in cc and Horsepower:
# Interaction Term: The significant positive interaction term (engine:horsepower) highlights the fact that 
# the negative impact of larger engine sizes on the selling price is mitigated by higher horsepower. In other 
# words, larger engines that also have high horsepower are valued more than large engines with low horsepower. 
# This interaction should be highlighted when predicting prices or analyzing the market for cars with different 
# engine sizes and horsepower ratings.
# All predictors, including the interaction term, are highly significant, which supports their inclusion in the 
# model.
# The plots show little to no violations of regression assumptions such as non-linearity or heteroscedasticity. 
# The VIFs indicate no multicollinearity. This is a good sign, ensuring that the estimates are reliable.
lm_interact_2 <- lm(selling_price ~ engine*horsepower + year, data = train_data)
summary(lm_interact_2)
par(mfrow = c(2, 2))
plot(lm_interact_2)
vif(lm_interact_2, type = "predictor")

# We decide to build model 3, as an evolved form of model 1 but incorporating both interactions and transforming the 
# target variable with the natural log. This shows a good fit, low RSE and strong statistical significance VIFs are acceptable.
# The plots again show no extreme violations of the norms. There are a few outliers but we will leave them in.
lm_3 <- lm(log(selling_price) ~ mil_driven*seller_type + year + engine*horsepower, data = train_data)
summary(lm_3)
par(mfrow = c(2, 2))
plot(lm_3)
vif(lm_3, type = "predictor")

# Cross-validation on model 3, which shows a low RMSE value and an improved "unseen data" fit of 93%.
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
lm_3_cv <- train(log(selling_price) ~ mil_driven*seller_type + year + engine*horsepower, data = train_data, method = "lm", trControl = train_control)
lm_3_cv

# 4. Best subset regression
# Positive coefficient, suggesting that electric vehicles (or vehicles associated with this category) tend to have 
# higher selling prices.
# mil_driven: Negative coefficient, consistent with expectations that higher mileage reduces vehicle value.
# year: Positive coefficient, indicating that newer models sell at higher prices.
# horsepower: Positive coefficient, affirming that more powerful cars are priced higher.
# vehicle_typeHatchback_Wagon: Negative coefficient, which might suggest that hatchbacks and wagons are less valued 
# compared to the baseline vehicle type in the dataset.
#install.packages("leaps")
library(leaps)
lm_4 <- regsubsets(selling_price ~ ., data=train_data, nvmax=ncol(train_data)-1)
lm_4_summary = summary(lm_4)
lm_4_summary
names(lm_4_summary)
par(mfrow = c(1, 1))
plot(lm_4_summary$adjr2)
lm_4_summary$adjr2
coef(lm_4, 5)
# From the coefficients plot, we see that after 5 features the graph levels out at an adjusted R2 of about 0.91.
# So, we then plot the model with the best variables and we see that they are at 0.91. We keep the whole fuel and 
# vehicle_type variables as while Diesel and Hybrid fuels and Hatchback/Wagon vehicles aren't statistically significant 
# in relation to their respective baselines we know based on domain knowledge that they are strong determinants of
# selling price for used cars and can play a moderator effect when combined with other variables, e.g. horsepower's effect
# on price varied by diesel fuel type. Interestingly, engine size is dropped entirely.
plot(lm_4, scale = "adjr2")

lm_4 <- lm(selling_price ~ mil_driven + year + horsepower + vehicle_type + fuel, data = train_data)    
summary(lm_4)
vif(lm_4)
par(mfrow = c(2, 2))
plot(lm_4)

# Cross-validation on model 4, which shows a high RMSE value (likely due to skewed distribution of the target variable)
# and a good "unseen data" fit of 92%. Diagnostic checks on the final selected model show no major violations of 
# regression assumptions, such as homoscedasticity, normality of residuals, and absence of multicollinearity beyond 
# what's already shown by VIF scores.
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
lm_4_cv <- train(selling_price ~ mil_driven + year + horsepower + vehicle_type + fuel, data = train_data, method = "lm", trControl = train_control)
lm_4_cv

# 6. Evaluation - Comparing models --------------------------------------------------------------
# Define a custom summary function for models trained on log-transformed targets
logTransformSummary <- function(data, lev = NULL, model = NULL) {
  data$obs <- exp(data$obs)  # Transform observed values back to the original scale
  data$pred <- exp(data$pred)  # Transform predicted values back to the original scale
  
  RMSE <- sqrt(mean((data$obs - data$pred)^2))
  R2 <- cor(data$obs, data$pred)^2  # Calculate R-squared
  out <- c(RMSE = RMSE, Rsquared = R2)
  names(out) <- c("RMSE", "Rsquared")
  return(out)
}

# Set up training controls for cross-validation
train_control <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)
train_control_log <- trainControl(method = "cv", number = 10, summaryFunction = logTransformSummary)

# Train models with and without log-transformed target
set.seed(123)
lm_1_cv <- train(selling_price ~ mil_driven + year + horsepower, data = train_data, method = "lm", trControl = train_control)
lm_2_cv <- train(log(selling_price) ~ poly(mil_driven, 4) + poly(year, 4) + horsepower, data = train_data, method = "lm", trControl = train_control_log)
lm_3_cv <- train(log(selling_price) ~ mil_driven * seller_type + year + engine * horsepower, data = train_data, method = "lm", trControl = train_control_log)
lm_4_cv <- train(selling_price ~ mil_driven + year + horsepower + vehicle_type + fuel, data = train_data, method = "lm", trControl = train_control)

# Create a data frame to compare RMSEs and Rsquared for all models
cv_results <- data.frame(
  Model = c("lm_1", "lm_2", "lm_3", "lm_4"),
  RMSE = c(
    min(lm_1_cv$results$RMSE),
    min(lm_2_cv$results$RMSE),
    min(lm_3_cv$results$RMSE),
    min(lm_4_cv$results$RMSE)
  ),
  Rsquared = c(
    max(lm_1_cv$results$Rsquared),
    max(lm_2_cv$results$Rsquared),
    max(lm_3_cv$results$Rsquared),
    max(lm_4_cv$results$Rsquared)
  )
)

# Print out the RMSE and R2 values for all models
print(cv_results)
library(gridExtra)
data <- data.frame(
  Model = c("lm_1", "lm_2", "lm_3", "lm_4"),
  RMSE = c(66235.43, 57016.23, 65227.85, 59722.07),
  Rsquared = c(0.8985383, 0.9243669, 0.9106867, 0.9188256)
)
table_plot <- tableGrob(data)
jpeg("model_metrics.jpg", width = 1200, height = 600)
grid.arrange(table_plot)
dev.off()

# Model with the lowest RMSE
best_model_index <- which.min(cv_results$RMSE)
best_model_name <- cv_results$Model[best_model_index]
print(paste("The best model based on RMSE is:", best_model_name))

# Predict best model lm_2 on the test data
predictions <- predict(lm_2, newdata = test_data)
# Convert predictions back to the original scale given the model was fit to log(selling_price)
predictions <- exp(predictions)

# Calculate RMSE on test data
test_rmse <- sqrt(mean((predictions - test_data$selling_price)^2))
print(paste("Test RMSE for lm_2 is:", test_rmse))

# Calculate R-squared value on test data
rsquared <- cor(predictions, test_data$selling_price)^2
print(paste("R-squared for lm_2 is:", rsquared))

# Mean selling price - It's useful to compare the RMSE with the mean or median selling prices in your test data. 
# A rule of thumb is that an RMSE lower than 10% of the average selling price might be considered good in many business 
# contexts, but this can vary by industry and specific use cases. Ours is just over 16% which is reasonable but could do
# with some improvements, including both an increase in the dataset size and the range of appropriate features.
mean_price <- mean(test_data$selling_price)
# Compare RMSE with the mean selling price
print(paste("Mean selling price: ", mean_price))
print(paste("RMSE as a percentage of mean price for Model 2: ", (test_rmse / mean_price) * 100))

#7. INTERPRET MODELS
summary(lm_2)  # Hypothesis testing
confint(lm_2, level = 0.95)

# Randomly sample 2 rows from test_data
set.seed(123) 
random_rows <- sample(nrow(test_data), 2)
subset_test_data <- test_data[random_rows, ]

# Generate CI and PI for the randomly selected (2 rows of) test data
confidence_intervals <- predict(lm_2, newdata = subset_test_data, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_2, newdata = subset_test_data, interval = "prediction", level = 0.95)
confidence_intervals <- exp(confidence_intervals)
prediction_intervals <- exp(prediction_intervals)
confidence_intervals
prediction_intervals

test_data$Predicted <- exp(predict(lm_2, newdata = test_data))
ggplot(test_data, aes(x = selling_price, y = Predicted)) + geom_point() + geom_smooth(method = "lm", color = "blue")

