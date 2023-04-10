# install and loading packages

library(readr)
#install.packages("readxl")
library(readxl)
#install.packages("BBmisc")
library(BBmisc)
#install.packages('Hmisc')
library(Hmisc)
#install.packages('GGally')
library(magrittr)
#install.packages('magrittr ')
#install.packages('ggplot2')
library(purrr)
library(corrplot)
library(dplyr)
df<- read_excel("Train.xlsx")
df
summary(df)

#examine the data
pairs(df, pch = 25, col = "steelblue")
library(ggplot2)

cor_mat <- cor(df)
corrplot(cor_mat, method="color", number.cex = 0.8,addCoef.col = 'black',
         col=colorRampPalette(c("#7F0000", "#FFFFFF", "#007F00"))(200))



#note--x1 and x3 have correlation
#x1 and x8 have negative, x1 and x9 have postitve
#x8 and x3 have neg corr


#boxplot 
boxplot(df, main = "Boxplot of the Data (Fig 1)", xlab = "Data", ylab = "Values", col="orange",
        border="brown", horizontal=FALSE, notch=FALSE)


#replace outliers

replaceOuts = function(df) {
  map_if(df, is.numeric, 
         ~ replace(.x, .x %in% boxplot.stats(.x)$out, median(.x))) %>%
    bind_cols 
}



newdf<-replaceOuts(df[,1:11])
boxplot(newdf,main = "Boxplot of the Data outliers replaced (Fig 2)", xlab = "Data", ylab = "Values", col="orange",
        border="brown", horizontal=FALSE, notch=FALSE)


# method = range for normalization 
scaled_df = normalize(newdf[,1:11], method = "range", range = c(-1, 1))

summary(scaled_df)
scaled_df

#normalised data frame

df1<-data.frame(scaled_df,Y=df$Y)
df1
boxplot(df1[,1:11], main = "Boxplot of normalised dataset (Fig 3)", xlab = "Data", ylab = "Values", col="orange",
        border="brown", horizontal=FALSE, notch=FALSE)

library(MASS)

# Split the dataset into training and testing sets
set.seed(123)
train <- sample(nrow(df1), 0.7 * nrow(df1))
train_data <- df1[train, ]
train_data
test_data <- df1[-train, ]

library(tidyverse)
library(caret)
#install.packages("caret")
theme_set(theme_bw())

# Build the model
model <- lm(Y ~X1+X2+X3+X4+X5+X6+X7+X8+X10+X11, data = train_data)
# Summarize the model
summary(model)
# Make predictions
predictions <- model %>% predict(test_data)

rmse <- sqrt(mean((test_data$Y -predictions )^2))
rmse

# Build the model after removing correlated variables
modelp <- lm(Y ~X2+X3+X4+X5+X6+X7+X10+X11, data = train_data)
# Summarize the model
summary(modelp)
# Make predictions
predictions <- modelp %>% predict(test_data)

rmse <- sqrt(mean((test_data$Y -predictions )^2))
rmse


#LET'S CHECK THE ASSUMPTIONS

#ASSUMPTION 1-NO MULTI COLLINEARITY
library(car)

vif(model)
#create vector of VIF values
vif_values <- vif(model)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)


#ASSUMPTION 2:
#Multiple linear regression assumes that each observation in the dataset is independent.

#The simplest way to determine if this assumption is met is to perform a Durbin-Watson test, 
#which is a formal statistical test that tells us whether or not the residuals 
#(and thus the observations) exhibit autocorrelation.

durbinWatsonTest(model)
# p-value is less than 0.05, we can reject the null hypothesis and
#conclude that the residuals in this regression model are autocorrelated.

#test stats is <2 indicates positive correlation
#In general, if d is between 1.5 and 2.5
#then autocorrelation is likely not a cause for concern.
#we have t-stats(d) between 1.5 and 2.5


#however For positive serial correlation, 
# we can consider adding lags of the dependent and/or independent variable to the model.


# Load the "dplyr" package
library(dplyr)

# Create a lagged version of the dependent variable "y" (in this case, lagged by 1 time period)
df2 <- df1 %>% mutate(y_lag = lag(Y, 1))
# Split the dataset into training and testing sets
set.seed(123)
train_l<- sample(nrow(df2), 0.7 * nrow(df2))
train_data_l <- df2[train_l, ]
train_data_l
test_data_l <- df2[-train_l, ]
test_data_l

# Build the model
model2 <- lm(Y ~X1+X2+X3+X4+X5+X6+X7+X8+X10+X11+y_lag, data = train_data_l)
# Summarize the model
summary(model2)
durbinWatsonTest(model2)
summary(model)
#as you can see no significant improvement.

#ASSUMPTION 3:Homoscedasticity
summary(model)
plot(fitted(model), resid(model),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Heteroscedasticity plot")
abline(0,0)

#load lmtest package
library(lmtest)

#perform Breusch-Pagan test
bptest(model)
#p-value from the test is less than 0.05 we will reject the null hypothesis 
#and conclude that heteroscedasticity is a problem in this model.

#Let's try few things to imrpove
#log of dependent variable

# Load the "ggplot2" package
library(ggplot2)


# Create a histogram of the residuals for the original model
ggplot(data.frame(resid = resid(model)), aes(x = resid)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Residuals of original model")

# Fit a linear regression model with the transformed response variable "log(Y)"
model3 <- lm( log(Y) ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11, data = train_data)

# Create a histogram of the residuals for the transformed model
ggplot(data.frame(resid = resid(model3)), aes(x = resid)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black") +
  labs(title = "Residuals of transformed model")

summary(model3)

# Make predictions
predictions <- model3 %>% predict(test_data)

rmse <- sqrt(mean((log(test_data$Y) -predictions )^2))
rmse 
bptest(model3)
plot(fitted(model3), resid(model3),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Heteroscedasticity plot")
abline(0,0)
#THIS WORKS! P-value is closer to 0.05 and rmse is improved
#and so is residual standard error

# Create a normal Q-Q plot of the residuals
qqnorm(model3$residuals)
qqline(model3$residuals)

#lets compare it with original model
qqnorm(model$residuals)
qqline(model$residuals)
plot(model3, which = 5)
plot(model, which = 5)

#391, 691 and 833 are high influence

to.rm <- c(391,691,833)
# X.train_red[to.rm,]
test_data_3 <- test_data[-to.rm,]
rownames(test_data_3) <- NULL


to.rm <- c(391,691,833)
# X.train_red[to.rm,]
train_data_3 <- train_data[-to.rm,]
rownames(train_data_3) <- NULL

model4 <- lm( log(Y) ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11, data = train_data_3)
summary(model4)
# Make predictions
predictions <- model4 %>% predict(test_data_3)

rmse <- sqrt(mean((log(test_data_3$Y) -predictions )^2))
rmse

#no significant improvement

#final model:

model3 <- lm( log(Y) ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11, data = train_data)
summary(model3)
# Make predictions
predictions <- model3 %>% predict(test_data)

rmse <- sqrt(mean((log(test_data$Y) -predictions )^2))
rmse
