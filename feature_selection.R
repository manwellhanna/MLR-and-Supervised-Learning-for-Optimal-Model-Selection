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


#replace outliers

replaceOuts = function(df) {
  map_if(df, is.numeric, 
         ~ replace(.x, .x %in% boxplot.stats(.x)$out, median(.x))) %>%
    bind_cols 
}



newdf<-replaceOuts(df[,1:11])
boxplot(newdf, main = "Boxplot of My Data", xlab = "My Data", ylab = "Values")


# method = range for normalization 
scaled_df = normalize(newdf[,1:11], method = "range", range = c(-1, 1))

summary(scaled_df)
scaled_df

#normalised data frame

df1<-data.frame(scaled_df,Y=df$Y)
df1
boxplot(df1[,1:11], main = "Boxplot of My Data", xlab = "My Data", ylab = "Values")

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

#build model
model3 <- lm( log(Y) ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11, data = train_data)

library(Metrics)

# Fit the forward, backward, and stepwise models on the training set
forward_model <- step(model3, direction="forward", trace=FALSE)
backward_model <- step(model3, direction="backward", trace=FALSE)
stepwise_model <- step(model3, direction="both", trace=FALSE)

# Make predictions on the testing set using each model
pred_full <- predict(model3, newdata=test_data)
pred_forward <- predict(forward_model, newdata=test_data)
pred_backward <- predict(backward_model, newdata=test_data)
pred_stepwise <- predict(stepwise_model, newdata=test_data)

# Calculate RMSE and MAE for each model
rmse_full <- rmse(log(test_data$Y), pred_full)
rmse_forward <- rmse(log(test_data$Y), pred_forward)
rmse_backward <- rmse(log(test_data$Y), pred_backward)
rmse_stepwise <- rmse(log(test_data$Y), pred_stepwise)


# Print the RMSE and MAE for each model
cat("RMSE for full model:", rmse_full, "\n")
cat("RMSE for forward model:", rmse_forward, "\n")
cat("RMSE for backward model:", rmse_backward, "\n")
cat("RMSE for stepwise model:", rmse_stepwise, "\n")

