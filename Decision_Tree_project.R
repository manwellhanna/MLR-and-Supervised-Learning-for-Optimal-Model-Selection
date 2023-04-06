# install and loading packages

library(readr)
install.packages("readxl")
library(readxl)
install.packages("BBmisc")
library(BBmisc)

df<- read_excel("Train.xlsx")
df
summary(df)

#boxplot 
boxplot(df, main = "Boxplot of My Data", xlab = "My Data", ylab = "Values")



# method = range for normalisation 
scaled_df = normalize(df[,1:12], method = "range", range = c(0, 1))

summary(scaled_df)
scaled_df

#normalised data frame

df1<-data.frame(scaled_df,Y=df$Y)
df1

#Splitting the dataset into Training Set & Test Set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$Y, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

library(rpart)  # load rpart package

model <- rpart(Y ~ ., data = training_set, method = "anova", maxdepth = 3)

# fit the decision tree model on the training data
model_fit <- predict(model, newdata = training_set)

# use the decision tree model to predict the response variable for the testing data
predictions <- predict(model, newdata = test_set)

# calculate the mean squared error (MSE)
mse <- mean((predictions - test_set$Y)^2)
print(paste("Mean Squared Error:", mse))

# calculate the R-squared
rsq <- 1 - sum((test_set$Y - predictions)^2)/sum((test_set$Y - mean(test_set$Y))^2)
print(paste("R-squared:", rsq))

#Calculate rmse 
rmse <- sqrt(mean((predictions - test_set$Y)^2))
print(paste("Root Mean Squared Error:", rmse))