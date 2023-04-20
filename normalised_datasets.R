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

df_TEST<- read_excel("Test.xlsx")
df_TEST
summary(df_TEST)
#replace outliers

replaceOuts = function(df) {
  map_if(df, is.numeric, 
         ~ replace(.x, .x %in% boxplot.stats(.x)$out, median(.x))) %>%
    bind_cols 
}




newdf<-replaceOuts(df[,1:11])
boxplot(newdf, main = "Boxplot of TRAIN Data", xlab = "My Data", ylab = "Values")

newdf_TEST<-replaceOuts(df_TEST[,1:11])
boxplot(newdf_TEST, main = "Boxplot of TEST Data", xlab = "My Data", ylab = "Values")

# method = range for normalization of TRAIN DATA
scaled_df = normalize(newdf[,1:11], method = "range", range = c(-1, 1))

summary(scaled_df)
scaled_df

# method = range for normalization of TEST DATA
scaled_df_TEST = normalize(newdf_TEST[,1:11], method = "range", range = c(-1, 1))

summary(scaled_df_TEST)
scaled_df_TEST

#normalised data frame TRAIN DATA

df_TRAIN<-data.frame(scaled_df,Y=df$Y)
df_TRAIN
boxplot(df_TRAIN[,1:11], main = "Boxplot of TRAIN Data", xlab = "My Data", ylab = "Values")


#normalised data frame TEST DATA

df_TEST<-data.frame(scaled_df_TEST,Y=df_TEST$Y)
df_TEST
boxplot(df_TEST[,1:11], main = "Boxplot of TEST Data", xlab = "My Data", ylab = "Values")
