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
