# install and loading packages

library(readr)
install.packages("readxl")
library(readxl)
install.packages("BBmisc")
library(BBmisc)
install.packages('Hmisc')
library(Hmisc)
df<- read_excel("Train.xlsx")
df
summary(df)

#boxplot 
boxplot(df, main = "Boxplot of My Data", xlab = "My Data", ylab = "Values")


#replace outliers

replaceOuts = function(df) {
  map_if(df, is.numeric, 
         ~ replace(.x, .x %in% boxplot.stats(.x)$out, median(.x))) %>%
    bind_cols 
}


newdf<-replaceOuts(df[,1:11])
boxplot(newdf, main = "Boxplot of My Data", xlab = "My Data", ylab = "Values")


# method = range for normalisation 
scaled_df = normalize(newdf[,1:11], method = "range", range = c(0, 1))

summary(scaled_df)
scaled_df

#normalised data frame

df1<-data.frame(scaled_df,Y=df$Y)
df1
boxplot(df1[,1:11], main = "Boxplot of My Data", xlab = "My Data", ylab = "Values")

