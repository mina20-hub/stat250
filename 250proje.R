getwd()
setwd("C:/Users/USER/Desktop/250proje")
data <- read.csv("Aemf1.csv")
data2 <-  as.data.frame(data)


mean(data2$Person.Capacity)

library("dplyr")
data_numeric <- select_if(data2,is.numeric)
lapply(data_numeric,mean)
lapply(data_numeric,sd)

summary(data2)


data_categoric <- select_if(data2, is.character)
lapply(data_categoric,table)

library(dplyr)
library(magrittr)
library(ggplot2)
ggplot(data2, aes(x = City)) + 
  geom_bar()

ggplot(data2, 
       aes(x = City, 
           fill = Room.Type)) + 
  geom_bar(position = "stack")

ggplot(data2, 
       aes(x = City, 
           fill = Day)) + 
  geom_bar(position = "stack")

ggplot(data2, 
       aes(x = City, 
           fill = Superhost)) + 
  geom_bar(position = "stack")

ggplot(data2, 
       aes(x = City, 
           fill = )) + 
  geom_bar(position = "stack")

ggplot(data2, 
       aes(x = City, 
           fill = Room.Type)) + 
  geom_bar(position = "dodge")

ggplot(data2, 
       aes(x = Price, 
           y = Metro.Distance..km.)) +
  geom_point()

ggplot(data2, 
       aes(y = Room.Type, 
           x = Price)) +
  geom_jitter() 
  
#set.seed(250)

#sample_1 <-  data2[sample(nrow(data2), size = 1067, replace = FALSE),]
#mean(sample_1$Price)
mean(data2$Price)

lm_result <- lm(Metro.Distance..km.~Attraction.Index, data=data2)
plot(data2$Metro.Distance..km., data2$Attraction.Index, 
     xlab = "attraction", ylab = "metro dist",
     main = "Relationship between ",
     col = "blue", pch = 16)
correlation <- cor(data2$Metro.Distance..km., data2$Attraction.Index)
abline(lm_result, col = "red")

data2[data2$City=="Budapest" & data2$Day=="Weekend",]

lm_result <- lm(Price~Metro.Distance..km., data=data2)
plot(data2$Metro.Distance..km., data2$Price, 
     xlab = "attraction", ylab = "metro dist",
     main = "Relationship between ",
     col = "blue", pch = 16)
correlation <- cor(data2$Metro.Distance..km., data2$Price)
abline(lm_result, col = "red")


plot(data2$Metro.Distance..km., data2$Price, 
     xlab = "attraction", ylab = "metro dist",
     main = "Relationship between ",
     col = "blue", pch = 16)