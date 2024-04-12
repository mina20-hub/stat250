library(dplyr)
library(rstatix)
library(RColorBrewer)
library(viridis)
library(colorspace)

install.packages(c("dplyr", "rstatix", "RColorBrewer", "viridis", "colorspace"))

getwd()
setwd("C:/Users/USER/Desktop/250proje")
df <- read.csv("Aemf1.csv")
mean(df$Price[df$City=="Budapest"])
set.seed(250)
mean(df$Price[df$City=="Budapest"])
df <- df %>% sample_n(5000)
mean(df$Price[df$City=="Budapest"])

#Descriptive statistics
numeric_df <- select_if(berlin_data, is.numeric)
library(corrplot)
mean(df$Price[df$City=="Budapest"])
cor_matrix <- cor(numeric_df)

# Create correlation matrix plot
corrplot(cor_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(100), 
         tl.cex = 0.8, tl.col = "black", tl.srt = 45)

#City count
# Get unique cities in the data
unique_cities <- unique(df$City)
# Generate random colors using RColorBrewer palette
n_colors <- length(unique_cities)
palette <- brewer.pal(n_colors, "Set1")
# Create the plot
ggplot(df, aes(x = City, fill = City)) +
  geom_bar() +
  labs(x = "City", y = "Count") +
  ggtitle("Count of Listings by City") +
  theme_minimal() +
  scale_fill_manual(values = palette)

Bedrooms

ggplot(df, aes(x = factor(Bedrooms))) +
  geom_bar() +
  labs(x = "Number of Bedrooms", y = "Count") +
  ggtitle("Number of Bedrooms Distribution") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

#Price  city


ggplot(df, aes(x = Price, fill = City, color = City)) +
  geom_density(alpha = 0.5,fill = "transparent",linewidth=1.2) +
  labs(x = "Price", fill = "City", color = "City") +
  ggtitle("Density of Prices by City") +
  theme_minimal()+
  scale_fill_manual(values = palette)

#price day
ggplot(df, aes(x = Price, fill = Day)) +
  geom_density(alpha = 0.5) +
  labs(x = "Price", fill = "Day") +
  ggtitle("Price by Days") +
  scale_fill_manual(values = color_palette) +
  theme_minimal()

#price capacity
df$Person.Capacity <- factor(df$Person.Capacity)
ggplot(df, aes(x = Price, fill = Person.Capacity,color =Person.Capacity)) +
  geom_density(alpha = 0.5,fill = "transparent",linewidth=1.2) +
  labs(x = "Price", fill = "Person Capacity") +
  ggtitle("Price by Capacity") +
  scale_fill_manual(values = color_palette) +
  theme_minimal()

#guest satisfaction-clean
ggplot(df, aes(x = as.factor(`Cleanliness.Rating`), y = `Guest.Satisfaction`)) +
  geom_boxplot(width = 0.7, fill = color_palette, outlier.shape = 4) +
  labs(x = "Cleanliness Rating", y = "Guest Satisfaction") +
  theme_minimal()

#super host  or not
df$Superhost <- ifelse(df$Superhost == "True", 1, 0)
# Count the number of Superhost and non-Superhost listings
superhost_count <- sum(df$Superhost == 1)
non_superhost_count <- sum(df$Superhost == 0)

# Create a data frame
pie_data <- data.frame(Category = c("Superhost", "Non-Superhost"),
                       Count = c(superhost_count, non_superhost_count))

# Create the pie chart
ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Host Type") +
  theme_void() +
  ggtitle("Proportion of Superhost vs Non-Superhost Listings")

#3.1 average price of airbnb houses in Lisbon less than 260 -> One sample mean
hist(df$Price[df$City=="Lisbon"])

mean(df$Price[df$City=="Budapest"])


# Shapiro-Wilk test
shapiro.test(df$Price[df$City=="Lisbon"])
#not normal
# Create a boxplot
boxplot(lisbon_prices, horizontal = TRUE, notch = TRUE, main = "Boxplot of Prices in Lisbon",
        xlab = "Price")
df %>% identify_outliers(Price) %>% head()
dff <- df %>% identify_outliers(Price) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
lisbon_prices <- df$Price[df$City == "Lisbon"]
boxplot(lisbon_prices, horizontal = TRUE, notch = TRUE, main = "Boxplot of Prices in Lisbon",
        xlab = "Price")
#h0 median is not less than 260
wilcox.test(df$Price[df$City=="Lisbon"], mu = 260, alternative = "less")

# Subset the data for Lisbon prices
budapest_prices <- mean(df$Price[df$City == "Budapest"])

df[df$City=="Budapest"]$price

# Create a boxplot
boxplot(lisbon_prices, horizontal = TRUE, notch = TRUE, main = "Boxplot of Prices in Lisbon",
        xlab = "Price")
df %>% identify_outliers(Price) %>% head()
dff <- df %>% identify_outliers(Price) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
lisbon_prices <- df$Price[df$City == "Lisbon"]
boxplot(lisbon_prices, horizontal = TRUE, notch = TRUE, main = "Boxplot of Prices in Lisbon",
        xlab = "Price")



#3.2  Is there significant difference in average price of weekdays and weekends in cities -> Two sample mean
#it is  4.most popular city based on our rank and european arego there no matter is day
#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Tourism_statistics_-_top_destinations
# Subset the data for Barcelona
Barcelona_data <- df[df$City == "Barcelona", ]

# Create separate groups for weekends and weekdays
weekend_prices <- Barcelona_data[Barcelona_data$Day == "Weekend", "Price"]
weekday_prices <- Barcelona_data[Barcelona_data$Day == "Weekday", "Price"]
shapiro.test(weekend_prices)
length(weekend_prices)
shapiro.test(df$Price)


head(df)
mean(df$Superhost)
##
mean(df$Attraction.Index)


mean(df$Guest.Satisfaction)
mean(df$Superhost)
mean(df$price)

#3.3 amsterdam super host proportion is greater than .28  -> one sample
#most expensive
Amsterdam_data <- subset(df, City == "Amsterdam")
superhost_count <- sum(Amsterdam_data$Superhost == 1)
prop.test(superhost_count, nrow(Amsterdam_data), p = .28, alternative = "greater")

#3.4 lisbon super host proportion greater than paris  -> two sample
#https://www.wereldreizigers.nl/en/worldly/safest-most-dangerous-countries-europe/
#https://www.cntraveller.com/gallery/friendliest-countries-in-europe
#testing being safest also mean well behave to host

lisbon_data <- subset(df, City == "Lisbon")
paris_data <- subset(df, City == "Paris")


city_proportions <- df %>%
  group_by(City) %>%
  summarize(Superhost_Proportion = mean(Superhost == "True"))

city_proportions
mean(city_proportions)

superhost_mean <- mean(city_proportions$Superhost_Proportion)
superhost_mean

fiyat <- df %>%
  group_by(City) %>%
  summarize(price_mean = mean(df$Price))
fiyat
mean_prices <- aggregate(Price ~ City, data = df, FUN = mean)
mean(mean_prices)

lisbon_superhost_count <- sum(lisbon_data$Superhost == 1)
paris_superhost_count <- sum(paris_data$Superhost == 1)

lisbon_total_listings <- nrow(lisbon_data)
paris_total_listings <- nrow(paris_data)

# Perform the two-sample proportion hypothesis test
prop.test(c(lisbon_superhost_count, paris_superhost_count),
          c(lisbon_total_listings, paris_total_listings),
          alternative = "greater")

#3.5.1  is there any rel btw price and metro distance -> simple corrolation 
#https://www.timeout.com/news/its-official-this-european-city-has-the-worlds-best-public-transport-040523
#because of they have best public transport
summary(df$City.Center..km.[df$City=="Vienna"])
heatmap(cor_matrix)
berlin_data <- subset(df, City == "Vienna")
# Perform simple linear regression
df$City.Center..km.
lm_model <- lm(Price ~  Metro.Distance..km. , data = df)
summary(lm_model)
library(ggplot2)
cor(df$Price,df$Metro.Distance..km.,method = "spearman")


ggplot(df, aes(x = Metro.Distance..km., y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
  

df %>% identify_outliers(Metro.Distance..km.) %>% head()
dff <- df %>% identify_outliers(Metro.Distance..km.) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)

#price
df %>% identify_outliers(Price) %>% head()
dff <- df %>% identify_outliers(Price) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)

cor(df$Price,df$Metro.Distance..km. )


ggplot(df, aes(x = Metro.Distance..km., y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

#nonparametric 
#assumptions
#normal değil
#robust
#spearman için normal dağılması gerekmiyor















library("ggpubr")
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


getwd()
setwd("C:/Users/USER/Desktop")
data <- read.csv("Aemf1.csv")
data2 <-  as.data.frame(data)
getwd()
data <- read.csv("Aemf1.csv")


library("dplyr")
data_numeric <- select_if(data2,is.numeric)
getwd()
data <- read.csv("Aemf1.csv")
getwd()
getwd()
setwd("C:/Users/USER/Desktop/stat250projesi")
setwd("C:/Users/USER/Desktop/250proje")
data <- read.csv("Aemf1.csv")


model <- lm(Price ~ Metro.Distance..km., data = df)
model
summary(model)


model <- aov(Price ~ City, data = df)
print(summary(model))

#hnull :means are equal   
#halternative: not all means are equal 
#2e-16 0.00000000000000022

#%95 confidence 
#The ANOVA results indicate a significant relationship between the "City" variable and the "Price" variable.
#The p-value associated with the F-statistic is less than 2e-16, which is smaller than 0.05. 
#This suggests strong evidence against the null hypothesis, indicating that there is a statistically significant difference in the mean prices among the cities.
#In conclusion, based on the ANOVA results, we can reject the null hypothesis and conclude that there is a significant relationship between the city and price variables. 
#The mean prices differ significantly across the cities you have considered.
#En az birisinde önemli bir şekilde farklılık gösteriyor.
#Pairwise comparisonla faklı olanlar hangisi o söylenecek.


#anova_result <- oneway.test(Price ~ City, data = df)   bunu kullanmak için büyüklük yetersiz 
print(anova_result)

install.packages("car")
library(car)
#leveneTest(Price ~ City, data = df)

model <- lm(Price ~ City, data = df)
#shapiro.test(df$Price)

ks.test(df$Price, 'pnorm') # normality check küçük diye
kruskal.test(Price ~ City, data = df) #bu asıl check anovanın non parametric

boxplot(Price ~ City, data = df, xlab = "City", ylab = "Price")

#BARİNEDEN GELEN KISIM
library(dplyr)
library(rstatix)
library(RColorBrewer)
library(viridis)
library(colorspace)
library(ggplot2)
install.packages("rstatix")
df <- read.csv("Aemf1.csv")

df$Multiple.Rooms <- ifelse((df$Multiple.Rooms+df$Business)==1,1,0)
df <- df[, -which(names(df) %in% c("Room.Type","Business","Shared.Room","Normalised.Attraction.Index","Normalised.Restraunt.Index"))]
#price
df %>% identify_outliers(Price) %>% head()
dff <- df %>% identify_outliers(Price) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#restaurant
df %>% identify_outliers(Restraunt.Index) %>% head()
dff <- df %>% identify_outliers(Restraunt.Index) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#attraction
df %>% identify_outliers(Attraction.Index) %>% head()
dff <- df %>% identify_outliers(Attraction.Index) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#center
df %>% identify_outliers(City.Center..km.) %>% head()
dff <- df %>% identify_outliers(City.Center..km.) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#metro
df %>% identify_outliers(Metro.Distance..km.) %>% head()
dff <- df %>% identify_outliers(Metro.Distance..km.) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)