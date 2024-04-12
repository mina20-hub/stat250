library(dplyr)
library(rstatix)
library(RColorBrewer)
library(viridis)
library(colorspace)
library(ggplot2)
library(corrplot)
df <- read.csv("Aemf1.csv")
df$Multiple.Rooms <- ifelse((df$Multiple.Rooms+df$Business)==1,1,0)
df <- df[, -which(names(df) %in% c("Private.Room","Room.Type","Business","Shared.Room","Person.Capacity",
                                    "Cleanliness.Rating", 
                                    "Guest.Satisfaction", "Bedrooms","Normalised.Attraction.Index","Normalised.Restraunt.Index"))]
#price
dff <- df %>% identify_outliers(Price) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#restaurant
dff <- df %>% identify_outliers(Restraunt.Index) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#attraction
dff <- df %>% identify_outliers(Attraction.Index) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#center
dff <- df %>% identify_outliers(City.Center..km.) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)
#metro
dff <- df %>% identify_outliers(Metro.Distance..km.) %>% filter(is.extreme == TRUE)
dff <- dff[-c(20,21)]
df <- anti_join(df, dff)





#3.1  average price of airbnb houses in Budapest less than 260 -> One sample mean
lisbon_prices <- df$Price[df$City == "Budapest"]
qqnorm(lisbon_prices, pch = 1, frame = FALSE)
qqline(lisbon_prices, col = "steelblue", lwd = 2)
ks.test(lisbon_prices, 'pnorm')
table(df$City)
library(ggpubr)
wilcox.test(lisbon_prices, mu = 203, alternative = "less")
summary(Price)

#3.2  Is there any significant difference in mean price of weekdays and weekends in Amsterdam? -> Two sample mean

#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Tourism_statistics_-_top_destinations
ks.test(df$Price, 'pnorm')

medians_by_city <- aggregate(Price ~ City + Day, data = df, FUN = median)
ggplot(medians_by_city, aes(x = City, y = Price, fill = Day)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cities", y = "Median Price", fill = "Day") +
  ggtitle("Median Prices by City and Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Subset the data for Barcelona
Barcelona_data <- df[df$City == "Barcelona", ]

# Create separate groups for weekends and weekdays
weekend_prices <- Barcelona_data[Barcelona_data$Day == "Weekend", "Price"]
weekday_prices <- Barcelona_data[Barcelona_data$Day == "Weekday", "Price"]
wilcox.test(weekend_prices, weekday_prices)

Paris_data <- df[df$City == "Paris", ]

# Create separate groups for weekends and weekdays
weekend_prices <- Paris_data[Paris_data$Day == "Weekend", "Price"]
weekday_prices <- Paris_data[Paris_data$Day == "Weekday", "Price"]
wilcox.test(weekend_prices, weekday_prices)

Amsterdam_data <- df[df$City == "Amsterdam", ]

# Create separate groups for weekends and weekdays
weekend_prices <- Amsterdam_data[Amsterdam_data$Day == "Weekend", "Price"]
weekday_prices <- Amsterdam_data[Amsterdam_data$Day == "Weekday", "Price"]
wilcox.test(weekend_prices, weekday_prices)
#we can conclude that there is a significant difference in the median prices between weekends and weekdays in Ams


df$Superhost


#3.3 3.3 Is the proportion of superhosts among listings with host who have
#multiple house significantly greater than 0.28?
df
sum(df$Superhost[df$Multiple.Rooms==1])
sum(df$Multiple.Rooms==1)
df$Superhost <- ifelse(df$Superhost=="True",1,0)
prop.test(sum(df$Superhost[df$Multiple.Rooms==1]),sum(df$Multiple.Rooms==1), .28, alternative = "greater")
prop.test(10,200, .28, alternative = "greater")


#3.4 lisbon super host proportion greater than paris  -> two sample
#https://www.wereldreizigers.nl/en/worldly/safest-most-dangerous-countries-europe/
#https://www.cntraveller.com/gallery/friendliest-countries-in-europe
#testing being safest also mean well behave to host
df$Superhost <- ifelse(df$Superhost=="True",1,0)
contingency_table <- table(df$Superhost, df$City)
chisq.test(contingency_table)

lisbon_data <- subset(df, City == "Lisbon")
paris_data <- subset(df, City == "Paris")


lisbon_superhost_count <- sum(lisbon_data$Superhost == 1)
paris_superhost_count <- sum(paris_data$Superhost == 1)

lisbon_total_listings <- nrow(lisbon_data)
paris_total_listings <- nrow(paris_data)

# Perform the two-sample proportion hypothesis test
prop.test(c(lisbon_superhost_count, paris_superhost_count),
          c(lisbon_total_listings, paris_total_listings),
          alternative = "greater",correct = FALSE)

#3.5.1  is there any rel btw price and metro distance -> simple corrolation 
#https://www.timeout.com/news/its-official-this-european-city-has-the-worlds-best-public-transport-040523
#because of they have best public transport
summary(df$City.Center..km.[df$City=="Vienna"])
heatmap(cor_matrix)
berlin_data <- subset(df, City == "Vienna")
# Perform simple linear regression
df$City.Center..km.
lm_model <- lm(Price ~  log(City.Center..km.) , data = berlin_data)
summary(lm_model)

cor(df$Price,df$Metro.Distance..km.,method = "spearman")
# Get the regression coefficients
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]

ggplot(berlin_data, aes(x = City.Center..km., y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Metro.Distance..km.", y = "Price", title = "Linear Regression: Price vs. Metro.Distance..km.") +
  annotate("text", x = max(df$Metro.Distance..km.), y = max(df$Price), label = paste("Price =", round(intercept, 2), "+", round(slope, 2), "Metro.Distance..km."), hjust = 1, vjust = -0.5)

#3.5.2
# Compute the correlation matrix
plot(Price ~ Attraction.Index+Restraunt.Index+ City.Center..km. + Metro.Distance..km., data = df)
cor_matrix <- cor(df[, c("Price", "Attraction.Index", "Restraunt.Index", "City.Center..km.", "Metro.Distance..km.")])

# Create the heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8,diag = FALSE, addCoef.col = "black")
#suprisingly attraction and restaurant index have more effect rather than city  and metro distance.


# Assuming you have already loaded the dataset into a variable named 'data'
# Fit the multiple linear regression model
model <- lm(Price ~ Attraction.Index+Restraunt.Index+ City.Center..km. + Metro.Distance..km., data = df)
summary(model)
# Print the summary of the regression model
library("olsrr")
ols_plot_resid_hist(model)
library(MASS)
library("robustbase")
summary(lmrob(Price ~ Attraction.Index + Restraunt.Index + City.Center..km. + Metro.Distance..km.,df))
robust_model <- rlm(Price ~ Attraction.Index + Restraunt.Index + City.Center..km. + Metro.Distance..km., data = df)
summary(robust_model)
#but which one is has  more effect
# Compute the correlation matrix
# Create a scatterplot matrix with regression lines
library(ggplot2)
library(gridExtra)

# Scatterplot 1
plot1 <- ggplot(df, aes(x = Attraction.Index, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Attraction.Index", y = "Price")

# Scatterplot 2
plot2 <- ggplot(df, aes(x = Restraunt.Index, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Restraunt.Index", y = "Price")

# Scatterplot 3
plot3 <- ggplot(df, aes(x = City.Center..km., y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(x = "City.Center..km.", y = "Price")

# Scatterplot 4
plot4 <- ggplot(df, aes(x = Metro.Distance..km., y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(x = "Metro.Distance..km.", y = "Price")

# Combine plots vertically
combined_plots <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2,nrow=2)

# Print the combined plots
print(combined_plots)


cor_matrix <- cor(df[, c("Price", "Attraction.Index", "Restraunt.Index", "City.Center..km.", "Metro.Distance..km.")])

# Create the heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8,diag = FALSE, addCoef.col = "black")
#suprisingly attraction and restaurant index have more effect rather than city  and metro distance.

#3.6.2 which pairs of city prices are significantly different from each other. 

one.way <- aov(Price ~ City, data = df)
summary(one.way)

pairwise.t.test(df$Price, df$City, p.adj = "none")


#3.1   
#3.2   
#3.3   
#3.4   
#3.5.1 
#3.5.2 
#3.6.1 
#3.6.2 

X_X <- matrix(c(5, 4, 3, 12, 4, 36, 8, 12, 3, 8, 11, 11, 12, 12, 11, 34), nrow = 4, ncol = 4)
inverse_X_X <- matrix(c(1.5,-0.01,0.2,-0.6,-0.01,0.03,-0.02,-0.001,0.2,-0.02,0.2,-0.1,-0.6,-0.001,-0.1,0.3),nrow = 4,ncol = 4)
X_Y <- matrix(c(21,12,2,45))
mse <- 2.2
inverse_X_X  %*%X_Y
inverse_X_X
