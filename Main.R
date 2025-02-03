# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Step 1: Data Import and Cleaning
listings <- read_csv("/Users/yashsingh/Desktop/listings.csv")
View(listings)
dim(listings)

# Check for missing values
missing_values <- colSums(is.na(listings))
missing_values
colnames(listings)
# Clean data: missing values, remove rows with missing price,
# and fill missing reviews_per_month
listings_cleaned <- listings %>%
  select(-neighbourhood_group, -license,-id) %>%
  filter(!is.na(price)) %>%
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), 0, reviews_per_month))

View(listings_cleaned)
class(listings_cleaned$last_review)
# last_review remove na values
listings_cleaned <- listings_cleaned %>% filter(!is.na(last_review))
colSums(is.na(listings_cleaned))

# Step 2: Exploratory Data Analysis (EDA)
# Plot histogram of price distribution
  ggplot(listings_cleaned, aes(x = price)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(x = "Price", y = "Frequency", title = "Histogram of Listing Prices")+
    scale_x_continuous(breaks = seq(0, 1500, by = 500))

# Scatter plot of reviews vs. availability
ggplot(listings_cleaned, aes(x = number_of_reviews, y = availability_365)) +
  geom_point(alpha = 0.7, color = "blue") +
  labs(x = "Number of Reviews", y = "Availability (in days)", title = "Impact of Reviews on Availability")

# Bar plot of room types
ggplot(listings_cleaned, aes(x = room_type, fill = room_type)) +
  geom_bar() +
  labs(x = "Room Type", y = "Count", fill = "Room Type", title = "Distribution of Room Types")

# Barplot of Sum of prices by neighborhood

neighbourhood_summary <- listings_cleaned %>%
  group_by(neighbourhood) %>%
  summarize(total_price = mean(price))

# Plot total price by neighborhood
ggplot(neighbourhood_summary, aes(x = neighbourhood, y = total_price)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(x = "Neighborhood", y = "Total Price", title = "Total Price of Listings by Neighborhood") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 3: Feature Engineering
listings_cleaned <- listings_cleaned %>%
  mutate(days_since_last_review = as.numeric(Sys.Date() - last_review))



# Step 4: Linear Regression Model
listings_cleaned <- listings_cleaned %>%
  select(price,number_of_reviews,reviews_per_month,availability_365,days_since_last_review)

#first normalize the data
normalize <- function(x){
  a<-(x-min(x))/(max(x)-min(x))
  return(a)
}
listings_cleaned<- listings_cleaned %>% mutate_all(normalize)

# Split data into training and testing sets
set.seed(1234)  # Set seed for reproducibility
train_index <- sample(nrow(listings_cleaned), 0.7 * nrow(listings_cleaned))
train_data <- listings_cleaned[train_index, ]
test_data <- listings_cleaned[-train_index, ]

# Build linear regression model
model_lm <- lm(price ~ days_since_last_review, data = train_data)
#price is the dependant variable
#change in independant variable will result in change in dependant variable
#how other features affect the price 
sum(is.na(model_lm))

# Step 5: Model Evaluation
# Predict using the linear regression model
predictions_lm <- predict(model_lm, newdata = test_data)

summary(predictions_lm)

  # Count how many predictions are correct
correct_predictions <- sum(abs(predictions_lm - test_data$price))

# Calculate total valid predictions and accuracy
total_predictions <- length(predictions_lm)
accuracy <- correct_predictions / total_predictions
cat("Accuracy of Predictions:", accuracy * 100, "%\n")

#root mean squared error evaluation for regression model
rmse <- sqrt(mean((test_data$price - predictions_lm)^2))
print(paste("RMSE:", round(rmse, 2)))

ggplot(listings_cleaned,aes(x=price,y=days_since_last_review))+
  geom_point()+
  stat_smooth(method = "lm",col="red")


