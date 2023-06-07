setwd("E:\\R Assignment")

# Load the required libraries
library(tidyverse)

# Read the dataset
data <- read.csv("book1.csv")

# Convert the Date column to a proper date format
data$Date <- as.Date(data$Date)

# Convert Latitude and Longitude columns to numeric
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)

# Remove rows with missing Latitude or Longitude values
data <- na.omit(data)

# Split the data into training and test sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Train the multiple linear regression model
model <- lm(cbind(Latitude, Longitude) ~ Date, data = train_data)

# Predict on the test data
predicted <- predict(model, newdata = test_data)

# Compute evaluation metrics
mse <- mean((predicted - test_data[, c("Latitude", "Longitude")])^2, na.rm = TRUE)
mae <- mean(abs(predicted - test_data[, c("Latitude", "Longitude")]), na.rm = TRUE)
rmse <- sqrt(mse)
r2 <- summary(model)$r.squared



# Function to predict new points using the trained model
predictNewPoint <- function(new_date, model) {
  new_date <- as.Date(new_date)
  
  new_entry <- data.frame(Date = new_date)
  new_prediction <- predict(model, newdata = new_entry)
  
  new_latitude <- new_prediction[1]
  new_longitude <- new_prediction[2]
  
  return(c(Latitude = new_latitude, Longitude = new_longitude))
}

# Example usage of predictNewPoint function
new_date <- "2028-01-21"
new_point <- predictNewPoint(new_date, model)
print(new_point)