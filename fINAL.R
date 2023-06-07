# Load the required libraries
library(shiny)
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

# UI
ui <- fluidPage(
  titlePanel("Latitude and Longitude Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("new_date", "Enter Date:", value = ""),
      actionButton("predict_button", "Predict")
    ),
    
    mainPanel(
      h4("Predicted Latitude and Longitude:"),
      tableOutput("prediction_output")
    )
  )
)

# Server
server <- function(input, output) {
  # Perform prediction when the button is clicked
  observeEvent(input$predict_button, {
    new_date <- input$new_date
    new_point <- predictNewPoint(new_date, model)
    output$prediction_output <- renderTable({
      data.frame(new_point)
    })
  })
}

# Function to predict new points using the trained model
predictNewPoint <- function(new_date, model) {
  new_date <- as.Date(new_date)
  
  new_entry <- data.frame(Date = new_date)
  new_prediction <- predict(model, newdata = new_entry)
  
  new_latitude <- formatC(new_prediction[1], format = "f", digits = 6)
  new_longitude <- formatC(new_prediction[2], format = "f", digits = 6)
  
  return(c(Latitude = new_latitude, Longitude = new_longitude))
}

# Run the Shiny app
shinyApp(ui, server)
