# Importing necessary libraries
library(ggplot2)
library(tidyverse)

# Specifying the relative path to your dataset within the project
dataset_path <- "cleaned starting file/cleaned_autos.csv"

# Importing the dataset using read.csv with a.strings parameter to indicate the values that should be treated as missing (blank)
df <- read.csv(dataset_path, na.strings = c("", "NA"))

# Viewing the contents of a dataframe
View(df)

# Assuming there are commas in the numeric values while converting columns to numeric
df$odometer_in_km <- as.numeric(gsub(",", "", df$odometer_in_km))

# Scatter plot
ggplot(df, aes(x = odometer_in_km, y = price_in_EUR)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Scatter Plot: Relationship between Kilometers Traveled and Car Price", x = "Kilometers Traveled", y = "Car Price")





