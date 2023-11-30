# Importing necessary libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(forcats)
library(RColorBrewer)

# Specifying the relative path to your dataset within the project
dataset_path <- "cleaned starting file/cleaned_autos.csv"

# Importing the dataset using read.csv with a.strings parameter to indicate the values that should be treated as missing (blank)
df <- read.csv(dataset_path, na.strings = c("", "NA"))

# Viewing the contents of a dataframe
View(df)

# Assuming there are commas in the numeric values while converting columns to numeric
df$odometer_in_km <- as.numeric(gsub(",", "", df$odometer_in_km))

# Creating a scatter plot
ggplot(df, aes(x = odometer_in_km, y = price_in_EUR)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Scatter Plot: Relationship between Kilometers Traveled and Car Price", x = "Kilometers Traveled", y = "Car Price")

# Creating a scatter plot
ggplot(df, aes(x = powerPS, y = price_in_EUR)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "Scatter Plot: Horsepower vs. Car Price", x = "Horsepower (powerPS)", y = "Car Price (EUR)")


# Calculate percentages using dplyr and forcats
df_percent <- df %>%
  group_by(gearbox) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Creating a pie chart with percentages
ggplot(df_percent, aes(x = "", y = percentage, fill = gearbox)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Detailed Pie Chart: Distribution of Gearbox Types with Percentages") +
  theme_void()

# Creating a bar chart with different colors for the distribution of vehicle types
ggplot(df, aes(x = fct_infreq(vehicleType), fill = vehicleType)) +
  geom_bar() +
  labs(title = "Bar Chart: Distribution of Vehicle Types", x = "Vehicle Type", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1))

# Creating a grouped bar chart for the relationship between vehicleType and gearbox
ggplot(df, aes(x = vehicleType, fill = gearbox)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Grouped Bar Chart: Relationship between Vehicle Type and Gearbox", x = "Vehicle Type", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1))

# Creating a grouped bar chart for the relationship between powerPS and vehicleType
ggplot(df, aes(x = vehicleType, y = powerPS, fill = vehicleType)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +  # Set the color palette
  labs(title = "Grouped Bar Chart: Relationship between Horsepower and Vehicle Type", x = "Vehicle Type", y = "Average Horsepower") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1))

# Calculate average price for each unique vehicle type
avg_price_by_vehicle_type <- df %>%
  group_by(vehicleType) %>%
  summarize(avg_price = mean(price_in_EUR, na.rm = TRUE))

# Plotting the average price for each unique vehicle type
ggplot(avg_price_by_vehicle_type, aes(x = vehicleType, y = avg_price, fill = vehicleType)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Price for Different Vehicle Types",
       x = "Vehicle Type",
       y = "Average Price in EUR") +
  theme_minimal() +
  scale_fill_manual(values = rainbow(nrow(avg_price_by_vehicle_type)))