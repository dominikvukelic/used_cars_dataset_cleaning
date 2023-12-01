# Importing necessary libraries
library(ggplot2)
library(tidyverse)  # Includes dplyr and forcats
library(RColorBrewer)
library(ggrepel)
library(viridis)

# Specifying the relative path to your dataset within the project
dataset_path <- "cleaned starting file/cleaned_autos.csv"

# Importing the dataset using read.csv with na.strings parameter to indicate missing values
df <- read.csv(dataset_path, na.strings = c("", "NA"))

# Viewing the contents of a dataframe
View(df)

# Calculate percentages of gearbox types using dplyr and forcats
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
  labs(title = "Distribution of Gearbox Types with Percentages",
       fill = "Gearbox Type") +
  theme_void()

# Creating a bar chart with different colors for the distribution of vehicle types
ggplot(df, aes(x = fct_infreq(vehicleType), fill = vehicleType)) +
  geom_bar() +
  labs(title = "Distribution of Vehicle Types",
       x = "Vehicle Type",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1))

# Creating a grouped bar chart for the relationship between vehicleType and gearbox
ggplot(df, aes(x = vehicleType, fill = gearbox)) +
  geom_bar(position = "dodge", stat = "count", color = "black") +
  labs(title = "Relationship between Vehicle Type and Gearbox",
       x = "Vehicle Type",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1))

# Creating a grouped bar chart for the relationship between powerPS and vehicleType
ggplot(df, aes(x = vehicleType, y = powerPS, fill = vehicleType)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2") +  # Set the color palette
  labs(title = "Relationship between Horsepower and Vehicle Type",
       x = "Vehicle Type",
       y = "Average Horsepower") +
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

# Calculate percentages using dplyr and forcats for fuel types
df_percent <- df %>%
  group_by(fuelType) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Creating a pie chart with percentages and preventing text overlap
ggplot(df_percent, aes(x = "", y = percentage, fill = fuelType)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text_repel(aes(label = sprintf("%s\n%.1f%%", fuelType, percentage)),
                  position = position_stack(vjust = 0.5),
                  color = "black", size = 3) +
  labs(title = "Distribution of Fuel Types with Percentages",
       fill = "Fuel Type") +
  theme_void()

# Count the occurrences of each brand
brand_counts <- table(df$brand)

# Convert to data frame
brand_df <- data.frame(brand = names(brand_counts), count = as.numeric(brand_counts))

# Sort by count in descending order and take the top N brands
top_brands <- head(brand_df[order(-brand_df$count), ], 10)  # Adjust '10' as needed

# Create a horizontal bar chart for the top N brands with a color palette
ggplot(top_brands, aes(x = reorder(brand, -count), y = count, fill = brand)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis(discrete = TRUE) +  # Using viridis color palette
  labs(title = "Top Car Brands",
       x = "Car Brands",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

