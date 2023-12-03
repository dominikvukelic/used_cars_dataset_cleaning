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

# Calculate percentages of gearbox types using dplyr
df_percent <- df %>%
  count(gearbox) %>%
  mutate(percentage = n / sum(n) * 100)

# Creating a pie chart with percentages for gearbox type
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
  geom_bar(color = "black", size = 0.5, position = "dodge", show.legend = FALSE) +
  geom_text(stat = "count", aes(label = stat(count)), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add numbers into the bars
  labs(title = "Distribution of Vehicle Types",
       x = "Vehicle Type",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1.1),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5))

# Creating a grouped bar chart for the relationship between vehicleType and gearbox
max_count <- max(table(df$vehicleType), na.rm = TRUE)

ggplot(df, aes(x = fct_infreq(vehicleType), fill = gearbox)) +
  geom_bar(position = "dodge", color = "black") +
  geom_text(aes(label = after_stat(count)),
            stat = "count",
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Relationship between Vehicle Type and Gearbox",
       x = "Vehicle Type",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, max_count))  # Set y-axis limits

# Creating a grouped bar chart for the relationship between powerPS and vehicleType
ggplot(df, aes(x = vehicleType, y = powerPS, fill = vehicleType)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", color = "black", show.legend = FALSE) +
  geom_text(stat = "summary", fun = "mean", aes(label = sprintf("%.1f", after_stat(y))),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Add numbers into the bars
  scale_fill_brewer(palette = "Set2") +  # Set the color palette
  labs(title = "Relationship between Horsepower and Vehicle Type",
       x = "Vehicle Type",
       y = "Average Horsepower") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1))

# Calculate the maximum average price for each vehicle type
max_avg_price <- avg_price_by_vehicle_type %>%
  group_by(vehicleType) %>%
  summarize(max_avg_price = max(avg_price, na.rm = TRUE)) %>%
  arrange(desc(max_avg_price)) %>%
  pull(vehicleType)

# Reorder the vehicleType factor based on max_avg_price
avg_price_by_vehicle_type$vehicleType <- factor(avg_price_by_vehicle_type$vehicleType, levels = max_avg_price)

# Plot the bar chart
ggplot(avg_price_by_vehicle_type, aes(x = vehicleType, y = avg_price, fill = vehicleType)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", show.legend = FALSE) +
  labs(title = "Average Price for Different Vehicle Types",
       x = "Vehicle Type",
       y = "Average Price in EUR") +
  theme_minimal() +
  scale_fill_manual(values = rainbow(nrow(avg_price_by_vehicle_type)))

# Count the occurrences of each brand
brand_counts <- table(df$brand)

# Convert to data frame
brand_df <- data.frame(brand = names(brand_counts), count = as.numeric(brand_counts))

# Arrange the data frame by count in descending order and take the top 10 brands
top_brands <- brand_df %>%
  arrange(desc(count)) %>%
  head(10)

# Create a horizontal bar chart for the top 10 brands with a color palette
ggplot(top_brands, aes(x = reorder(brand, -count), y = count, fill = brand)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) +  # Using viridis color palette
  labs(title = "Top 10 Car Brands",
       x = "Car Brands",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



# Box plot: Price Distribution by Fuel Type
ggplot(df, aes(x = fuelType, y = price_in_EUR)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Price Distribution by Fuel Type",
       x = "Fuel Type",
       y = "Price in EUR") +
  theme_minimal()

# Scatter Plot: Price vs. Mileage
ggplot(df, aes(x = odometer_in_km, y = price_in_EUR)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Price vs. Mileage",
       x = "Mileage (Odometer in km)",
       y = "Price in EUR") +
  theme_minimal()