# Importing necessary libraries
library(tidyverse)

# Specify the relative path to your dataset within the project
dataset_path <- "starting file/autos.csv"

# Import the dataset using read.csv with a.strings parameter to indicate the values that should be treated as missing (blank)
df <- read.csv(dataset_path, na.strings = c("", "NA"))

# View the contents of a dataframe
View(df)

# Inspect the structure of the data
glimpse(df)

# Identify missing values in the dataset
df %>% 
  summarise_all(~sum(is.na(.)))

# Count the number of unique values in the "seller" column
num_unique_sellers <- df %>%
  distinct(seller) %>%
  n_distinct()

# Showing the number of unique values in seller column
cat("Number of unique values in seller column:", num_unique_sellers, "\n")

# Summary of the unique values in the seller column along with their counts
seller_counts <- df %>%
  count(seller)

print(seller_counts)

# Changing values from German to English in seller column
df <- df %>%
  mutate(seller = ifelse(seller == "privat", "private", ifelse(seller == "gewerblich", "commercial", seller)))

# Showing unique values from seller column to check if they were changed successfully
unique_sellers <- unique(df$seller)
print(unique_sellers)

# mutate() is used to modify the "price" column in the dataframe.
# gsub("[\\$,]", "", price) is used to remove both "$" and "," symbols from the "price" column. 
# The result is then converted to a numeric format using as.numeric()
df <- df %>%
  mutate(price = as.numeric(gsub("[\\$,]", "", price)))

# Renaming price column to price_in_USD
df <- df %>%
  rename(price_in_USD = price)

# Summary of the unique values in the offerType column along with their counts
offerType_counts <- df %>%
  count(offerType)

print(offerType_counts)

# Changing values from German to English in offerType column
df <- df %>%
  mutate(offerType = ifelse(offerType == "Angebot", "Seling", ifelse(offerType == "Gesuch", "Buying", offerType)))

# summary of the unique values in the nrOfPictures column along with their counts
nrOfPictures_counts <- df %>%
  count(nrOfPictures)

print(nrOfPictures_counts)

# Dropping nrOfPictures column
df <- df %>%
  select(-nrOfPictures)

# Replacing _ sign with a space to improve readability in name column
df <- df %>%
  mutate(name = str_replace_all(name, "_", " "))

# Renaming name column to short_car_description
df <- df %>%
  rename(short_car_description = name)

# Exchange rate 25.11.2023.
exchange_rate <- 0.91

# Converting the 'price' column from dollars to euros and overwrite the original column
df$price_in_USD <- df$price_in_USD * exchange_rate

# Renaming price_in_USD column to price_in_EUR
df <- df %>%
  rename(price_in_EUR = price_in_USD)

# Summary of the unique values in the vehicleType column along with their counts
vehicleType_counts <- df %>%
  count(vehicleType)

print(vehicleType_counts)

# Changing values from German to English in vehicleType column

df <- df %>%
  mutate(
    vehicleType = case_when(
      vehicleType == "andere" ~ "other",
      vehicleType == "kleinwagen" ~ "normal_car",
      vehicleType == "kombi" ~ "van",
      TRUE ~ vehicleType  # Keep other values unchanged
    )
  )


# Removing rows where vehicleType is NA
df <- na.omit(df, cols = "vehicleType")

# Using mutate to change "andere" to "other" in the model column
df <- df %>%
  mutate(model = ifelse(model == "andere", "other", model))

# Summary of the unique values in the gearbox column along with their counts
gearbox_counts <- df %>%
  count(gearbox)

print(gearbox_counts)

# Changing values from German to English in gearbox column
df <- df %>%
  mutate(gearbox = ifelse(gearbox == "automatik", "automatic", ifelse(gearbox == "manuell", "manual", gearbox)))

# Using mutate and gsub to remove "km" from the odometer column values
df <- df %>%
  mutate(odometer = gsub("km", "", odometer))

# Renaming odometer column to odometer_in_km
df <- df %>%
  rename(odometer_in_km = odometer)

# Summary of the unique values in the notRepairedDamage column along with their counts
notRepairedDamage_counts <- df %>%
  count(notRepairedDamage)

print(notRepairedDamage_counts)

# Changing values from German to English in gearbox column
df <- df %>%
  mutate(notRepairedDamage = ifelse(notRepairedDamage == "nein", "no", ifelse(notRepairedDamage == "ja", "yes", notRepairedDamage)))

View(df)

# Summary of the unique values in the fuelType column along with their counts
fuelType_counts <- df %>%
  count(fuelType)

print(fuelType_counts)

# Changing values from German to English in fuelType column

df <- df %>%
  mutate(
    fuelType = case_when(
      fuelType == "andere" ~ "other",
      fuelType == "benzin" ~ "petrol",
      fuelType == "cng" ~ "compressed_natural_gas",
      fuelType == "elektro" ~ "electric",
      fuelType == "lpg" ~ "liqefied_petroleum_gas",
      TRUE ~ fuelType  # Keep other values unchanged
    )
  )