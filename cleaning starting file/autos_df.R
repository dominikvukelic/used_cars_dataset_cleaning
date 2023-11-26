library(tidyverse)

# Specify the relative path to your dataset within the project
dataset_path <- "starting file/autos.csv"

# Import the dataset using read.csv or appropriate function
df <- read.csv(dataset_path)

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

# summary of the unique values in the seller column along with their counts
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

# summary of the unique values in the offerType column along with their counts
offerType_counts <- df %>%
  count(offerType)

print(offerType_counts)

# Changing values from German to English in offerType column
df <- df %>%
  mutate(offerType = ifelse(offerType == "Angebot", "Offer", ifelse(offerType == "Gesuch", "Want", offerType)))

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

# Renaming name column to car_model
df <- df %>%
  rename(car_model = name)

# Exchange rate 25.11.2023.
exchange_rate <- 0.91

# Convert the 'price' column from dollars to euros and overwrite the original column
df$price_in_USD <- df$price_in_USD * exchange_rate

# Renaming price_in_USD column to price_in_EUR
df <- df %>%
  rename(price_in_EUR = price_in_USD)
