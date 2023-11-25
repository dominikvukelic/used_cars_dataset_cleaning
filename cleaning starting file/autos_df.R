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

# Changing values from German to English

df <- df %>%
  mutate(seller = ifelse(seller == "privat", "private", ifelse(seller == "gewerblich", "commercial", seller)))

# Showing unique values from seller column to check if they were changed successfully

unique_sellers <- unique(df$seller)
print(unique_sellers)

