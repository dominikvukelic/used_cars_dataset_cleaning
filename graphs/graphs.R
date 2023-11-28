# Importing necessary libraries
library(ggplot2)

# Specifying the relative path to your dataset within the project
dataset_path <- "cleaned starting file/cleaned_autos.csv"

# Importing the dataset using read.csv with a.strings parameter to indicate the values that should be treated as missing (blank)
df <- read.csv(dataset_path)

# Viewing the contents of a dataframe
View(df)