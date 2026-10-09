# Week 1 Internship Task
# Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic

# Load the dataset
titanic <- read.csv("test.csv")

# Display the first 6 rows
head(titanic)
# Check the structure of the dataset
str(titanic)

# Check the dimensions of the dataset
dim(titanic)

# Generate summary statistics
summary(titanic)
# Check missing values in each column
colSums(is.na(titanic))
# Calculate percentage of missing values
missing_percentage <- colSums(is.na(titanic)) / nrow(titanic) * 100

missing_percentage
# Check blank values in Cabin
sum(titanic$Cabin == "")
# Convert blank Cabin values to NA
titanic$Cabin[titanic$Cabin == ""] <- NA
# Recheck missing values
colSums(is.na(titanic))