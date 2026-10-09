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
missing_percentage <- colSums(is.na(titanic)) / nrow(titanic) * 100

missing_percentage
titanic$Cabin[titanic$Cabin == ""] <- NA

colSums(is.na(titanic))
# Find the median age
median_age <- median(titanic$Age, na.rm = TRUE)
median_age
# Replace missing Age values with the median
titanic$Age[is.na(titanic$Age)] <- median_age

# Check Age missing values
sum(is.na(titanic$Age))
# Find the median fare
median_fare <- median(titanic$Fare, na.rm = TRUE)
median_fare
# Replace missing Fare with the median
titanic$Fare[is.na(titanic$Fare)] <- median_fare

# Check Fare missing values
sum(is.na(titanic$Fare))
# Create a new variable for Cabin Deck
titanic$Cabin_Deck <- ifelse(
  is.na(titanic$Cabin),
  "Unknown",
  substr(titanic$Cabin, 1, 1)
)

# View the Cabin Deck categories
table(titanic$Cabin_Deck)
# Check the first few Cabin and Cabin_Deck values
head(titanic[, c("Cabin", "Cabin_Deck")])
# Final check of missing values
colSums(is.na(titanic))
# -----------------------------------------
# Outlier Detection
# -----------------------------------------

# Calculate IQR for Fare
Q1_Fare <- quantile(titanic$Fare, 0.25)
Q3_Fare <- quantile(titanic$Fare, 0.75)
IQR_Fare <- IQR(titanic$Fare)

# Define lower and upper bounds
lower_Fare <- Q1_Fare - 1.5 * IQR_Fare
upper_Fare <- Q3_Fare + 1.5 * IQR_Fare

# Identify Fare outliers
Fare_outliers <- titanic$Fare[
  titanic$Fare < lower_Fare | titanic$Fare > upper_Fare
]

# Display results
length(Fare_outliers)
Fare_outliers
length(Fare_outliers)
Fare_outliers
# Calculate IQR for Age
Q1_Age <- quantile(titanic$Age, 0.25)
Q3_Age <- quantile(titanic$Age, 0.75)
IQR_Age <- IQR(titanic$Age)

# Define lower and upper bounds
lower_Age <- Q1_Age - 1.5 * IQR_Age
upper_Age <- Q3_Age + 1.5 * IQR_Age

# Identify Age outliers
Age_outliers <- titanic$Age[
  titanic$Age < lower_Age | titanic$Age > upper_Age
]

# Display results
length(Age_outliers)
Age_outliers
# Calculate IQR for SibSp
Q1_SibSp <- quantile(titanic$SibSp, 0.25)
Q3_SibSp <- quantile(titanic$SibSp, 0.75)
IQR_SibSp <- IQR(titanic$SibSp)

# Define lower and upper bounds
lower_SibSp <- Q1_SibSp - 1.5 * IQR_SibSp
upper_SibSp <- Q3_SibSp + 1.5 * IQR_SibSp

# Identify SibSp outliers
SibSp_outliers <- titanic$SibSp[
  titanic$SibSp < lower_SibSp | titanic$SibSp > upper_SibSp
]

# Display results
length(SibSp_outliers)
SibSp_outliers
# Calculate IQR for Parch
Q1_Parch <- quantile(titanic$Parch, 0.25)
Q3_Parch <- quantile(titanic$Parch, 0.75)
IQR_Parch <- IQR(titanic$Parch)

# Define lower and upper bounds
lower_Parch <- Q1_Parch - 1.5 * IQR_Parch
upper_Parch <- Q3_Parch + 1.5 * IQR_Parch

# Identify Parch outliers
Parch_outliers <- titanic$Parch[
  titanic$Parch < lower_Parch | titanic$Parch > upper_Parch
]

# Display results
length(Parch_outliers)
Parch_outliers
# Boxplots for numerical variables

par(mfrow = c(2, 2))

boxplot(titanic$Age,
        main = "Age Boxplot",
        ylab = "Age")

boxplot(titanic$Fare,
        main = "Fare Boxplot",
        ylab = "Fare")

boxplot(titanic$SibSp,
        main = "SibSp Boxplot",
        ylab = "Number of Siblings/Spouses")

boxplot(titanic$Parch,
        main = "Parch Boxplot",
        ylab = "Number of Parents/Children")

par(mfrow = c(1, 1))
# -----------------------------------------
# Data Normalization
# -----------------------------------------

# Min-Max normalization for Age
titanic$Age_Normalized <- 
  (titanic$Age - min(titanic$Age)) /
  (max(titanic$Age) - min(titanic$Age))

# Min-Max normalization for Fare
titanic$Fare_Normalized <- 
  (titanic$Fare - min(titanic$Fare)) /
  (max(titanic$Fare) - min(titanic$Fare))

# Check the normalized values
head(titanic[, c("Age", "Age_Normalized",
                 "Fare", "Fare_Normalized")])
range(titanic$Age_Normalized)
range(titanic$Fare_Normalized)
# -----------------------------------------
# Categorical Encoding
# -----------------------------------------

# Encode Sex
titanic$Sex_Encoded <- ifelse(titanic$Sex == "male", 1, 0)

# Check the encoding
table(titanic$Sex, titanic$Sex_Encoded)
# Create dummy variables for Embarked
Embarked_encoded <- model.matrix(~ Embarked - 1, data = titanic)

# Add encoded variables to dataset
titanic <- cbind(titanic, Embarked_encoded)

# View the result
head(titanic[, c("Embarked", "EmbarkedC", "EmbarkedQ", "EmbarkedS")])
str(titanic)
# -----------------------------------------
# Exploratory Data Analysis (EDA)
# -----------------------------------------

# Structure of the cleaned dataset
str(titanic)

# Summary statistics
summary(titanic)
# Descriptive statistics for Age
mean(titanic$Age)
median(titanic$Age)
sd(titanic$Age)
min(titanic$Age)
max(titanic$Age)
# Descriptive statistics for Fare
mean(titanic$Fare)
median(titanic$Fare)
sd(titanic$Fare)
min(titanic$Fare)
max(titanic$Fare)
# -----------------------------------------
# EDA Visualizations
# -----------------------------------------

# Histogram of Age
hist(titanic$Age,
     main = "Distribution of Passenger Age",
     xlab = "Age",
     ylab = "Number of Passengers",
     breaks = 15)
# Histogram of Fare
hist(titanic$Fare,
     main = "Distribution of Passenger Fare",
     xlab = "Fare",
     ylab = "Number of Passengers",
     breaks = 20)
# Bar chart of passenger sex
barplot(table(titanic$Sex),
        main = "Passenger Distribution by Sex",
        xlab = "Sex",
        ylab = "Number of Passengers")
# Bar chart of passenger class
barplot(table(titanic$Pclass),
        main = "Passenger Distribution by Class",
        xlab = "Passenger Class",
        ylab = "Number of Passengers")
# Bar chart of embarkation port
barplot(table(titanic$Embarked),
        main = "Passenger Distribution by Embarkation Port",
        xlab = "Port of Embarkation",
        ylab = "Number of Passengers")
# -----------------------------------------
# Correlation Analysis
# -----------------------------------------

# Select numerical variables for correlation
numeric_data <- titanic[, c("Pclass", "Age", "SibSp", "Parch", "Fare")]

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data)

# Display correlation matrix
round(correlation_matrix, 2)
# Visualize the correlation matrix
heatmap(correlation_matrix,
        main = "Correlation Matrix of Numerical Variables",
        symm = TRUE)
# -----------------------------------------
# Initial Insights
# -----------------------------------------

# Passenger class distribution
table(titanic$Pclass)

# Sex distribution
table(titanic$Sex)

# Embarkation distribution
table(titanic$Embarked)

# Average age by passenger class
aggregate(Age ~ Pclass, data = titanic, mean)

# Average fare by passenger class
aggregate(Fare ~ Pclass, data = titanic, mean)

# Average fare by sex
aggregate(Fare ~ Sex, data = titanic, mean)

# Average age by sex
aggregate(Age ~ Sex, data = titanic, mean)
table(titanic$Pclass)
table(titanic$Sex)
table(titanic$Embarked)

aggregate(Age ~ Pclass, data = titanic, mean)
aggregate(Fare ~ Pclass, data = titanic, mean)
aggregate(Fare ~ Sex, data = titanic, mean)
aggregate(Age ~ Sex, data = titanic, mean)
# -----------------------------------------
# Final Dataset Verification
# -----------------------------------------

dim(titanic)

str(titanic)

colSums(is.na(titanic))