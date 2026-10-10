# =========================================
# Week 2 Internship Task
# Data Visualization and Insight Communication using R
# Dataset: Titanic
# =========================================

# Load the dataset
titanic <- read.csv("test.csv")

# Check the dataset
head(titanic)
# Check dataset structure
str(titanic)

# Check dimensions
dim(titanic)
packageVersion("ggplot2")
install.packages("ggplot2")
library(ggplot2)
# =========================================
# Week 2 Internship Task
# Data Visualization and Insight Communication using R
# Dataset: Titanic
# =========================================

# Load the dataset
titanic <- read.csv("test.csv")

# Check the dataset
head(titanic)

# Check dataset structure
str(titanic)

# Check dimensions
dim(titanic)

# Load visualization library
library(ggplot2)
library(ggplot2)
# =========================================
# 1. Age Distribution
# =========================================

ggplot(titanic, aes(x = Age)) +
  geom_histogram(
    bins = 20,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Passenger Age",
    x = "Age",
    y = "Number of Passengers"
  ) +
  theme_minimal()
# =========================================
# 2. Fare Distribution
# =========================================

ggplot(titanic, aes(x = Fare)) +
  geom_histogram(
    bins = 30,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Passenger Fare",
    x = "Fare",
    y = "Number of Passengers"
  ) +
  theme_minimal()
# =========================================
# 3. Passenger Class Distribution
# =========================================

ggplot(titanic, aes(x = factor(Pclass))) +
  geom_bar() +
  labs(
    title = "Passenger Distribution by Class",
    x = "Passenger Class",
    y = "Number of Passengers"
  ) +
  theme_minimal()
factor(Pclass)
factor(Pclass)
ggplot(titanic, aes(x = factor(Pclass))) +
  geom_bar() +
  labs(
    title = "Passenger Distribution by Class",
    x = "Passenger Class",
    y = "Number of Passengers"
  ) +
  theme_minimal()
# =========================================
# 4. Passenger Distribution by Sex
# =========================================

ggplot(titanic, aes(x = Sex)) +
  geom_bar() +
  labs(
    title = "Passenger Distribution by Sex",
    x = "Sex",
    y = "Number of Passengers"
  ) +
  theme_minimal()