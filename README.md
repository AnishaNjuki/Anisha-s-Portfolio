# Anisha-s-Portfolio
Exploratory Data analysis of unknown variables

---
author: "Anisha Njuki"
date: "2024-03-30"
output: html_document
---

```{r setup, include=FALSE}
if (!("knitr" %in% installed.packages())) {
  install.packages('knitr', repos='http://cran.rstudio.org')}
library(knitr)

if (!("formatR" %in% installed.packages())) {
  install.packages("formatR")}
library(formatR)

knitr::opts_chunk$set(echo = TRUE)

```
```{r working directory}
setwd("~/MAST5954")

```
```{r packages and libraries}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")}
library(tidyverse)

if (!("dplyr" %in% installed.packages())) {
  install.packages("dplyr")}
library(dplyr)

if (!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2")}
library(ggplot2)

if (!("DescTools" %in% installed.packages())) {
  install.packages("DescTools")}
library(DescTools)

if (!("reshape2" %in% installed.packages())) {
  install.packages("reshape2")}
library(reshape2)

if (!("tidyr" %in% installed.packages())) {
  install.packages("tidyr")}
library(tidyr)

if (!("cluster" %in% installed.packages())) {
  install.packages("cluster")}
library(cluster)
  
if (!("gridExtra" %in% installed.packages())) {
  install.packages("gridExtra")}
library(gridExtra)
  
if (!("rpart" %in% installed.packages())) {
  install.packages("rpart")}
library(rpart)

if (!("rpart.plot" %in% installed.packages())) {
install.packages("rpart.plot")}
library(rpart.plot)

if (!("corrplot" %in% installed.packages())) {
install.packages("corrplot")}
library(corrplot)

if (!("GGally" %in% installed.packages())) {
install.packages("GGally")}
library(GGally)

if (!("stats" %in% installed.packages())) {
install.packages("stats")}

if (!("factoextra" %in% installed.packages())) {
install.packages("factoextra")}

```

```{r data checking}
data <- read.csv("AsstDat2024cut.csv")


missing_values <- data %>%
  summarise_all(~sum(is.na(.)))
numerical_columns <- sapply(data, is.numeric)
# Impute missing values for numerical variables with mean
data[, numerical_columns] <- lapply(data[, numerical_columns], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

data$C7 <- ifelse(data$C7 != 0 & data$C7 != 1, 1, data$C7)
data$C7 <- ifelse(data$C7 == 0 | data$C7 == 1, data$C7, 0)

summary(data)


Y <- data$Y
C1<- data$C1
C2<-data$C2
C3<-data$C3
C4<-data$C4
C5<-data$C5
C6<-data$C6
C7<-data$C7

```


```{r General EDA}
# general plots to present the distribution of each variable in the data set.

ggplot(data, aes(x = Y)) +
  geom_histogram(fill = 'blue', binwidth = 5) +
  labs(title = "Distribution of y") 
  
ggplot(data, aes(x = C1)) +
  geom_histogram(fill = 'blue', binwidth = 5) +
  labs(title = "Distribution of C1") 

ggplot(data, aes(x = C2)) +
  geom_histogram(fill = 'blue', binwidth = 5) +
  labs(title = "Distribution of C2") 

ggplot(data, aes(x = C3)) +
  geom_histogram(fill = 'blue', binwidth = 5) +
  labs(title = "Distribution of C3") 

ggplot(data, aes(x = C4)) +
  geom_histogram(fill = 'blue', binwidth = 5) +
  labs(title = "Distribution of C4") 

ggplot(data, aes(x = C5)) +
  geom_histogram(fill = 'blue', binwidth = 5) +
  labs(title = "Distribution of C5") 

ggplot(data, aes(x = C6)) +
  geom_histogram(fill = 'blue', binwidth = 5) +
  labs(title = "Distribution of C6") 

# Ensure missing value entries do not affect the binary variable

data$C7 <- ifelse(data$C7 != 0 & data$C7 != 1, 1, data$C7)
data$C7 <- ifelse(data$C7 == 0 | data$C7 == 1, data$C7, 0)

binary_counts <- table(data$C7)

# Create bar chart
barplot(binary_counts, 
        names.arg = names(binary_counts), 
        xlab = "C7", 
        ylab = "Frequency", 
        main = "Distribution of C7 Variable")

# Create box plot

selected_data <- c("C1","C2","C3","C4","C5","C6")
  
subset_data <- data[, c("C1","C2","C3","C4","C5","C6")]

boxplot(subset_data, main = "Boxplot") 

```
```{r EDA}
# comparing variables

#Create scatter plots to compare each of the variables to the response variable

ggplot(data, aes(x = C1, y = Y, )) +
  geom_point() +
  geom_smooth(aes(x = C1, y = Y), method = "lm") +  
  labs(x = "C1", y = "Y", title = "Scatter Plot with Smooth Line")

ggplot(data, aes(x = C2, y = Y, )) +
  geom_point() +
  geom_smooth(aes(x = C2, y = Y), method = "lm") +  
  labs(x = "C2", y = "Y", title = "Scatter Plot with Smooth Line")

ggplot(data, aes(x = C3, y = Y, )) +
  geom_point() +
  geom_smooth(aes(x = C3, y = Y), method = "lm") +  
  labs(x = "C3", y = "Y", title = "Scatter Plot with Smooth Line")

ggplot(data, aes(x = C4, y = Y, )) +
  geom_point() +
  geom_smooth(aes(x = C4, y = Y), method = "lm") +  
  labs(x = "C4", y = "Y", title = "Scatter Plot with Smooth Line")

ggplot(data, aes(x = C5, y = Y, )) +
  geom_point() +
  geom_smooth(aes(x = C5, y = Y), method = "lm") +  
  labs(x = "C5", y = "Y", title = "Scatter Plot with Smooth Line")

ggplot(data, aes(x = C6, y = Y, )) +
  geom_point() +
  geom_smooth(aes(x = C6, y = Y), method = "lm") +  
  labs(x = "C6", y = "Y", title = "Scatter Plot with Smooth Line")

# Calculate averages for all the variables

mean(Y)
mean(C1)
mean(C2)
mean(C3)
mean(C4)
mean(C5)
mean(C6)

# Analysis comparison to binary C7

data$C7_group <- factor(ifelse(data$C7 == 0, "0", "1"))

ggplot(data, aes(x = C1, color = C7_group)) +
  geom_freqpoly(binwidth = 10) +
  labs(title = "Frequency Polygon of C1 by C7 binary",
       x = "C1",
       y = "Frequency")

ggplot(data, aes(x = C2, color = C7_group)) +
  geom_freqpoly(binwidth = 10) +
  labs(title = "Frequency Polygon of C2 by C7 binary",
       x = "C2",
       y = "Frequency")

ggplot(data, aes(x = C3, color = C7_group)) +
  geom_freqpoly(binwidth = 10) +
  labs(title = "Frequency Polygon of C3 by C7 binary",
       x = "C3",
       y = "Frequency")

ggplot(data, aes(x = C4, color = C7_group)) +
  geom_freqpoly(binwidth = 10) +
  labs(title = "Frequency Polygon of C4 by C7 binary",
       x = "C4",
       y = "Frequency")

ggplot(data, aes(x = C5, color = C7_group)) +
  geom_freqpoly(binwidth = 10) +
  labs(title = "Frequency Polygon of C5 by C7 binary",
       x = "C5",
       y = "Frequency")

ggplot(data, aes(x = C6, color = C7_group)) +
  geom_freqpoly(binwidth = 10) +
  labs(title = "Frequency Polygon of C6 by C7 binary",
       x = "C6",
       y = "Frequency")

ggplot(data, aes(x = Y, color = C7_group)) +
  geom_freqpoly(binwidth = 30) +
  labs(title = "Frequency Polygon of Y by C7 binary",
       x = "Y",
       y = "Frequency")


# Correlation analysis

num_vars_subset <- c("C1","C2","C3","C4","C5","C6")
pairs(data[, num_vars_subset])

num_vars <- c("C1","C2","C3","C4","C5","C6")
scatter_matrix <- ggpairs(data, columns = num_vars)
print(scatter_matrix)

# Select the relevant variables
selected_vars <- data[, c("Y","C1","C2","C3","C4","C5","C6")]

# Calculate the correlation matrix
correlation_matrix <- cor(selected_vars, method = "pearson")


ggplot(data = reshape2::melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "purple", mid = "white", high = "pink", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", 
       x = "Variables",
       y = "Variables")

```
# Table of average value for each variable
Variable      | Average
------------- | -------------
Y             | 854.4126
C1            | 32.99384
C2            | 103.4392
C3            | 115.8035
C4            | 22.80039
C5            | -0.502065
C6            | 11.35267

```{r clustering}

# Load additional libraries
library(cluster)
library(factoextra)
library(stats)

 # Clustering for variable C1 to check

# Select relevant variables for clustering (adjust as needed)
clustering_data <- data[, c("C1", "Y")]

# Standardize the data
scaled_data <- scale(clustering_data)

# Determine the optimal number of clusters using, for example, the elbow method
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 10)
  wss[i] <- sum(kmeans_model$tot.withinss)
}
plot(1:10, wss, type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Based on the elbow method, choose the optimal number of clusters
# Replace 'optimal_clusters' with the selected number
optimal_clusters <- 8

# Perform k-means clustering
kmeans_model <- kmeans(scaled_data, centers = optimal_clusters, nstart = 10)

# Visualize the clustering results
fviz_cluster(kmeans_model, data = scaled_data, geom = "point")

# Perform k-means clustering with the optimal number of clusters
final <- kmeans(scaled_data, optimal_clusters, nstart = 25)

# Visualize the clustering results
fviz_cluster(final, data = scaled_data)

# Add clustering information to the original data
data_with_clusters <- data %>% 
  mutate(Cluster = final$cluster)

# Select relevant variables for summarisation
variables_of_interest <- c("C1", "Y")

# Summarize selected variables by cluster
summary_by_cluster <- data_with_clusters %>% 
  select(Cluster, all_of(variables_of_interest)) %>% 
  group_by(Cluster) %>% 
  summarise_all("mean")

# Print summary
print(summary_by_cluster)
 


# Clustering for all variables

# Select relevant variables for clustering (adjust as needed)
clustering_data2 <- data[, c("Y","C1","C2","C3","C4","C5","C6")]

# Standardize the data
scaled_data <- scale(clustering_data2)

# Determine the optimal number of clusters using, for example, the elbow method
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 10)
  wss[i] <- sum(kmeans_model$tot.withinss)
}
plot(1:10, wss, type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Based on the elbow method, choose the optimal number of clusters
# Replace 'optimal_clusters' with the selected number
optimal_clusters <- 8

# Perform k-means clustering
kmeans_model <- kmeans(scaled_data, centers = optimal_clusters, nstart = 10)

# Visualize the clustering results
fviz_cluster(kmeans_model, data = scaled_data, geom = "point")

# Perform k-means clustering with the optimal number of clusters
final <- kmeans(scaled_data, optimal_clusters, nstart = 25)

# Visualize the clustering results
fviz_cluster(final, data = scaled_data)

# Add clustering information to the original data
data_with_clusters <- data %>% 
  mutate(Cluster = final$cluster)

# Select relevant variables for summarisation
variables_of_interest <- c("Y","C1","C2","C3","C4","C5","C6")

# Summarize selected variables by cluster
summary_by_cluster <- data_with_clusters %>% 
  select(Cluster, all_of(variables_of_interest)) %>% 
  group_by(Cluster) %>% 
  summarise_all("mean")

# Print summary
print(summary_by_cluster)

# Clustering 2, refined after first clustering

# Select relevant variables for clustering (adjust as needed)
clustering_data3 <- data[, c("Y","C1","C2","C3","C4","C5","C6")]

# Standardize the data
scaled_data <- scale(clustering_data3)

# Determine the optimal number of clusters using, for example, the elbow method
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 10)
  wss[i] <- sum(kmeans_model$tot.withinss)
}
plot(1:10, wss, type = "b", main = "Elbow Method", xlab = "Number of Clusters", ylab = "Within Sum of Squares")

# Based on the elbow method, choose the optimal number of clusters
# Replace 'optimal_clusters' with the selected number
optimal_clusters <- 6

# Perform k-means clustering
kmeans_model <- kmeans(scaled_data, centers = optimal_clusters, nstart = 10)

# Visualize the clustering results
fviz_cluster(kmeans_model, data = scaled_data, geom = "point")

# Perform k-means clustering with the optimal number of clusters
final <- kmeans(scaled_data, optimal_clusters, nstart = 25)

# Visualize the clustering results
fviz_cluster(final, data = scaled_data)

# Add clustering information to the original data
data_with_clusters <- data %>% 
  mutate(Cluster = final$cluster)

# Select relevant variables for summarisation
variables_of_interest <- c("Y","C1","C2","C3","C4","C5","C6")

# Summarize selected variables by cluster
summary_by_cluster <- data_with_clusters %>% 
  select(Cluster, all_of(variables_of_interest)) %>% 
  group_by(Cluster) %>% 
  summarise_all("mean")

# Print summary
print(summary_by_cluster)

# This print is much cleaner after being refined.

```

```{r dimnsionality reduction}
# Group clustering 1

# Choose the number of components (e.g., number of principal components)
num_components <- 7

# Perform PCA
pca_model <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Extract the principal components
principal_components <- pca_model$x[, 1:num_components]

# Visualize the clustered data in the reduced space
plot(principal_components, col = "blue", 
     main = "Clustered Data in Reduced Space (PCA)", xlab = "PC1", ylab = "PC2")

```


```{r modelling}

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(data), 0.7 * nrow(data))  # 70% train, 30% test
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit a linear regression model
lm_model <- lm(Y ~ C1 + C2 + C3 + C4 + C5 + C6 + C7, data = train_data) 

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model
mse <- mean((test_data$response - predictions)^2)
r_squared <- summary(lm_model)$r.squared

par(mfrow = c(1, 2))
plot(lm_model, which = c(1, 2))

plot(lm_model, which = 3)

summary(lm_model)

new_data1 <- data.frame(
  C1 = rnorm(100),  # Generate 100 random values for C1
  C2 = rnorm(100),  # Generate 100 random values for C2
  C3 = rnorm(100),  # Generate 100 random values for C3
  C4 = rnorm(100),  # Generate 100 random values for C4
  C5 = rnorm(100),  # Generate 100 random values for C5
  C6 = rnorm(100),  # Generate 100 random values for C6
  C7 = rnorm(100)   # Generate 100 random values for C7
)

# Predict the response variable Y using the linear regression model
predictions <- predict(lm_model, newdata = new_data1)

# View the predicted values
print(predictions)

mean(predictions)

# Accuracy of predicted values
actual_values <- data[, c("Y")]

mae <- mean(abs(actual_values - predictions))

print(paste("Mean Absolute Error (MAE):", mae))


# Model predictions for different combinations

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices1 <- sample(nrow(data), 0.7 * nrow(data))  # 70% train, 30% test
train_data1 <- data[train_indices1, ]
test_data1 <- data[-train_indices1, ]

# Fit a linear regression model
lm_model1 <- lm(Y ~ C7, data = train_data1)  

# Make predictions on the test set
predictions <- predict(lm_model1, newdata = test_data1)

# Evaluate the model
mse <- mean((test_data1$response - predictions)^2)
r_squared <- summary(lm_model1)$r.squared

par(mfrow = c(1, 2))
plot(lm_model1, which = c(1, 2))

plot(lm_model1, which = 3)

```

```{r}
# more relationship testing between variables

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(data), 0.7 * nrow(data))  # 70% train, 30% test
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fit a linear regression model
lm_model <- lm(Y ~ C3, data = train_data) 

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model
mse <- mean((test_data$response - predictions)^2)
r_squared <- summary(lm_model)$r.squared

par(mfrow = c(1, 2))
plot(lm_model, which = c(1, 2))


```
