library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)

# Load the dataset
df <- read.csv("Mall_Customers.csv")

# View the structure and summary of the dataset
colnames(df)
str(df)
summary(df)

df <- df %>% drop_na()

df$Gender <- as.factor(df$Gender)

df_scaled <- df %>% 
  mutate_at(vars(Age, `Annual.Income..k..`, `Spending.Score..1.100.`), scale)

ggplot(df, aes(x=Age)) + geom_histogram(binwidth=5) + ggtitle("Age Distribution")
ggplot(df, aes(x=`Annual.Income..k..`)) + geom_histogram(binwidth=5) + ggtitle("Income Distribution")
ggplot(df, aes(x=`Spending.Score..1.100.`)) + geom_histogram(binwidth=5) + ggtitle("Spending Score Distribution")

pairs(df_scaled)

summary(df_scaled)
sum(is.na(df_scaled))          # Check for NA values
sum(is.infinite(as.matrix(df_scaled))) # Check for Inf values
sum(is.nan(as.matrix(df_scaled)))  # Check for NaN values


df_scaled <- df_scaled %>% 
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_all(~ ifelse(is.nan(.), mean(., na.rm = TRUE), .)) %>%
  mutate_all(~ ifelse(is.infinite(.), mean(., na.rm = TRUE), .))

set.seed(123)
wss <- function(k) {
  kmeans(df_scaled, k, nstart = 10 )$tot.withinss
}
k.values <- 1:10
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = TRUE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 5, nstart = 25)
df$cluster <- kmeans_result$cluster

fviz_cluster(kmeans_result, data = df_scaled)

dist_mat <- dist(df_scaled, method = "euclidean")

hc <- hclust(dist_mat, method = "ward.D2")
plot(hc, labels = FALSE, hang = -1)
rect.hclust(hc, k = 5, border = 2:6)
df$hc_cluster <- cutree(hc, k = 5)

fviz_dend(hc, k = 5, cex = 0.5, color_labels_by_k = TRUE)

cluster_summary <- df %>% 
  group_by(cluster) %>% 
  summarise(across(-Gender, \(x) mean(x, na.rm = TRUE)))

gender_summary <- df %>%
  group_by(cluster, Gender) %>%
  summarise(count = n())

list(cluster_summary = cluster_summary, gender_summary = gender_summary)

ggplot(df, aes(x=`Annual.Income..k..`, y=`Spending.Score..1.100.`, color=factor(cluster))) +
  geom_point() + ggtitle("Customer Segments")
