# load data
library(dplyr)
path <- "~/Downloads/michail2"
df <- readxl::read_excel(paste(path, "full_data.xlsx", sep = "/"))
# drop redundant columns
data <- df[, !grepl("\\.", names(df))]

# build unsupervised learning models
col.id <- c("SEQN")
col.lab <- c("MASLD", "Cryptogenic", "MetALD", "FIB4", "APRI", "HOMAIR", "ION", "fibrosis")
col.bin.lab <- c("MASLD", "MetALD")
# TODO:HERE may be feature engineering to get a subset
col.feat <- colnames(data)[!colnames(data) %in% c(col.id, col.lab)]

# Load Required Libraries
library(dplyr) # For data manipulation
library(ggplot2) # For data visualization
library(FactoMineR) # For PCA
library(cluster) # For K-means clustering

# Load and Preprocess Data
numeric_data <- data[, col.feat]

# Perform PCA
pca_result <- FactoMineR::PCA(numeric_data, scale.unit = TRUE)
# Select the Number of Components
fig.pca <- plot(pca_result, choix = "var")
ggsave(paste(path, "pca.pdf", sep = "/"), fig.pca)
# Decide the number of components to keep (e.g., K)
K <- 2
# Extract PCA Components
selected_pca <- pca_result$ind$coord[, 1:K]

# Perform K-means Clustering
wss <- numeric(10)

for (i in 1:10) {
  kmeans_model <- kmeans(selected_pca, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}

# Plot the Elbow curve to choose an appropriate K

# Choose K based on the plot
final_kmeans_model <- kmeans(selected_pca, centers = K)
data$cluster <- final_kmeans_model$cluster
data <- cbind(data, selected_pca)
# Visualize the Results
for (lab in col.bin.lab) {
  fig <- ggplot(data, aes(Dim.1, Dim.2, color = factor(!!sym(lab)))) +
    geom_point() +
    labs(title = "PCA + K-means Clustering")
  ggsave(paste(path, paste0(lab, "_pca+kmeans.pdf"), sep = "/"), fig)
}
