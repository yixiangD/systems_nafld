library(dplyr)
rm(list = ls())
path <- "/Users/yd973/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/mantzoros/systems_nafld/data"
code.path <- "/Users/yd973/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/mantzoros/systems_nafld/R"
df <- read.csv(paste(path, "NewDataset for Yixiang.csv", sep = "/"), sep = ";", header = TRUE, skip = 1)

#### Clinical models
# identify feature groups
na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
drop <- na_count[na_count >= 0.5 * dim(df)[1] & na_count != dim(df)[1]]

# find the feature cut offs
print(na_count[na_count == dim(df)[1]])
sectors <- which(na_count == dim(df)[1])
outcomes <- colnames(df)[3:(sectors[1] - 1)]
nona.outcomes <- names(na_count[na_count == 0])
valid.outcomes <- nona.outcomes[nona.outcomes %in% outcomes]

df.demo <- df[, sectors[1]:(sectors[2] - 1)]
df.demo <- df.demo[-1]
df.lab <- df[, sectors[2]:(sectors[3] - 1)]
df.lab <- df.lab[-1]
df.horm <- df[, sectors[3]:(sectors[4] - 1)]
df.horm <- df.horm[-1]
df.lip <- df[, sectors[4]:(sectors[5] - 1)]
df.lip <- df.lip[-1]
df.meta <- df[, sectors[5]:ncol(df)]
df.meta <- df.meta[-1]

opt <- "meta"
for (opt in c("demo", "lab", "horm", "lip", "meta")) {
  if (opt == "demo") {
    df.local <- df.demo
  } else if (opt == "lab") {
    df.local <- df.lab
  } else if (opt == "horm") {
    df.local <- df.horm
  } else if (opt == "lip") {
    df.local <- df.lip
  } else {
    df.local <- df.meta
  }
}

n.epoch <- 400
b.size <- 128
for (outcome in valid.outcomes[1]) {
  df.final <- cbind(df.local, df[[outcome]])
  df.final <- df.final[apply(df.final, 1, function(x) sum(is.na(x)) <= ncol(df.final) * 0.1), ]
  colnames(df.final)[ncol(df.final)] <- "y"
  rownames(df.final) <- NULL
  df.final[1:(ncol(df.final) - 1)] <- DMwR::knnImputation(df.final[1:(ncol(df.final) - 1)])
  df.final[1:(ncol(df.final) - 1)] <- scale(df.final[1:(ncol(df.final) - 1)])

  # Assuming 'data' has numeric predictors and a binary factor target variable
  data <- df.final
  data_rows <- floor(0.8 * nrow(df.final))
  indices <- sample(c(1:nrow(df.final)), data_rows)
  train_data <- as.matrix(data[indices, -which(names(data) == "y")])
  train_labels <- as.numeric(data[indices, "y"]) # Convert factor to 0/1 numeric
  test_data <- as.matrix(data[-indices, -which(names(data) == "y")])
  test_labels <- as.numeric(data[-indices, "y"]) # Convert factor to 0/1 numeric

  # Define the model
  model <- keras::keras_model_sequential() %>%
    keras::layer_dense(units = 16, activation = "relu", input_shape = c(ncol(train_data))) %>%
    keras::layer_dense(units = 16, activation = "relu") %>%
    keras::layer_dense(units = 1, activation = "sigmoid") # output layer with 1 unit for binary classification

  # Compile the model
  model %>% keras::compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )

  # Fit the model to the training data
  history <- model %>% keras::fit(
    train_data,
    train_labels,
    epochs = n.epoch,
    batch_size = b.size,
    validation_split = 0.2
  )

  # Evaluate the model
  model %>% keras::evaluate(test_data, test_labels)

  # Generate predictions on new data
  predictions <- model %>% predict(train_data)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  train.accuracy <- sum(predicted_classes == train_labels) / length(train_labels)
  print(paste0("train acc: ", train.accuracy))

  predictions <- model %>% predict(test_data)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  test.accuracy <- sum(predicted_classes == test_labels) / length(test_labels)
  print(test.accuracy)

  # For more detailed performance metrics, you can use other functions like confusionMatrix from the caret package
  test.cm <- caret::confusionMatrix(factor(predicted_classes), factor(test_labels))
  print(test.cm)
}
