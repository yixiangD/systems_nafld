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

opt <- "lab"
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

source(paste(code.path, "utils.R", sep = "/"))
for (outcome in valid.outcomes) {
  df.final <- cbind(df.local, df[[outcome]])
  df.final <- df.final[apply(df.final, 1, function(x) sum(is.na(x)) <= ncol(df.final) * 0.1), ]
  colnames(df.final)[ncol(df.final)] <- "y"
  rownames(df.final) <- NULL
  df.final[1:(ncol(df.final) - 1)] <- DMwR::knnImputation(df.final[1:(ncol(df.final) - 1)])

  predictors <- colnames(df.final)[1:(ncol(df.final) - 1)]

  sel.feats <- predictors
  print(sel.feats)

  data_rows <- floor(0.80 * nrow(df.final))
  train_indices <- sample(c(1:nrow(df.final)), data_rows)
  train_data <- df.final[train_indices, ]
  test_data <- df.final[-train_indices, ]

  form.string <- paste("y", paste(sel.feats, collapse = " + "), sep = "~")
  nn.model <- neuralnet::neuralnet(
    as.formula(form.string),
    data = train_data,
    hidden = c(16, 2),
    linear.output = FALSE
  )

  pred <- predict(nn.model, test_data)
  pred
  labels <- sort(unique(df.final$y))
  prediction_label <- data.frame(max.col(pred)) %>%
    dplyr::mutate(pred = labels[max.col.pred.] + 1) %>%
    dplyr::select(2) %>%
    unlist()

  table(test_data$y, prediction_label)
  res <- as.data.frame(cbind(test_data$y, prediction_label))
  colnames(res) <- c("y_true", "y_pred")
  res.roc <- pROC::roc(res$y_true, res$y_pred)
  print(outcome)
  print(res.roc$auc)
}
