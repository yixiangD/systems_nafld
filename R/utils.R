# df, a clean dataframe containing columns of outcome and predictors
# outcome, a string variable of desired outcome variable
# feats, a vector of variables/features used for selection
# thr, a integer denoting variable thresholding

# Return a vector of selected features
select_feats <- function(df, outcome, feats, thr = 2) {
  df.sub <- df[, c(outcome, feats)]
  formula <- as.formula(paste0(outcome, "~."))
  model <- stats::lm(formula, data = df.sub)

  # Calculate VIF values for all features, identify the ones with VIF>2 and exclude them
  vif.val <- car::vif(model)
  high.vif.feats <- names(vif.val[vif.val > thr])
  return(high.vif.feats)
}


classifier <- function(df, outcome, feats) {
  df.sub <- df[, c(outcome, feats)]
  formula <- as.formula(paste0(outcome, "~."))
  df.sub[[outcome]] <- as.factor(df.sub[[outcome]])
  ctrl <- caret::trainControl(method = "cv", number = 5)
  model <- stats::glm(formula, data = df.sub, family = binomial())
  return(model)
}


plot.roc <- function(model, df, outcome) {
  predictions <- predict(model, df, type = "response")
  roc <- pROC::roc(
    response = df[[outcome]],
    predictor = predictions
  )
  df.roc <- data.frame(roc$sensitivities, roc$specificities)
  colnames(df.roc) <- c("sensitivity", "specificity")
  fig <- ggplot2::ggplot(df.roc, ggplot2::aes(x = 1 - sensitivity, y = specificity)) +
    ggplot2::annotate("text", x = 0.8, y = 0, label = paste0("AUC = ", round(roc$auc, 4))) +
    ggplot2::geom_line()
  return(fig)
}


plot.prc <- function(model, df, outcome) {
  predictions <- predict(model, df, type = "response")
  res <- cbind(df[[outcome]], predictions)
  colnames(res) <- c("y", "yscore")
  res <- as.data.frame(res)
  res$y <- factor(res$y - 1)
  df.prc <- yardstick::pr_curve(res, y, yscore)
  fig <- ggplot2::ggplot(df.prc, ggplot2::aes(x = recall, y = precision)) +
    ggplot2::geom_path() +
    ggplot2::coord_equal() +
    ggplot2::theme_bw()
  return(fig)
}
