# df, a clean dataframe containing columns of outcome and predictors
# outcome, a string variable of desired outcome variable
# feats, a vector of variables/features used for selection
# thr, a integer denoting variable thresholding
select_feats <- function(df, outcome, feats, thr = 2) {
  df.sub <- df[, c(outcome, feats)]
  formula <- as.formula(paste0(outcome, "~."))
  model <- stats::lm(formula, data = df.sub)

  # Calculate VIF values for all features, identify the ones with VIF>2 and exclude them
  vif.val <- car::vif(model)
  high.vif.feats <- names(vif.val[vif.val > thr])
  return(high.vif.feats)
}
