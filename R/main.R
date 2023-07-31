data.dir <-
  "/Users/yd973/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/NAFLD/data"
# Import dataset and include features of interest
df <- read.csv(paste(data.dir, "NASHdf.csv", sep = "/"))

#### Clinical models
dfC <-
  df[, c(
    "ControlsVSatRiskNASH",
    "Age",
    "sex1f0m",
    "Weightkg",
    "Heightcm",
    "BMIcmm2",
    "WaistCircumferencecm",
    "HipCircumferencecm",
    "HYPERTENSIONYES1NO0",
    "TOTALCHOLESTEROLmgdl",
    "HDLmgdl",
    "LDLmgdl",
    "TGmgdl",
    "DiabetesYES1NO0",
    "Glycemiamgdl",
    "InsulinemiamlUml",
    "HOMAIRvalue",
    "ASTUIL",
    "ALTUIL",
    "GAMMAGTUIL",
    "Albumingl",
    "Plateletsx109l",
    "Creatininemgdl",
    "eGFRCDKEPI2021"
  )]

dfC <- dfC[complete.cases(dfC$ControlsVSatRiskNASH), ]
non_binary_columns <-
  c(
    "Age",
    "Weightkg",
    "Heightcm",
    "BMIcmm2",
    "WaistCircumferencecm",
    "HipCircumferencecm",
    "TOTALCHOLESTEROLmgdl",
    "HDLmgdl",
    "LDLmgdl",
    "TGmgdl",
    "Glycemiamgdl",
    "InsulinemiamlUml",
    "HOMAIRvalue",
    "ASTUIL",
    "ALTUIL",
    "GAMMAGTUIL",
    "Albumingl",
    "Plateletsx109l",
    "Creatininemgdl",
    "eGFRCDKEPI2021"
  )
dfC[, non_binary_columns] <-
  lapply(dfC[, non_binary_columns], function(x) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  })
dfC <- na.omit(dfC)

# feature selection
source("R/utils.R")
outcome <- "ControlsVSatRiskNASH"
predictors <- c(
  "Age",
  "sex1f0m",
  "Weightkg",
  "Heightcm",
  "BMIcmm2",
  "WaistCircumferencecm",
  "HipCircumferencecm",
  "HYPERTENSIONYES1NO0",
  "TOTALCHOLESTEROLmgdl",
  "HDLmgdl",
  "LDLmgdl",
  "TGmgdl",
  "DiabetesYES1NO0",
  "Glycemiamgdl",
  "InsulinemiamlUml",
  "HOMAIRvalue",
  "ASTUIL",
  "ALTUIL",
  "GAMMAGTUIL",
  "Albumingl",
  "Plateletsx109l",
  "Creatininemgdl",
  "eGFRCDKEPI2021"
)
# 1) univariate plot
df.sep <- reshape2::melt(dfC[, c(outcome, predictors)], id = outcome)
df.sep[[outcome]] <- as.factor(df.sep[[outcome]])
df.sep$outcome <- df.sep[[outcome]]
color.grp <- c("1" = "red", "0" = "blue")
print(table(df.sep$outcome))
fig.uni <- ggplot2::ggplot(df.sep, ggplot2::aes(x = outcome, y = value, fill = outcome)) +
  ggplot2::geom_violin() +
  ggpubr::stat_compare_means(label = "p.signif") +
  ggplot2::geom_jitter(shape = 21, size = 1.5, alpha = 0.6) +
  ggplot2::scale_fill_manual(values = color.grp) +
  ggplot2::facet_wrap(~variable, nrow = 3, scales = "free")

fname <- "figs/univar.pdf"
ggplot2::ggsave(fname, fig.uni,
  width = 12,
  height = 8,
  dpi = 300
)
stop()
# 2) feature selection
sel.feats <- select_feats(dfC, outcome, predictors)
print(sel.feats)

# 3) fitting model
fit.model <- classifier(dfC, outcome, sel.feats)

# 4.1) evaluate model: ROC curve
fig.roc <- plot.roc(fit.model, dfC, outcome)
fname <- "figs/roc.pdf"
ggplot2::ggsave(fname, fig.roc,
  width = 4,
  height = 3,
  dpi = 300
)

# 4.2) evaluate model: precision-recall curve
fig.prc <- plot.prc(fit.model, dfC, outcome)
fname <- "figs/precision_recall.pdf"
ggplot2::ggsave(fname, fig.prc,
  width = 4,
  height = 3,
  dpi = 300
)
