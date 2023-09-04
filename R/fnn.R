path <- "/Users/yd973/Library/CloudStorage/OneDrive-BethIsraelLaheyHealth/NAFLD/data"
df <- read.csv(paste(path, "NAFLDdf.csv", sep = "/"))

#### Clinical models
dfC <-
  df[, c(
    "ControlsVSatRiskNASH",
    "Age",
    "sex1f0m",
    "BMIcmm2",
    "WaistCircumferencecm",
    "HipCircumferencecm",
    "TOTALCHOLESTEROLmgdl",
    "HDLmgdl",
    "LDLmgdl",
    "TGmgdl",
    "ASM",
    "SMI",
    "SMOKE0NO1YES",
    "centralobesity",
    "DiabetesYES1NO0",
    "ATPIIIMetS01",
    "HYPERTENSIONYES1NO0",
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
    "BMIcmm2",
    "WaistCircumferencecm",
    "HipCircumferencecm",
    "TOTALCHOLESTEROLmgdl",
    "HDLmgdl",
    "LDLmgdl",
    "TGmgdl",
    "ASM",
    "SMI",
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

data_rows <- floor(0.80 * nrow(dfC))
train_indices <- sample(c(1:nrow(dfC)), data_rows)
train_data <- dfC[train_indices, ]
test_data <- dfC[-train_indices, ]

outcome <- "ControlsVSatRiskNASH"
predictors <- c(
  "Age",
  "sex1f0m",
  "BMIcmm2",
  "WaistCircumferencecm",
  "HipCircumferencecm",
  "TOTALCHOLESTEROLmgdl",
  "HDLmgdl",
  "LDLmgdl",
  "TGmgdl",
  "ASM",
  "SMI",
  "SMOKE0NO1YES",
  "centralobesity",
  "DiabetesYES1NO0",
  "ATPIIIMetS01",
  "HYPERTENSIONYES1NO0",
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

form.string <- paste(outcome, paste(predictors, collapse = " + "), sep = "~")
nn.model <- neuralnet::neuralnet(
  as.formula(form.string),
  data = train_data,
  hidden = c(6, 2),
  linear.output = FALSE
)

pred <- predict(nn.model, test_data)
labels <- sort(unique(dfC[[outcome]]))
prediction_label <- data.frame(max.col(pred)) %>%
  dplyr::mutate(pred = labels[max.col.pred.] + 1) %>%
  dplyr::select(2) %>%
  unlist()

table(test_data[[outcome]], prediction_label)
res <- cbind(test_data[[outcome]], prediction_label)
res
