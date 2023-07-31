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
# encapsulate feature selection in a function
sel.feats <- select_feats(dfC, outcome, predictors)
print(sel.feats)
