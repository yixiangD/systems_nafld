library(SASxport)
library(foreign)
path <- "~/Downloads/Michail"
output.dir <- "~/Downloads/michail2/"
temp <- read.xport(paste(path, "P_ALQ.XPT", sep = "/"))
df <- read.xport(paste(path, "P_DEMO.XPT", sep = "/"))

files <-
  c(
    "P_BMX.XPT",
    "P_LUX.XPT",
    "P_BIOPRO.XPT",
    "P_CBC.XPT",
    "P_GLU.XPT",
    "P_GHB.XPT",
    "P_MCQ.XPT",
    "P_BPQ.XPT",
    "P_BPXO.XPT",
    "P_TRIGLY.XPT",
    "P_HDL.XPT",
    "P_DIQ.XPT",
    "P_INS.XPT",
    "P_HEQ.XPT",
    "P_FETIB.XPT",
    "P_ALQ.XPT",
    "P_RXQ_RX.XPT",
    "P_DR1TOT.XPT"
  )
for (i in files) {
  temp <- read.xport(paste(path, i, sep = "/"))
  # print(dim(temp))
  df <- merge(df, temp, by = intersect(names(temp), names(df)))
}

list.of.vars <- c(
  "SEQN", "LUXCAPM", "BMXBMI", "BMXWAIST", "BMXHIP", "RIDRETH3", "RIAGENDR", "RIDAGEYR", "LBXSASSI",
  "LBXPLTSI", "LBXSATSI", "BMXWAIST", "LBDGLUSI", "LBXGH", "DIQ010", "DIQ050", "DIQ070", "LBDIRNSI",
  "BPQ030", "BPQ020", "BPXOSY1", "BPXODI1", "BPXOSY2", "BPXODI2", "ALQ121",
  "BPXOSY3", "BPXODI3", "BPQ040A", "BPQ050A", "LBDTRSI", "BPQ100D", "RXDUSE",
  "LBDHDDSI", "LBDINSI", "LBXTR", "LBXSGTSI", "LBDSGLSI", "LUXSMED", "HEQ010", "HEQ030", "DR1TALCO"
)

for (col in list.of.vars) {
  if (!col %in% colnames(df)) {
    print(col)
  }
}
dfSEL <- df[, list.of.vars]
head(dfSEL)

columns_to_replace_na_with_mean <- setdiff(names(dfSEL), c("DIQ050", "DIQ070", "BPQ030", "BPQ020", "BPQ040A", "BPQ050A", "BPQ100D"))

for (col in columns_to_replace_na_with_mean) {
  dfSEL[is.na(dfSEL[, col]), col] <- mean(dfSEL[, col], na.rm = TRUE)
}

columns_to_replace_na_with_median <- c("DIQ050", "DIQ070", "BPQ030", "BPQ020", "BPQ040A", "BPQ050A", "BPQ100D", "HE0010", "HE0030")

for (col in columns_to_replace_na_with_median) {
  dfSEL[is.na(dfSEL[, col]), col] <- median(dfSEL[, col], na.rm = TRUE)
}
# age criterion
dfSEL <- dfSEL[dfSEL$RIDAGEYR >= 18, ]

# Define MASLD column
dfSEL$MASLD <- ifelse(
  (dfSEL$LUXCAPM >= 285) &
    (
      dfSEL$BMXBMI >= 25 |
        (dfSEL$BMXBMI >= 23 & dfSEL$RIDRETH3 == 6) |
        (dfSEL$RIAGENDR == 1 & dfSEL$BMXWAIST > 84) |
        (dfSEL$RIAGENDR == 2 & dfSEL$BMXWAIST > 80) |
        # fasting glucose
        dfSEL$LBDGLUSI >= 5.6 |
        # hbA1c
        dfSEL$LBXGH >= 5.7 |
        # told had diabetes
        dfSEL$DIQ010 == 1 |
        # taking insulin now
        dfSEL$DIQ050 == 1 |
        # taking pills for diabetes
        dfSEL$DIQ070 == 1 |
        # told had HBP??
        dfSEL$BPQ030 == 1 |
        dfSEL$BPQ020 == 1 |
        # blood pressure
        (dfSEL$BPXOSY1 >= 130 & dfSEL$BPXODI1 >= 85) |
        (dfSEL$BPXOSY2 >= 130 & dfSEL$BPXODI2 >= 85) |
        (dfSEL$BPXOSY3 >= 130 & dfSEL$BPXODI3 >= 85) |
        # taking prescription for hypertension
        dfSEL$BPQ040A == 1 |
        # now taking prescribed medicine for HBP
        dfSEL$BPQ050A == 1 |
        # Triglycerides, refrig serum
        dfSEL$LBDTRSI >= 1.70 |
        # 2-hr loading glucose
        dfSEL$LBDSGLSI >= 7.8 |
        (
          # Direct HDL-Cholesterol
          (dfSEL$RIAGENDR == 1 & dfSEL$LBDHDDSI >= 1.0) |
            (dfSEL$RIAGENDR == 2 & dfSEL$LBDHDDSI >= 1.3)
        ) |
        #  Now taking prescribed medicine for cholesterol??
        dfSEL$BPQ100D == 1
    ),
  1, # MASLD = 1 if conditions are met
  0 # MASLD = 0 if conditions are not met
)

# Define Cryptogenic column
dfSEL$Cryptogenic <- ifelse(
  (dfSEL$MASLD == 1) &
    (
      # no excessive alcohol use: 42 grams for women and 56 grams for men
      dfSEL$ALQ121 <= 2 &
        # no Hep BC infection
        dfSEL$HEQ010 == 2 &
        dfSEL$HEQ030 == 2 &
        # no pharma agents??
        # dfSEL$RXDUSE == 2 &
        # no iron overload, an estimate
        dfSEL$LBDIRNSI <= 30
    ),
  1, # Cryptogenic = 1 if conditions are met
  0 # Cryptogenic = 0 if conditions are not met
)


# Define MetALD column
dfSEL$MetALD <- ifelse(
  (dfSEL$MASLD == 1) &
    (
      (dfSEL$RIAGENDR == 2 & dfSEL$DR1TALCO > 140) |
        (dfSEL$RIAGENDR == 1 & dfSEL$DR1TALCO > 210)
    ),
  1, # MetALD = 1 if conditions are met
  0 # MetALD = 0 if conditions are not met
)

dfSEL$FIB4 <- (dfSEL$RIDAGEYR * dfSEL$LBXSASSI) / (dfSEL$LBXPLTSI * sqrt(dfSEL$LBXSATSI))

dfSEL$APRI <- ((dfSEL$LBXSASSI / 40) / dfSEL$LBXPLTSI) * 100
dfSEL$HOMAIR <- ((dfSEL$LBDGLUSI * dfSEL$LBDINSI) / 22.5)
dfSEL$ION <- ifelse(dfSEL$RIAGENDR == 1,
  1.33 * (dfSEL$BMXWAIST / dfSEL$BMXHIP) + (0.03 * dfSEL$LBXTR) + (0.18 * dfSEL$LBXSATSI) + (8.53 * dfSEL$HOMAIR) - 13.93,
  (0.02 * dfSEL$LBXTR) + (0.24 * dfSEL$LBXSATSI) + (9.61 * dfSEL$HOMAIR) - 13.99
)
dfSEL$fibrosis <- ifelse(dfSEL$LUXSMED >= 12, 1, 0)
dfSEL <- dfSEL[dfSEL$RIDAGEYR >= 18, ]
writexl::write_xlsx(dfSEL, paste(output.dir, "full_data.xlsx", sep = "/"))
