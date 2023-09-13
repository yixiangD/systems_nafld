# load data
library(dplyr)
path <- "~/Downloads/Michail"
df <- SASxport::read.xport(paste(path, "P_DEMO.XPT", sep = "/"))

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
    "P_DR1TOT.XPT"
  )
for (i in files) {
  temp <- SASxport::read.xport(paste(path, i, sep = "/"))
  df <- merge(df, temp, by = intersect(names(temp), names(df)))
}
