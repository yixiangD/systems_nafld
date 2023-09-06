# load data
path <- "~/Downloads/Michail"
files <- list.files(path)
xpt.files <- files[grepl("XPT$", files)]
df <- list()
name <- c()
for (f in xpt.files) {
  temp <- SASxport::read.xport(paste(path, f, sep = "/"))
  # print(temp)
  temp.name <- strsplit(f, "\\.")[[1]][1]
  name <- c(name, temp.name)
  df[[temp.name]] <- temp
}
print(colnames(df[[name[1]]]))
