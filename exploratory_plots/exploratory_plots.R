library(ggplot2)

### Convert data to RData format
# train <- read.csv("training_filev1_new2.csv")
# save(train, file="train.RData")

### Read data
if (!exists("train")) {
  load("data/train.RData")
  # Plot a random sample of 10000 points to save time and disk space
  train <- train[sample(nrow(train), 10000),]
}

cols <- names(train)

### Linear plots
for (i in 9:length(cols)) {
  ptm <- proc.time()
  field <- cols[i]
  message(sprintf("Making plot for %s (%d/%d)... ", field, i, length(cols)), appendLF=F)
  p <- ggplot(train, aes_string(x=field, y="Mail_Return_Rate_CEN_2010", color="State_name")) + geom_point(alpha=0.5) +
    opts(legend.position="none")
  outfile <- sprintf("plots/%s.pdf", field)
  pdf(outfile)
  print(p)
  dev.off()
  message(sprintf("Done (%.3fs)", proc.time()[3] - ptm[3]))
}

### Log plots
for (i in 9:length(cols)) {
  ptm <- proc.time()
  field <- cols[i]
  message(sprintf("Making plot for %s (%d/%d)... ", field, i, length(cols)), appendLF=F)
  p <- ggplot(train, aes_string(x=field, y="Mail_Return_Rate_CEN_2010", color='State_name')) + geom_point(alpha=0.5) + 
    scale_x_log10() + opts(legend.position="none")
  outfile <- sprintf("plots/%s_xlog.pdf", field)
  pdf(outfile)
  print(p)
  dev.off()
  message(sprintf("Done (%.3fs)", proc.time()[3] - ptm[3]))
}
