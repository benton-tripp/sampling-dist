get.summary.stats <- function(dt, col="y") {
  data.table(
    .mean=mean(dt[[col]]),
    .variance=var(dt[[col]]),
    .stdev=sd(dt[[col]]),
    .median=median(dt[[col]]),
    .min=min(dt[[col]]),
    .max=max(dt[[col]])
  )
}
