# Undersöka normalfördelning

## Load data
lins <- read.table(
  file = "lins.csv",
  dec = ",",
  header = T
)

## Histogram & QQ-plot
hist(lins$growth)

qqnorm(
  lins$growth,
  col = "red",
  pch = 16
)
qqline(
  lins$growth
)

## Log 10 transformation
lins$growth.log <- log10(lins$growth)

## Histogram & QQ-plot on transformed data
hist(lins$growth.log)

qqnorm(
  lins$growth.log,
  col = "red",
  pch = 16
)
qqline(
  lins$growth.log
)
