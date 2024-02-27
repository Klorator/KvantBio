# Seminarium

## Load data
# library(readr)
# ortdata <- readr::read_csv2(
#   file = "ortdata.csv"
# )
ortdata <- read.table(
  file = "ortdata.csv",
  header = T,
  sep = ";",
  dec = ","
)

## Fråga 6
### Convert height cm to feet
ortdata$hojd_fot <- ortdata$hojd / 30.48

library(plotrix)

mv <- c()
s <- c()
sem <- c()

for (i in 1:4) {
  mv[i] <- mean(ortdata$klorofyll[ortdata$omrade == i])
  s[i] <- sd(ortdata$klorofyll[ortdata$omrade == i])
  sem[i] <- s[i] / sqrt(length(ortdata$omrade[ortdata$omrade == i]))
}

plotCI(
  x = 1:4,
  y = mv,
  uiw = sem,
  xlim = c(0,5),
  pch = 16,
  col = RColorBrewer::brewer.pal(4, "Dark2"),
  xlab = "Område",
  ylab = "Klorofyll",
  xaxt = "n"
)
axis(
  side = 1,
  at = 1:4,
  labels = 1:4
)

