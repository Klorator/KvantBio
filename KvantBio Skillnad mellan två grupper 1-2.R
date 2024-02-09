# Skillnad mellan två grupper

# Del 1

## Load data
zink <- read.table(
  file = "zntill.csv",
  sep = ";",
  dec = ",",
  header = T
)

## Check normal distribution
hist(zink$ktrl_p)
qqnorm(
  zink$ktrl_p,
  col = "red",
  pch = 16
)
qqline(zink$ktrl_p)

hist(zink$tih_p)
qqnorm(
  zink$tih_p,
  col = "orange",
  pch = 16
)
qqline(zink$tih_p)

## t-test
test.plasma <- t.test(
  zink$ktrl_p,
  zink$tih_p
)
test.plasma

## CI plot
medel_ktrl <- mean(zink$ktrl_p)
medel_tih <- mean(zink$tih_p)
medel <- c(medel_ktrl, medel_tih)

test.ktrl <- t.test(zink$ktrl_p)
test.tih <- t.test(zink$tih_p)
conf.lower <- c(test.ktrl$conf.int[1], test.tih$conf.int[1])
conf.upper <- c(test.ktrl$conf.int[2], test.tih$conf.int[2])

plotrix::plotCI(
  x = c(1, 2),
  y = medel,
  ui = conf.upper,
  li = conf.lower,
  xlim = c(0.5, 2.5),
  ylim = c(0, 1.2),
  pch = 16,
  col = "blue",
  xlab = "Grupp",
  ylab = "Zn-koncentration",
  xaxt = "n"
)
axis(1,
     at = c(1, 2),
     labels = c("Kontroll", "Behandling")
)


# Del 2.1

## Load data
LD <- read.table(
  file = "LD.csv",
  sep = ";",
  dec = ",",
  header = T
)

## Get diff
LD$diff <- LD$after - LD$before

## Normality of diff
hist(LD$diff)
qqnorm(
  LD$diff,
  col = "red",
  pch = 16
)
qqline(LD$diff)

## t-test
test.diff <- t.test(
  LD$before,
  LD$after,
  paired = T
)

test.diff

# Del 2.2
## Load data
hgexp <- read.table(
  file = "hgexp.csv",
  sep = ";",
  dec = ",",
  header = T
)

## Get diff
# hgexp$diff <- hgexp$after - hgexp$before # finns redan

## Normality of diff
hist(hgexp$diff)
qqnorm(
  hgexp$diff,
  col = "red",
  pch = 16
)
qqline(hgexp$diff)

## Wilcox test
wilcox.diff <- wilcox.test(
  hgexp$before,
  hgexp$after,
  paired = T
)
wilcox.diff

## Boxplot
boxplot(
  hgexp$before,
  hgexp$after
)


# Del 2.3
## Load data
flowers <- read.table(
  file = "flowers.csv",
  sep = ";",
  dec = ",",
  header = T
)

## Normal distribution
hist(flowers$flowers[flowers$grazing == "high"])
qqnorm(
  flowers$flowers[flowers$grazing == "high"],
  col = "red",
  pch = 16
)
qqline(flowers$flowers[flowers$grazing == "high"])


hist(flowers$flowers[flowers$grazing == "low"])
qqnorm(
  flowers$flowers[flowers$grazing == "low"],
  col = "red",
  pch = 16
)
qqline(flowers$flowers[flowers$grazing == "low"])

## analysis
test.flowers <- t.test(
  flowers$flowers[flowers$grazing == "high"],
  flowers$flowers[flowers$grazing == "low"]
)
test.flowers
