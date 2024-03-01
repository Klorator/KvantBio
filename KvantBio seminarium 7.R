# Seminarium block 7

## Fråga 1
# r-värde
# p- värde


## Fråga 2
# Pearson är parametrisk
# Spearman är inte parametrisk och kan hantera 
# outliers och icke-normal fördelning

## Fråga 3
# Korrelation för korrelation
# Regression för kausalitet

## Fråga 4
# p-value is significant
# r-value is weakly positive

## Fråga 5


## Fråga 6

## Fråga 7

## Fråga 8
my_hist <- function(x) {
  par(mfrow = c(1,2))
  hist(x)
  qqnorm(x)
  qqline(x)
  par(mfrow = c(1,1))
}
my_hist(GBM$citrat[GBM$celltyp == "NSC10"])
my_hist(GBM$citrat[GBM$celltyp == "U251"])
my_hist(GBM$citrat[GBM$celltyp == "GBM37"])
my_hist(GBM$citrat[GBM$celltyp == "GBM28"])

GBM_model <- aov(
  citrat ~ celltyp,
  data = GBM
)
par(mfrow = c(2,2))
plot(GBM_model)
par(mfrow = c(1,1))
plot(TukeyHSD(GBM_model))


GBM_kruskal <- kruskal.test(
  citrat ~ celltyp,
  data = GBM
)

pairwise.wilcox.test(
  GBM$citrat,
  GBM$celltyp,
  p.adjust.method = "holm"
)
