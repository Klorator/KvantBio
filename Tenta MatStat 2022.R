# MatStat tenta 2022

# Matte ----



# Statistik ----

## Fråga 2 ----
nektarin <- read.table(
  file = "Data nektarin.csv",
  sep = ";",
  dec = ",",
  header = T
)

# Regression
nekt_lm <- lm(
  ohms ~ juice,
  data = nektarin
)

# Diagnostics
model_diagnostic <- function(modell) {
  par(mfrow = c(1,2))
  plot(modell, which = 1)
  plot(modell, which = 2)
  par(mfrow = c(1,1))
}
model_diagnostic(nekt_lm)
# Andragradare?
# qq-plot looks good

# Gör bootstrap!
nekt_boot <- boot.pval::boot_summary(nekt_lm)
nekt_boot

print(paste("Anpassad linje är: y =",nekt_boot$Estimate[2],"* x +",nekt_boot$Estimate[1]))
