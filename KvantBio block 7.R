# KvantBio block 7

# Quiz 1 ----
# Korrelation

## Fråga 1-3 ----

## Check assumptions

### crassa
plot(crassa$nitrit,
     crassa$antal)
crassa_lm <- lm(
  antal ~ nitrit,
  data = crassa
)
plot(crassa_lm, which = 1) # Linear, ok
plot(crassa_lm, which = 2) # Normal distribution, good
plot(crassa_lm, which = 3) # Equal variance, ok

qqnorm(crassa$nitrit); qqline(crassa$nitrit) # Normal distribution, good
qqnorm(crassa$antal); qqline(crassa$antal) # Normal distribution, good


### raci
plot(raci$nitrit,
     raci$langd)
raci_lm <- lm(
  langd ~ nitrit,
  data = raci
)
plot(raci_lm, which = 1) # Linear, bad
plot(raci_lm, which = 2) # Normal distribution, not great
plot(raci_lm, which = 3) # Equal variance, bad

qqnorm(raci$nitrit); qqline(raci$nitrit) # Normal distribution, not great
qqnorm(raci$langd); qqline(raci$langd) # Normal distribution, not great


## Fråga 4-5 ----
# crassa -> pearsson => r = -0.351 & p = 0.02266
# Significant but weak
crassa_cor <- cor.test(
  crassa$nitrit,
  crassa$antal
)
# raci -> spearman => rho = -0.8285714 & p = 0.05833
# !! Not significant !!
raci_cor <- cor.test(
  raci$nitrit,
  raci$langd,
  method = "spearman"
)

## Fråga 6


# Quiz 2 ----
# Regression

## Fråga 1-2 ----
NVDI_lm <- lm(
  plant ~ income,
  data = NVDI
)

## Fråga 3-9 ----
# Diagnostics
par(mfrow = c(2,2))
plot(NVDI_lm)
par(mfrow = c(1,1))
# Kinda bad, but whatever

summary(NVDI_lm)
# lm(formula = plant ~ income, data = NVDI)
# p = 0.01105
# r^2 adj. = 0.4089 
# df = 11
# intercept = 3.024e-01
# slope = 9.507e-07

plot(NVDI$income,
     NVDI$plant)


## Info box ----
plot( # Basic plot
  NVDI$income,
  NVDI$plant,
  xlim = c(min(NVDI$income) * 0.9,
           max(NVDI$income) * 1.1),
  ylim = c(min(NVDI$plant) * 0.9,
           max(NVDI$plant) * 1.1),
  col = "darkblue",
  pch = 16,
  xlab = "Income",
  ylab = "NVDI"
)
clip( # Limit regression line (coordinates make a rectangle to "clip")
  x1 = min(NVDI$income),
  x2 = max(NVDI$income),
  y1 = min(NVDI$plant),
  y2 = max(NVDI$plant)
)
abline( # Add regression line
  NVDI_lm,
  col = "red"
)
clip( # Open the limits to not clip text (here, same as axis limits)
  x1 = min(NVDI$income) * 0.9,
  x2 = max(NVDI$income) * 1.1,
  y1 = min(NVDI$plant) * 0.9,
  y2 = max(NVDI$plant) * 1.1
)
text( # Add text 
  NVDI$income,
  NVDI$plant,
  labels = NVDI$area,
  pos = 4,
  col = "blue"
)


# Quiz 3 ----
# ANOVA

## Fråga 1-2 ----
boxplot(ind_per_km ~ area,
        data = steenbok)

## Fråga 3-11 ----
# Assumptions of ind_p_km by area
## fr
hist(steenbok$ind_per_km[steenbok$area == "fr"]) # Norm. dist., skewed
qqnorm(steenbok$ind_per_km[steenbok$area == "fr"]) # Possibly an outlier
qqline(steenbok$ind_per_km[steenbok$area == "fr"])

## cga
hist(steenbok$ind_per_km[steenbok$area == "cga"]) # Norm. dist., good
qqnorm(steenbok$ind_per_km[steenbok$area == "cga"]) # Looks fine
qqline(steenbok$ind_per_km[steenbok$area == "cga"])

## wma
hist(steenbok$ind_per_km[steenbok$area == "wma"]) # Norm. dist., fine
qqnorm(steenbok$ind_per_km[steenbok$area == "wma"]) # Looks fine
qqline(steenbok$ind_per_km[steenbok$area == "wma"])

## np
hist(steenbok$ind_per_km[steenbok$area == "np"]) # Norm. dist., not np
qqnorm(steenbok$ind_per_km[steenbok$area == "np"]) # Looks fine
qqline(steenbok$ind_per_km[steenbok$area == "np"])

## Equal variance
steenbok_lm <- lm(
  ind_per_km ~ area,
  data = steenbok
)
par(mfrow = c(2,2))
plot(steenbok_lm)
par(mfrow = c(1,1))


## Fråga 12 ----
# Transform
steenbok$log_ipkm <- log(steenbok$ind_per_km)

steen_log_lm <- lm(
  log_ipkm ~ area,
  data = steenbok
)
par(mfrow = c(2,2))
plot(steen_log_lm)
par(mfrow = c(1,1))

# Nu ser allt bra ut!

## Fråga 13-16 ----
anova(steen_log_lm)

source( # Runs the entire .R script file
  file = file.path("KvantBio kalahari_block7_endast_grafer_simpel.R")
)

## Fråga 17 ----
TukeyHSD(aov(steen_log_lm))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = steen_log_lm)
# 
# $area
#               diff         lwr       upr     p adj
# fr-cga   0.7544994 -0.04252382 1.5515226 0.0690482
# np-cga   2.3893417  1.59231848 3.1863649 0.0000000
# wma-cga  1.9434138  1.14639056 2.7404370 0.0000007
# np-fr    1.6348423  0.83781909 2.4318655 0.0000174
# wma-fr   1.1889144  0.39189117 1.9859376 0.0015658
# wma-np  -0.4459279 -1.24295113 0.3510953 0.4440595

## Fråga 18 ----
plot(TukeyHSD(aov(steen_log_lm)))


## Fråga 19 ----
grass_cga <- grass[grass$area == "cga",]
hist(grass_cga$cover) # Norm. dist., heavily skewed
qqnorm(grass_cga$cover); qqline(grass_cga$cover) # not good


grass_fr <- grass[grass$area == "fr",]
hist(grass_fr$cover) # Norm. dist., heavily skewed
qqnorm(grass_fr$cover); qqline(grass_fr$cover) # not good


grass_wma <- grass[grass$area == "wma",]
hist(grass_wma$cover) # Norm. dist., heavily skewed
qqnorm(grass_wma$cover); qqline(grass_wma$cover) # not good


grass_np <- grass[grass$area == "np",]
hist(grass_np$cover) # Norm. dist., heavily skewed
qqnorm(grass_np$cover); qqline(grass_np$cover) # not good


grass_aov <- lm(
  cover ~ area,
  data = grass
)
par(mfrow = c(2,2))
plot(grass_aov)
par(mfrow = c(1,1))


## Fråga 20 ----
grass$log_cover <- log(grass$cover + 1)


grass_cga <- grass[grass$area == "cga",]
hist(grass_cga$log_cover) # Norm. dist., heavily skewed
qqnorm(grass_cga$log_cover); qqline(grass_cga$log_cover) # could be better


grass_fr <- grass[grass$area == "fr",]
hist(grass_fr$log_cover) # Norm. dist., heavily skewed
qqnorm(grass_fr$log_cover); qqline(grass_fr$log_cover) # not great


grass_wma <- grass[grass$area == "wma",]
hist(grass_wma$log_cover) # Norm. dist., heavily skewed & sloping
qqnorm(grass_wma$log_cover); qqline(grass_wma$log_cover) # decent


grass_np <- grass[grass$area == "np",]
hist(grass_np$log_cover) # Norm. dist., a little uneven
qqnorm(grass_np$log_cover); qqline(grass_np$log_cover) # decent


grass_aov <- lm(
  log_cover ~ area,
  data = grass
)
par(mfrow = c(2,2))
plot(grass_aov)
par(mfrow = c(1,1))

## Fråga 21 ----
kruskal.test(grass$cover ~ grass$area)



# Quiz 4 ----
