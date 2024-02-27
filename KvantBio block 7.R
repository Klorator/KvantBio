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
