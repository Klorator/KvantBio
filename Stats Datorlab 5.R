# Statistik datorlabb 5: mer om ANOVA % regression

## Data ----
ecoli <- read.table(
  file = file.path("Data ecoli.csv"),
  header = T,
  sep = ",",
  dec = "."
)


## Fråga 1 ----
data1 <- data.frame(y = c(10.00, 10.21, 10.59, 11.08, 11.09, 11.10, 11.18, 11.30, 11.52, 11.86, 11.95, 12.08, 12.16, 12.33, 12.43, 12.51, 12.77, 13.05, 13.30, 14.03),
                    x1 = rep(c("A", "B"), c(10, 10)),
                    x2 = rep(c("a", "b"), 10))

data2 <- data.frame(y = c(10.00, 10.21, 10.59, 11.08, 11.09, 11.10, 11.18, 11.30, 11.52, 11.86, 11.95, 12.08, 12.16, 12.33, 12.43, 12.51, 12.77, 13.05, 13.30, 14.03),
                    x1 = rep(c("A", "B", "B", "A"), c(5, 5, 5, 5)),
                    x2 = rep(c("a", "b"), 10))

data3 <- data.frame(y = c(10.00, 10.21, 10.59, 11.08, 11.09, 11.10, 11.18, 11.30, 11.52, 11.86, 11.95, 12.08, 12.16, 12.33, 12.43, 12.51, 12.77, 13.05, 13.30, 14.03),
                    x1 = rep(c("A", "B", "A", "B"), c(5, 5, 5, 5)),
                    x2 = rep(c("a", "b"), c(10, 10)))

interaction.plot(data1$x1, data1$x2, response = data1$y)
interaction.plot(data2$x1, data2$x2, response = data2$y)
interaction.plot(data3$x1, data3$x2, response = data3$y)


## Fråga 2 ----
interaction.plot(
  ecoli$Medium,
  ecoli$Stam,
  ecoli$Dubbleringstid
)


## Fråga 3 ----
ecoli_aov <- aov(
  Dubbleringstid ~ Stam * Medium,
  data = ecoli
)
anova(ecoli_aov)
# Analysis of Variance Table
# 
# Response: Dubbleringstid
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# Stam         4  8.6470  2.1618  6.0380  0.001093 ** 
#   Medium       1 16.8410 16.8410 47.0386 1.303e-07 ***
#   Stam:Medium  4  4.9156  1.2289  3.4324  0.020035 *  
#   Residuals   30 10.7408  0.3580 


## Info box ----
library(palmerpenguins)
adelie <- subset(penguins, species == "Adelie")
plot(bill_depth_mm ~ body_mass_g, data = adelie, col = adelie$sex, pch = 16)
legend(2800, 21, c("Hona", "Hane"), col = 1:2, pch =16)


## Fråga 4 ----
m <- lm(bill_depth_mm ~ body_mass_g * sex, data = adelie)
summary(m)

# The equation becomes:
# y = a + b1x1 + b2x2 + b3x1x2
#     y = bill depth
#     x1 = body mass
#     x2 = sex (transformed into 0 & 1)
#
# Where the results are then listed in the summary table in order:
#     a = intercept
#     b1 = body mass
#     b2 = sex male
#     b3 = body mass : sex male (the interaction)
#
# Coefficients:
#                       Estimate Std. Error t value
# (Intercept)         12.9408522  1.3969906   9.263
# body_mass_g          0.0013895  0.0004134   3.361
# sexmale              4.2363186  1.9103436   2.218
# body_mass_g:sexmale -0.0009208  0.0005234  -1.759
#
#
# Intercept for a male penguin is then: a + b2
intercept_male_penguin <- 12.9408522 + 4.2363186

