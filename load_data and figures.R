# Läsa in & bearbeta data i R ----

## Load data ----
# library(readxl)
Data_diabetes <- readxl::read_excel("Data_diabetes.xlsx")
# View(Data_diabetes)
diabetes <- Data_diabetes

## Omvandla till SI enheter ----
### Height from inches to cm
diabetes$height <- diabetes$height * 2.54

### Weight pounds to kg
diabetes$weight <- diabetes$weight / 2.2

### waist & hip inches to cm
diabetes$waist <- diabetes$waist * 2.54
diabetes$hip <- diabetes$hip * 2.54

## Calculate BMI = weight [kg] / (height [m])^2 ----
diabetes$BMI <- diabetes$weight / (diabetes$height/100)^2

### Check that BMI is within 10-58
range(diabetes$BMI)

## Finding stats variable practice ----
mean(diabetes$height)
median(diabetes$weight)
sd(diabetes$stab.glu)

## Calculate SEM = sd(x)/sqrt(length(x)) ----
sd(diabetes$waist) / sqrt(length(diabetes$waist))

## Check binomial distribution ----
table(diabetes$location, diabetes$gender)



# Skapa & snygga till figurer ----
## Histogram ----
hist(diabetes$height)    # Normal dist.
hist(diabetes$age)       # Normal dist. -ish
hist(diabetes$stab.glu)  # Not normal !
hist(diabetes$bp.1s)     # Normal dist.
hist(diabetes$waist)     # Normal dist. ?

## Scatterplot x = height, y = weight ----
plot(x = diabetes$height,
     y = diabetes$weight)

## Scatterplot x = waist, y = hip ----
plot(x = diabetes$waist,
     y = diabetes$hip)

## Boxplot hip ~ location ----
boxplot(diabetes$hip ~ diabetes$location)

## Calculate SEM = sd(x)/sqrt(length(x)) for mean weight ----
### Pull vectors for weight by male & female
weight_m <- diabetes$weight[diabetes$gender == "male"]
weight_f <- diabetes$weight[diabetes$gender == "female"]

### mean weight for males & females
mv_weight_m <- mean(weight_m)
mv_weight_f <- mean(weight_f)
mv_weight <- c(mv_weight_m, mv_weight_f)

### SD
sd_weight_m <- sd(weight_m)
sd_weight_f <- sd(weight_f)
sd_weight <- c(sd_weight_m, sd_weight_f)

### SEM
sem_weight_m <- sd_weight_m / sqrt(length(weight_m))
sem_weight_f <- sd_weight_f / sqrt(length(weight_f))
sem_weight <- c(sd_weight_m, sd_weight_f)

## Error plot using plotrix ----
plotrix::plotCI(x = c(1,2),
                y = mv_weight,
                uiw = sem_weight,
                xlim = c(0,3))

# More plots, for fun ----
## Scatterplot x = stable glukose, y = systolic bp ----
### Plot male
plot(x = diabetes$stab.glu[diabetes$gender == "male"],
     y = diabetes$bp.1s[diabetes$gender == "male"],
     col = "blue",
     pch = 19,
     xlab = "Stabilt glukos",
     ylab = "Systoliskt blodtryck")
### Add points for female
points(x = diabetes$stab.glu[diabetes$gender == "female"],
       y = diabetes$bp.1s[diabetes$gender == "female"],
       col = "darkgreen",
       pch = 19)
### Add legend
legend(x = 300,
       y = 200,
       legend = c("Male","Female"),
       # fill = c("blue", "darkgreen"),
       col = c("blue", "darkgreen"),
       pch = c(19, 19),
       bty = "n")
## Error plot for location ~ hip ----
### SEM for hip
hip_B <- diabetes$hip[diabetes$location == "Buckingham"]
hip_L <- diabetes$hip[diabetes$location == "Louisa"]

mv_hip_B <- mean(hip_B)
mv_hip_L <- mean(hip_L)
mv_hip <- c(mv_hip_B, mv_hip_L)

sem_hip_B <- sd(hip_B) / sqrt(length(hip_B))
sem_hip_L <- sd(hip_L) / sqrt(length(hip_L))
sem_hip <- c(sem_hip_B, sem_hip_L)

### Error plot
plotrix::plotCI(x = c(1,2),
                y = mv_hip,
                uiw = sem_hip,
                xlim = c(0,3),
                xlab = "Stad",
                ylab = "Höftmått",
                xaxt = "n")
axis(side = 1,
     at = c(1,2),
     labels = c("Buckingham", "Louisa"))













