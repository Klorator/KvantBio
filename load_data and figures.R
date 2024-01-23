# Läsa in & bearbeta data i R

## Load data
# library(readxl)
Data_diabetes <- readxl::read_excel("Data_diabetes.xlsx")
# View(Data_diabetes)
diabetes <- Data_diabetes

## Omvandla till SI enheter
### Height from inches to cm
diabetes$height <- diabetes$height * 2.54

### Weight pounds to kg
diabetes$weight <- diabetes$weight / 2.2

### waist & hip inches to cm
diabetes$waist <- diabetes$waist * 2.54
diabetes$hip <- diabetes$hip * 2.54

## Calculate BMI = weight [kg] / (height [m])^2
diabetes$BMI <- diabetes$weight / (diabetes$height/100)^2

### Check that BMI is within 10-58
range(diabetes$BMI)

## Finding stats variable practice
mean(diabetes$height)
median(diabetes$weight)
sd(diabetes$stab.glu)

## Calculate SEM = sd(x)/sqrt(length(x))
sd(diabetes$waist) / sqrt(length(diabetes$waist))

## Check binomial distribution
table(diabetes$location, diabetes$gender)



# Skapa & snygga till figurer
## Histogram
hist(diabetes$height)    # Normal dist.
hist(diabetes$age)       # Normal dist. -ish
hist(diabetes$stab.glu)  # Not normal !
hist(diabetes$bp.1s)     # Normal dist.
hist(diabetes$waist)     # Normal dist. ?

## Scatterplot x = height, y = weight
plot(x = diabetes$height,
     y = diabetes$weight)


