# Basic intro

1*2*3*4*5*6*7*8*9*10

a <- 924+124
a*a


height <- c(166, 185, 155, 167, 120)
weight <- c(45, 86, 120, 98, 88)
df <- data.frame(height, weight)

x <- c(1:5, 4:1)

mean(df$height)

sqrt(pi)


library(ggplot2)

table(msleep$vore, msleep$conservation)

proportions(table(msleep$vore, msleep$conservation),
            margin = 1)

proportions(table(msleep$vore, msleep$conservation),
            margin = 2)

aggregate(bodywt ~ vore,
          data = msleep,
          FUN = mean)
