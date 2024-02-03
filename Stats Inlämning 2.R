# Statistik Inlämning 2

## Fråga 1

#H0: Ingen skillnad; H1: Det är skillnad
# alternativet som är tvärt om är också möjligt
#  men mindre lämpligt/vanligt.

## Fråga 2

crayfish_length <- c(11.5, 11.5, 7.4, 10.3, 7.8,
                     10.1, 13.6, 6.6, 6.5, 8.9)
crayfish_conf.int <- t.test(
  crayfish_length,
  alternative = "two.sided",
  mu = 0, # mu = mean(crayfish_length) gave same result
  conf.level = 0.99
)$conf.int
crayfish_conf.int # Lower = 6.97803

# Kanske ska beräkna för hand också?

## Fråga 3

group1 <- c(16.3, 16.6,	18.3,	18.5,	13.2,	
            17.6,	16.7,	15.9)
group2 <- c(8.9, 9.3, 9.6, 15.1, 8.6, 
            10.9, 15.7, 13.4)
group_analysis <- t.test(
  group1,
  group2,
  alternative = "two.sided",
  conf.level = 0.95,
  paired = T # !!!
)
group_p <- group_analysis$p.value # p = 0.001028

## Fråga 4
  # Shitty study design...

winner <- c(3.5, 4.0, 3.2, 3.3, 3.2, 
            2.6, 3.5, 3.8)
looser <- c(3.7, 3.3, 3.4, 2.8, 2.8, 
            2.7, 3.3, 2.2)

combat_analysis <- t.test(
  winner,
  looser,
  alternative = "t",
  conf.level = 0.95
)
combat_analysis
  # p = 0.1341 => not significant