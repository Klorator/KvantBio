# KvantBio tenta 2021

birds <- rbind(
  has_bird = c(cancer = 98, control = 101),
  no_bird = c(         141,           328)
)

# a)
# Independent data.
# Because we have count data we do chi-squared.
# No cells with low counts, i.e. probably no cell with
# less than 5 in expected value.

# b)
# H0 = No difference between groups, 
# H1 = 

chisq.test(birds)
# Pearson's Chi-squared test with Yates' continuity correction
# x^2 = 21.54
# df = 1
# p-value = 3.452e-06  Significant

