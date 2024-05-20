# Ersättningsprojekt för Klubban

library(tidyverse)
library(rstatix)
library(ggpubr)
library(googlesheets4)

# Read data from google sheets
  ## gs4_auth() # For signing in to get access
df <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Paig6TZZLAFfF86WBTSRWvn7nUHvwh9G0yNVeQWN-DU/edit#gid=0",
  range = "Data!A:F",
  col_names = TRUE,
  col_types = "cciccc")


# Coerce Tid to ordered factor (Morgon, Dag, Kväll)
df$Tid <- factor(df$Tid,
                 levels = c("Morgon", "Dag", "Kväll"),
                 ordered = TRUE)
# Coerce Punkt to ordered factor (1-10)
df$Punkt <- factor(df$Punkt,
                   levels = as.character(1:10),
                   ordered = TRUE)
# Sort by Dag, Tid, Punkt, Art
df <- arrange(df, Dag, Tid, Punkt, Art)


# Shannon ----
## not paired t-test ----
# Sum counts from points (technical replicates)
df_antal <- df %>% 
  summarise(Antal_sum = sum(Antal),
            .by = c(Art, Tid, Dag, Punkt))


# Calculate biodiversity index
  ## P = relative abundance
  ## S = number of species
df_index <- df_antal %>% 
  mutate(rel_ab = Antal_sum/sum(Antal_sum),
         S = length(unique(Art)),
         .by = c(Tid, Dag, Punkt))

df_Shannon <- df_index %>% 
  summarise(Shannon = -sum(rel_ab * log(rel_ab)),
            .by = c(Tid, Dag, Punkt))

df_Simpson <- df_index %>% 
  summarise(Simpson = 1/sum(rel_ab^2),
            .by = c(Tid, Dag, Punkt))


# Parade parvisa t-test av Shannon index mellan tid på dygnet
test_Shannon <- df_Shannon %>% 
  # group_by(Dag) %>%  ## This makes comparisons within days
  pairwise_t_test(Shannon ~ Tid,
                  paired = F, ### Ändra till TRUE ###
                  p.adjust.method = "holm") %>% 
  add_xy_position(fun = "mean_sd",
                  x = "Tid")


# Plot Shannon index t-test (mean & sd)
ggbarplot(df_Shannon,
          x = "Tid",
          y = "Shannon",
          fill = "Dag",
          position = position_dodge(.8), # position_dodge2(.8) gives bars by points
          add = "mean_sd", # can't combine with dodge2
          
          palette = "Dark2",
          xlab = FALSE,
          ylab = "Shannon's index",
          legend = "bottom") +
  stat_pvalue_manual(test_Shannon,
                     label = "p = {p.adj}",
                     tip.length = .01,
                     step.increase = .03,
                     bracket.nudge.y = .3) 

# Plot Shannon index t-test (individual points)
ggbarplot(df_Shannon,
          x = "Tid",
          y = "Shannon",
          fill = "Dag",
          position = position_dodge2(.8), # position_dodge2(.8) gives bars by points
          # add = "mean_sd", # can't combine with dodge2
          
          palette = "Dark2",
          xlab = FALSE,
          ylab = "Shannon's index",
          legend = "bottom") +
  stat_pvalue_manual(test_Shannon,
                     label = "p = {p.adj}",
                     tip.length = .01,
                     step.increase = .04,
                     bracket.nudge.y = .2)


## Paired t-test ----
# Sum counts from points (technical replicates)
df_antal_paired <- df %>% 
  summarise(Antal_sum = sum(Antal),
            .by = c(Art, Tid, Dag))


# Calculate biodiversity index
## P = relative abundance
## S = number of species
df_index_paired <- df_antal_paired %>% 
  mutate(rel_ab = Antal_sum/sum(Antal_sum),
         S = length(unique(Art)),
         .by = c(Tid, Dag))

df_Shannon_paired <- df_index_paired %>% 
  summarise(Shannon = -sum(rel_ab * log(rel_ab)),
            .by = c(Tid, Dag))

df_Simpson_paired <- df_index_paired %>% 
  summarise(Simpson = 1/sum(rel_ab^2),
            .by = c(Tid, Dag))


### ANOVA & residualanalys ----
# Outliers
df_Shannon_paired %>% 
  group_by(Tid) %>%
  identify_outliers(Shannon)
# Normality
df_Shannon_paired %>% 
  group_by(Tid) %>% 
  shapiro_test(Shannon)

df_Shannon_paired %>% 
  ggqqplot("Shannon",
           facet.by = "Tid")
# ANOVA & residuals
aov_shannon <- anova_test(data = df_Shannon_paired,
                          dv = Shannon,
                          wid = Tid,
                          within = Dag)
get_anova_table(aov_shannon)

### Parade parvisa t-test
  #   av Shannon index mellan tid på dygnet ----
test_Shannon_paired <- df_Shannon_paired %>% 
  # group_by(Dag) %>%  ## This makes comparisons within days
  pairwise_t_test(Shannon ~ Tid,
                  paired = T, ### Ändra till TRUE ###
                  p.adjust.method = "holm") %>% 
  add_xy_position(fun = "mean_sd",
                  x = "Tid")


# Plot Shannon index t-test (mean)
ggbarplot(df_Shannon_paired,
          x = "Tid",
          y = "Shannon",
          fill = "Dag",
          position = position_dodge2(.8),

          palette = "Dark2",
          xlab = FALSE,
          ylab = "Shannon's index",
          legend = "bottom") +
  stat_pvalue_manual(test_Shannon,
                     label = "p = {p.adj}",
                     tip.length = .01,
                     step.increase = .04,
                     bracket.nudge.y = .5)

### Averaged across days plot ----
ggbarplot(df_Shannon_paired,
          x = "Tid",
          y = "Shannon",
          fill = "Tid",
          palette = "Dark2",
          add = "mean_sd",
          xlab = FALSE,
          ylab = "Shannon's index",
          legend = "none") +
  stat_pvalue_manual(test_Shannon_paired,
                     label = "p = {p.adj}",
                     tip.length = .01,
                     step.increase = .03)


# Antal arter ----
df_arter <- df %>% 
  select(-Person,
         -Antal) %>% 
  count(Dag, Tid, Punkt,
        name = "Arter")

### T-test ----
test_arter <- df_arter %>% 
  pairwise_t_test(Arter ~ Tid,
                  paired = F,
                  p.adjust.method = "holm") %>% 
  add_xy_position(fun = "mean_sd",
                  x = "Tid")

# Plot antal arter (mean & sd)
ggbarplot(df_arter,
          x = "Tid",
          y = "Arter",
          fill = "Dag",
          position = position_dodge(.8),
          add = "mean_sd",
          
          palette = "Dark2",
          xlab = FALSE,
          ylab = "Antal arter",
          legend = "bottom") +
  stat_pvalue_manual(test_arter,
                     label = "p = {p.adj}",
                     tip.length = .01,
                     bracket.nudge.y = .9)

# Plot antal arter (individual points)
ggbarplot(df_arter,
          x = "Tid",
          y = "Arter",
          fill = "Dag",
          position = position_dodge2(.8),
          
          palette = "Dark2",
          xlab = FALSE,
          ylab = "Antal arter",
          legend = "bottom") +
  stat_pvalue_manual(test_arter,
                     label = "p = {p.adj}",
                     tip.length = .01,
                     bracket.nudge.y = 3)


## Paired t-test ----
df_arter_paired <- df %>% 
  select(-Person,
         -Antal) %>% 
  summarise(Arter = length(unique(Art)),
            .by = c(Dag, Tid))

### ANOVA & residualanalys ----
# Outliers
df_arter_paired %>% 
  group_by(Tid) %>%
  identify_outliers(Arter)
# Normality
df_arter_paired %>% 
  group_by(Tid) %>% 
  shapiro_test(Arter)

df_arter_paired %>% 
  ggqqplot("Arter",
           facet.by = "Tid")
# ANOVA & residuals
aov_arter <- anova_test(data = df_arter_paired,
                          dv = Arter,
                          wid = Tid,
                          within = Dag)
get_anova_table(aov_arter)

### Paired t-test ----
test_arter_paired <- df_arter_paired %>% 
  pairwise_t_test(Arter ~ Tid,
                  paired = T,
                  p.adjust.method = "holm") %>% 
  add_xy_position(fun = "mean_sd",
                  x = "Tid")

ggbarplot(df_arter_paired,
          x = "Tid",
          y = "Arter",
          fill = "Tid",
          palette = "Dark2",
          legend = "none",
          add = "mean_sd",
          xlab = FALSE,
          ylab = "Antal arter") +
  stat_pvalue_manual(test_arter_paired,
                     label = "p = {p.adj}",
                     tip.length = .01)


# Subset of species ----
df_subset <- df %>% 
  filter(Art %in% c("Koltrast",
                    "Svartvit flugsnappare",
                    "Rödhake")) %>% 
  summarise(Antal_sum = sum(Antal),
            .by = c(Tid, Dag, Art))

test_antal <- df_subset %>% 
  group_by(Art) %>% 
  pairwise_t_test(Antal_sum ~ Tid,
                  paired = T,
                  p.adjust.method = "holm") %>% 
  add_xy_position(fun = "mean_sd",
                  x = "Tid")

# Plot (facet by species)
ggbarplot(df_subset,
          x = "Tid",
          y = "Antal_sum",
          fill = "Tid",
          facet.by = "Art",
          position = position_dodge(.8),
          add = "mean_sd",
          palette = "Dark2",
          legend = "none",
          xlab = FALSE,
          ylab = "Individer") +
  stat_pvalue_manual(test_antal,
                     label = "p = {p.adj}",
                     step.group.by = "Art",
                     tip.length = .01)
