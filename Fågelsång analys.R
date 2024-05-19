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


# Plot Shannon index t-test
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
                     bracket.nudge.y = .3
                     ) 
  # scale_y_continuous(expand = expansion(mult = c(.05, .1)))



# Antal arter
df_arter <- df %>% 
  select(-Person,
         -Antal) %>% 
  count(Dag, Tid, Punkt,
        name = "Arter")

test_arter <- df_arter %>% 
  pairwise_t_test(Arter ~ Tid,
                  paired = F,
                  p.adjust.method = "holm") %>% 
  add_xy_position(fun = "mean_sd",
                  x = "Tid")
