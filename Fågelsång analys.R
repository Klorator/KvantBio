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
            .by = c(Art, Tid, Dag))


# Calculate biodiversity index
  ## P = relative abundance
  ## S = number of species
df_index <- df_antal %>% 
  mutate(rel_ab = Antal_sum/sum(Antal_sum),
         S = length(unique(Art)),
         .by = c(Tid, Dag))

df_Shannon <- df_index %>% 
  summarise(Shannon = -sum(rel_ab * log(rel_ab)),
            .by = c(Tid, Dag)) %>% 
  arrange(Dag, Tid)

df_Simpson <- df_index %>% 
  summarise(Simpson = 1/sum(rel_ab^2),
            .by = c(Tid, Dag)) %>% 
  arrange(Dag, Tid)


# Parade parvisa t-test av Shannon index mellan tid på dygnet
test_Shannon <- df_Shannon %>% 
  pairwise_t_test(Shannon ~ Tid,
                  paired = F, ### Ändra till TRUE ###
                  p.adjust.method = "holm") %>% 
  add_xy_position(fun = "mean_sd",
                  x = "Tid")


# Plot Shannon index t-test
df_Shannon %>% 
  ggplot() +
  aes(x = Tid,
      y = Shannon,
      fill = Dag) +
  theme_classic() +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  # stat_pvalue_manual(test_Shannon,
  #                    label = "p = {p.adj}") +
  labs(x = "",
       y = "Shannon's index",
       fill = "Datum")








# plot_antal <- 
  df_plot %>% 
  ggplot() +
  aes(x = Tid,
      y = antal_mean,
      fill = Tid) +
  theme_classic() +
  geom_col(#position = "dodge"
           ) +
    facet_wrap(~Art) +
  scale_fill_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymax = antal_mean + antal_sd,
                    ymin = antal_mean - antal_sd),
                width = .2,
                position = position_dodge(.9)) +
  stat_pvalue_manual(test_antal,
                     label = "p = {p.adj}",
                     tip.length = .1) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_blank()) +
  labs(x = "Tid på dygnet",
       y = "Antal individer")

# plot_antal
