# Ersättningsprojekt för Klubban

library(tidyverse)
library(rstatix)
library(ggpubr)
library(googlesheets4)

# Data ----
## Read data from google sheets----
  ## gs4_auth() # For signing in to get access
df <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Paig6TZZLAFfF86WBTSRWvn7nUHvwh9G0yNVeQWN-DU/edit#gid=0",
  range = "Data!A:F",
  col_names = TRUE,
  col_types = "cciccc")

## Cleaning ----
# Coerce Tid to ordered factor (Morgon, Dag, Kväll)
df$Tid <- factor(df$Tid,
                 levels = c("Morgon", "Dag", "Kväll"),
                 ordered = TRUE)
# Coerce Punkt to ordered factor (1-10)
df$Punkt <- factor(df$Punkt,
                   levels = as.character(1:10),
                   ordered = TRUE)
# Coerce Dag to ordered factor (Fredag, Lördag, Söndag)
df <- df %>% 
  mutate(Dag = case_when(Dag == "17" ~ "Fredag",
                         Dag == "18" ~ "Lördag",
                         Dag == "19" ~ "Söndag"))
df$Dag <- factor(df$Dag,
                 levels = c("Fredag","Lördag","Söndag"))
# Sort by Dag, Tid, Punkt, Art
df <- arrange(df, Dag, Tid, Punkt, Art)


# Shannon ----
## Calculate biodiversity index ----
  ## P = relative abundance
  ## S = number of species
df_index <- df %>% 
  mutate(rel_ab = Antal/sum(Antal),
         S = length(unique(Art)),
         .by = c(Tid, Dag, Punkt))

df_Shannon <- df_index %>% 
  summarise(Shannon = -sum(rel_ab * log(rel_ab)),
            .by = c(Tid, Dag, Punkt))

df_Simpson <- df_index %>% 
  summarise(Simpson = 1/sum(rel_ab^2),
            .by = c(Tid, Dag, Punkt))

## Repeated measures ANOVA ----
shannon_tukey <- list()
for (day in unique(df_Shannon$Dag)) {
  df_shannon_day <- df_Shannon %>% 
    filter(Dag == day)
  aov_shannon <- aov(Shannon ~ Tid,
                     data = df_shannon_day)
  print(summary(aov_shannon))
  print(TukeyHSD(aov_shannon))
  shannon_tukey[[day]] <- TukeyHSD(aov_shannon)
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
}





####################

# Repeated measures ANOVA formula
## response ~ within factor + Error(between factor)
## within factor = the comparisons of interest
## between factor = what to control for (a "paired" factor)

rep_test <- aov(Shannon ~ Tid + Error(Dag),
                data = df_Shannon)
summary(rep_test)


###################








## Post-hoc ----
test_Shannon <- df_Shannon %>% 
  group_by(Dag) %>%
  pairwise_t_test(Shannon ~ Tid,
                  paired = F,
                  p.adjust.method = "holm") %>% 
  add_xy_position(x = "Tid")

test_Shannon$p.adj[test_Shannon$Dag == "Fredag"] <- shannon_tukey[["Fredag"]][["Tid"]][,4]
test_Shannon$p.adj[test_Shannon$Dag == "Lördag"] <- shannon_tukey[["Lördag"]][["Tid"]][,4]
test_Shannon$p.adj[test_Shannon$Dag == "Söndag"] <- shannon_tukey[["Söndag"]][["Tid"]][,4]

test_Shannon$p.format <- test_Shannon$p.adj %>%
  p_format(leading.zero = F)


## Plot ----
ggbarplot(df_Shannon,
          x = "Tid",
          y = "Shannon",
          fill = "Tid",
          position = position_dodge(.8),
          add = "mean_sd",
          facet.by = "Dag",
          
          palette = "Dark2",
          xlab = FALSE,
          ylab = "Shannon's index",
          # subtitle = aov_results_shannon,
          legend = "none") +
  stat_pvalue_manual(test_Shannon,
                     label = "p = {p.format}",
                     tip.length = .01) 






# Antal arter ----
df_arter <- df %>% 
  select(-Person,
         -Antal) %>% 
  count(Dag, Tid, Punkt,
        name = "Arter")
# df_arter <- df_arter %>% 
#   mutate(ID = paste0(Tid,"_",Punkt))

## Repeated measures ANOVA ----
arter_tukey <- list()
for (day in unique(df_arter$Dag)) {
  df_arter_day <- df_arter %>% 
    filter(Dag == day)
  aov_arter <- aov(Arter ~ Tid,
                     data = df_arter_day)
  print(summary(aov_arter))
  print(TukeyHSD(aov_arter))
  arter_tukey[[day]] <- TukeyHSD(aov_arter)
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
}

# 
# aov_arter <- anova_test(data = df_arter,
#                         dv = Arter,
#                         wid = ID,
#                         within = Dag)
# 
# y <- get_anova_table(aov_arter)
# aov_results_arter <- paste0("ANOVA, F(",y$DFn,", ",y$DFd,") = ",y$`F`,", p = ",y$p,", ges = ",y$ges)

## Post-hoc ----
test_arter <- df_arter %>% 
  group_by(Dag) %>% 
  pairwise_t_test(Arter ~ Tid,
                  paired = F,
                  p.adjust.method = "holm") %>% 
  add_xy_position(x = "Tid")

test_arter$p.adj[test_arter$Dag == "Fredag"] <- arter_tukey[["Fredag"]][["Tid"]][,4]
test_arter$p.adj[test_arter$Dag == "Lördag"] <- arter_tukey[["Lördag"]][["Tid"]][,4]
test_arter$p.adj[test_arter$Dag == "Söndag"] <- arter_tukey[["Söndag"]][["Tid"]][,4]

test_arter$p.format <- test_arter$p.adj %>%
  p_format(leading.zero = F)

## Plot ----
ggbarplot(df_arter,
          x = "Tid",
          y = "Arter",
          fill = "Tid",
          facet.by = "Dag",
          position = position_dodge(.8),
          add = "mean_sd",
          
          palette = "Dark2",
          xlab = FALSE,
          ylab = "Antal arter",
          # subtitle = aov_results_arter,
          legend = "none") +
  stat_pvalue_manual(test_arter,
                     label = "p = {p.format}",
                     tip.length = .01,
                     bracket.nudge.y = .9)








# Subset of species ----
hot_species <- c("Koltrast",
                 "Bofink",
                 "Rödhake")
plots <- list()

for (species in hot_species) {
  ## Subset dataframe ----
  df_subset <- df %>% 
    filter(Art == species)
  # df_subset <- df_subset %>% 
  #   mutate(ID = paste0(Tid,"_",Punkt))
  
  ## Repeated measures ANOVA ----
  subset_tukey <- list()
  for (day in unique(df_subset$Dag)) {
    df_subset_day <- df_arter %>% 
      filter(Dag == day)
    aov_subset <- aov(Arter ~ Tid,
                     data = df_subset_day)
    print(paste("++++++++++++++++++++++++++++++++++++++++",species,"-",day,"++++++++++++++"))
    print(summary(aov_subset))
    print(TukeyHSD(aov_subset))
    subset_tukey[[day]] <- TukeyHSD(aov_subset)
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  }
  # aov_species <- anova_test(data = df_subset,
  #                           dv = Antal,
  #                           wid = ID,
  #                           within = Dag)
  # 
  # z <- get_anova_table(aov_species)
  # aov_results_species <- paste0("ANOVA, F(",z$DFn,", ",z$DFd,") = ",z$`F`,", p = ",z$p,", ges = ",z$ges)
  
  
  ## Post-hoc ----
  test_antal <- df_subset %>% 
    group_by(Dag) %>% 
    pairwise_t_test(Antal ~ Tid,
                    paired = F,
                    p.adjust.method = "holm") %>% 
    add_xy_position(x = "Tid")
  
  test_antal$p.adj[test_antal$Dag == "Fredag"] <- subset_tukey[["Fredag"]][["Tid"]][,4]
  test_antal$p.adj[test_antal$Dag == "Lördag"] <- subset_tukey[["Lördag"]][["Tid"]][,4]
  test_antal$p.adj[test_antal$Dag == "Söndag"] <- subset_tukey[["Söndag"]][["Tid"]][,4]
  
  test_antal$p.format <- test_antal$p.adj %>%
    p_format(leading.zero = F)
  
  ## Plot ----
  plots[[species]] <- df_subset %>% 
    ggbarplot(
      x = "Tid",
      y = "Antal",
      fill = "Tid",
      facet.by = c("Dag"),
      position = position_dodge(.8),
      add = "mean_sd",
      palette = "Dark2",
      legend = "none",
      xlab = FALSE,
      ylab = paste0("Antal individer [",species,"]")) +
    stat_pvalue_manual(test_antal,
                       label = "p = {p.format}",
                       tip.length = .01)
}
plots
