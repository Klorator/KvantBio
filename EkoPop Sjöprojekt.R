# Analys av Ljungsjöarna

library(tidyverse)
library(readxl)

# Data ----
df_Vattenkemi <- read_excel(
  path = "./EkoPop Ljungsjöarna data/slu_mvm_240527_163717437_data.xlsx",
  sheet = "Mätvärden Vattenkemi"
)
## columns of interest
## Provtagningsår, Provtagningsmånad, Provtagningsdag, 
## Kvalitetsflagga, Parameter, Enhet, Värde/Koncentration
df_Vattenkemi2 <- df_Vattenkemi %>% 
  dplyr::select(Provplats,
                Provdatum,
                Provtagningsår,
                Provtagningsmånad,
                Kvalitetsflagga,
                Parameter,
                Enhet,
                `Värde/Koncentration`,
                `Kvantifieringsgräns/rapporteringsgräns`,
                Mätosäkerhet) %>% 
  mutate(Tid = paste0(Provtagningsår,"-",Provtagningsmånad))

df_PlanktonPlant <- read_excel(
  path = "./EkoPop Ljungsjöarna data/slu_mvm_240527_163717437_data.xlsx",
  sheet = "Mätvärden Växtplankton"
)

df_PlanktonAnimal <- read_excel(
  path = "./EkoPop Ljungsjöarna data/slu_mvm_240527_163717437_data.xlsx",
  sheet = "Mätvärden Djurplankton"
)
df_Biofouling <- read_excel(
  path = "./EkoPop Ljungsjöarna data/slu_mvm_240527_163717437_data.xlsx",
  sheet = "Mätvärden Påväxtalger"
)
df_Index <- read_excel(
  path = "./EkoPop Ljungsjöarna data/slu_mvm_240527_163717437_data.xlsx",
  sheet = "Indexberäkningar"
)



# Plot ----

## Vattenkemi ----
df_Vattenkemi2 %>% 
  ggplot() +
  aes(x = Tid,
      y = `Värde/Koncentration`) +
  theme_classic() +
  geom_line() +
  facet_wrap(~Parameter)
