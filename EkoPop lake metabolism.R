# Excel replacement

library(tidyverse)

file <- file.choose()
df_lake <- read_csv2(file)

df_lake <- df_lake %>% 
  filter(`DO (mg/L)` != "failure")

df_lake$`DO (mg/L)` <- str_replace(
  df_lake$`DO (mg/L)`, ",", ".")

df_lake <- df_lake %>% 
  type_convert()

df_lake$dateTime <- as.POSIXlt(df_lake$dateTime)

df_lake <- df_lake %>% 
  group_by(`Lake ID`) %>% 
  arrange(dateTime)

lake_summary <- df_lake[1,]
if (lake_summary$`PAR (umole/sq. m/sec)`[[1]] == 0) {
  night <- F
} else night <- T

for (i in nrow(df_lake)) {
  
  if (df_lake$`PAR (umole/sq. m/sec)`[[i]] == 0 & night == F) {
    night <- T
    
  }
  
  
  if (df_lake$`PAR (umole/sq. m/sec)`[[i]] > 0 & night == T) {
    night <- F
  }
  
}