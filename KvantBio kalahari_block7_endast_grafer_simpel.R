# Detta skript ?r endast t?nkt att vara hj?lp till delen som ritar graferna till uppgiften.
# Graferna som ritas h?r anv?nder endast enkla kommandon,
# och d?rf?r blir ber?kningarna lite l?ngre ?n vad man kan g?ra med mer avancerade kommandon eller loopar.

# F?r sj?lva testen ges inget skript, detta ?r endast ett grafiskt komplement.

library(plotrix)  
par(mfrow = c(1,2)) # Get 2 plots side by side

# OBS! Detta skript kr?ver att dataframen steenbok ?r laddad (finns i kalahari_block7.RData).
#steenbok<-read.csv2(file.choose()) # alternativ laddning av CSV

# logtransform av individer per km
steenbok$log_ipkm<-log(steenbok$ind_per_km)

# Ber?kna medelv?rde och standardavvikelse
# dessa ber?kningar ?r bara gjorda med klassisk klipp och klistra, och lite ers?ttningar

medel_fr  <-mean(steenbok$log_ipkm[steenbok$area=="fr"])
medel_cga <-mean(steenbok$log_ipkm[steenbok$area=="cga"])
medel_wma <-mean(steenbok$log_ipkm[steenbok$area=="wma"])
medel_np  <-mean(steenbok$log_ipkm[steenbok$area=="np"])
medel<-c(medel_fr,medel_cga,medel_wma,medel_np)

stdev_fr  <-sd(steenbok$log_ipkm[steenbok$area=="fr"])
stdev_cga <-sd(steenbok$log_ipkm[steenbok$area=="cga"])
stdev_wma <-sd(steenbok$log_ipkm[steenbok$area=="wma"])
stdev_np  <-sd(steenbok$log_ipkm[steenbok$area=="np"])
stdev<-c(stdev_fr,stdev_cga,stdev_wma,stdev_np)


# Plotta graf som illustration till env?gs ANOVA

plotCI(x = 1:4,
       y=medel,stdev, 
       xlab="Markanv?ndningstyp", 
       ylab="log (individer/km)",
       bty="n",                           # s?tter boxtype till "none", det vill s?ga ingen ruta runt grafytan
       xaxt="n"                           # s?tter x-axis type till "none", det vill s?ga ingen synlig x-axel
)

namn<-c("Fenced Ranches","Communal Grazing Areas","Wildlife Management Areas","National Park")
axis(1, at=1:4, labels=namn)



# Ber?kningar av medel och standardavvikelse f?r tv?v?gs ANOVA
# dessa ber?kningar ?r bara gjorda med klassisk klipp och klistra, och lite ers?ttningar

medel_fr_dag  <-mean(steenbok$log_ipkm[steenbok$area=="fr" & steenbok$tid=="dag"])
medel_cga_dag <-mean(steenbok$log_ipkm[steenbok$area=="cga" & steenbok$tid=="dag"])
medel_wma_dag <-mean(steenbok$log_ipkm[steenbok$area=="wma" & steenbok$tid=="dag"])
medel_np_dag  <-mean(steenbok$log_ipkm[steenbok$area=="np" & steenbok$tid=="dag"])

medel_fr_natt  <-mean(steenbok$log_ipkm[steenbok$area=="fr" & steenbok$tid=="natt"])
medel_cga_natt <-mean(steenbok$log_ipkm[steenbok$area=="cga" & steenbok$tid=="natt"])
medel_wma_natt <-mean(steenbok$log_ipkm[steenbok$area=="wma" & steenbok$tid=="natt"])
medel_np_natt  <-mean(steenbok$log_ipkm[steenbok$area=="np" & steenbok$tid=="natt"])

medel<-c(medel_fr_dag,
         medel_fr_natt,
         medel_cga_dag,
         medel_cga_natt,
         medel_wma_dag,
         medel_wma_natt,
         medel_np_dag,
         medel_np_natt
)

stdev_fr_dag  <-sd(steenbok$log_ipkm[steenbok$area=="fr" & steenbok$tid=="dag"])
stdev_cga_dag <-sd(steenbok$log_ipkm[steenbok$area=="cga" & steenbok$tid=="dag"])
stdev_wma_dag <-sd(steenbok$log_ipkm[steenbok$area=="wma" & steenbok$tid=="dag"])
stdev_np_dag  <-sd(steenbok$log_ipkm[steenbok$area=="np" & steenbok$tid=="dag"])

stdev_fr_natt  <-sd(steenbok$log_ipkm[steenbok$area=="fr" & steenbok$tid=="natt"])
stdev_cga_natt <-sd(steenbok$log_ipkm[steenbok$area=="cga" & steenbok$tid=="natt"])
stdev_wma_natt <-sd(steenbok$log_ipkm[steenbok$area=="wma" & steenbok$tid=="natt"])
stdev_np_natt  <-sd(steenbok$log_ipkm[steenbok$area=="np" & steenbok$tid=="natt"])

stdev<-c(stdev_fr_dag,
         stdev_fr_natt,
         stdev_cga_dag,
         stdev_cga_natt,
         stdev_wma_dag,
         stdev_wma_natt,
         stdev_np_dag,
         stdev_np_natt
)


# Plotta grupperna som illustration av resultat till tv?v?gs ANOVA

plotCI(x = 1:8,
       y=medel,
       stdev, 
       xlab="Markanv?ndningstyp", 
       ylab="log (individer/km)",
       bty="n",                           # s?tter boxtype till "none", det vill s?ga ingen ruta runt grafytan
       xaxt="n"                           # s?tter x-axis type till "none", det vill s?ga ingen synlig x-axel
)
namn<-c("FR Dag","FR Natt","CGA Dag","CGA Natt","WMA Dag","WMA Natt","NP Dag","NP Natt")
axis(1, at=1:8, labels=namn)

par(mfrow = c(1,1)) # Reset to 1 plot