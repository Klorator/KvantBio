#### Tentamen 2020 ####

### Fråga 1 

r1 <- c(160, 3)
r2 <- c(1806, 88)
cov <- rbind(r1,r2)
colnames(cov)<-c("Konfirmerade fall", "Avlidna")
rownames(cov)<-c("Uppsala","Stockholm")
cov # 2x2 tabell med rubriker

# b)
chisq.test(cov) #Chi2 test
#Det vi får är:

#Pearson's Chi-squared test with Yates' continuity correction

#data:  cov
#X-squared = 2.1701, df = 1, p-value = 0.1407

#c) 

r1 <- c(2222, 58)
r2 <- c(1806, 88)
cov <- rbind(r1,r2)
colnames(cov)<-c("Konfirmerade fall", "Avlidna")
rownames(cov)<-c("Resten av landet", "Stockholm")
cov # 2x2 tabell med rubriker

#Rättning:
# a) Korrekta hypoteser 1 p, frekvenser/nominalskala motivering 1 p. 
# b) Oberoende chi-2 1 p, 
# c) H0 förkastas 0,5 p, vaccination har effekt 0,5 p. 

#### Fråga 2 ####
#a)

#ALbion
r1<-c( 0.94  , 0     , 0     , 14 )
r2<-c( 0     , 0.89  , 0     , 0  )
r3<-c( 0     , 0     , 0.71  , 0  )
r4<-c( 0     , 0     , 0     , 0.71 )

A <-rbind(r1,r2,r3,r4)
max(Re(eigen(A)$values)) 
#Får 0.94

#Erli
r1<-c( 0.96  , 0     , 0     , 20 )
r2<-c( 0     , 0.86  , 0     , 0  )
r3<-c( 0     , 0     , 0.71  , 0  )
r4<-c( 0     , 0     , 0     , 0.64 )
E <-rbind(r1,r2,r3,r4)
max(Re(eigen(E)$values)) 
#Får 0.96

# b) 
r1<-c( 0.94  , 0     , 0     , 2 )
r2<-c( 0     , 0.89  , 0     , 0  )
r3<-c( 0     , 0     , 0.71  , 0  )
r4<-c( 0     , 0     , 0     , 0.71 )
A<-rbind(r1,r2,r3,r4)
max(Re(eigen(A)$values)) 

#Erli
r1<-c( 0.96  , 0     , 0     , 8 )
r2<-c( 0     , 0.86  , 0     , 0  )
r3<-c( 0     , 0     , 0.71  , 0  )
r4<-c( 0     , 0     , 0     , 0.64 )
E <-rbind(r1,r2,r3,r4)
max(Re(eigen(E)$values)) 






#####Fråga 3 #####
# För att få utföra ett t.test kontrolleras först
# normaliteten med histogram och qqplot.
par(mfrow=c(2,2))
hist(toxin$control)
hist(toxin$ATG)
qqnorm(toxin$control);qqline(toxin$control)
qqnorm(toxin$ATG);qqline(toxin$ATG)

# Jag accepterar detta som approximativt normalfördelat och väljer ett parametriskt test, dvs
# t-test med två oberoende stickprov som testar om de observerade gruppernas medelvärden är
# identiska.

#Lämpligt statiskts test är t.test
t.test(toxin$control, toxin$ATG)

#### Fråga 5 ##### 


# a) Anova med Adekvata hypoteser

# b) Undersökning av normalitet och varianshomogenitet
virus$virustyp<-as.factor(virus$virustyp) #Variabeln virustyp som text ersätts med variabeln virustyp som omvandlats till faktor

modell<-lm(procent~virustyp*medel, data=virus)
par(mfrow=c(2,2)); plot(modell) #residualgraf av tvåvägs-ANOVAn

# Det verkar helt OK med normalitet och varianshomogenitet. Vi kan således gå vidare med en
# parametrisk prövning.  Vi tar fram anova-tabellen på analysen genom
anova(modell)


# Resultat av variansprövning
#                 Df  Sum Sq Mean Sq F value    Pr(>F)    
# virustyp        1  606.69  606.69 73.1015 3.291e-11 ***
# medel           2 1201.53  600.77 72.3884 3.225e-15 ***
# virustyp:medel  2   45.09   22.55  2.7166   0.07626 .  

# c) 
# Nollhypoteserna för diet och kön kan förkastas och det finns skillnader i blodtryck mellan
# dietgrupperna samt mellan könen. Vi ser dock ingen växelverkan mellan faktorerna.
# Vi kan undersöka var skillnaderna ligger med TukeyHSD och boxplot. 

par(mfrow=c(1,2)) 
boxplot(virus$procent~virus$virustyp)      
boxplot(virus$procent~virus$medel) 
par(mfrow=c(1,1)) 

boxplot(virus$procent~virus$medel*virus$virustyp) 


TukeyHSD(aov(modell)) ##### Modeller
plot(TukeyHSD(aov(modell)))  ##### Plot av Modeller

#Rättning:
# 2-vägs ANOVA 0,5 p., Korrekta hypoteser 1 p. 
# b) Residualernormalfördelade med varianshomogenitet 1 p, rätt ANOVA 0,5 p. - interaktionsplot ej 
# nödvändig men hjälpsam. 
# c) Diet påverkar blodtrycket 0,5 p, kön påverkar blodtryck 0,5,
# Ingen växelverkan mellan diet och kön beträffande blodtrycket 1 p. 





###### Fråga 7 

#Adekvata hypoteser:
#H0: Det finns inget samband mellan antalet ägg och överlevnad hos kajorna
#H1: Det finns ett samband mellan antalet ägg och överlevnad hos kajorna


#För att kunna hitta sambandet genom beräkning av korrelationskoefficienten 
#måste först en bedömning av om ingående variabler är approximativt normalfördelade
#och om sambandet är linjärt.

#Undersökning om sambandet är linjärt med en plot.
plot(bird$egg, bird$surv)

#Detta visar ett tydligt linjärt samband. 

#För undersökning om båda variablerna är approximativt normalfördeladepar(mfrow=c(2,2))
par(mfrow=c(2,2))
hist(bird$egg)
hist(bird$surv) 
qqnorm(bird$egg);qqline(bird$egg) 
qqnorm(bird$surv);qqline(bird$surv)

#b)

#Detta visade normalfördelade variabler och vi kan använda Pearsons produktmomentkorrelationskoefficient. 

cor.test(bird$egg, bird$surv, method = "spearman") #Korrelationstest
par(mfrow=c(1,1))
boxplot(bird$surv~bird$egg, xlab="Ägg", ylab="Livslängd")
cor.test(bird$egg, bird$surv, method = "spearman")$estimate^2

