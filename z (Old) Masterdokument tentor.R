#### ============================================================================ ####
#                   MASTERDOKUMENT AV TENTAMEN & TENTAMENSÖVNINGAR
#### ============================================================================ ####



#### ============================================================================ ####
#                                 TENTAMEN 2020
#### ============================================================================ ####
#### ============================================================================ ####
#                   FRÅGA 1 - Icke parametriskt korrelationstest
#### ============================================================================ ####
#a) Adekvata hypoteser
#Nollhypotes H0: Det finns inte ett samband mellan individernas bakteriediversitet som återfanns 
#                i deras avföring och individernas vikt.
#Alternativ hypotes H1: Det finns inte samband mellan bakteriediversitet som återfanns i personernas 
#                avföring och individernas vikt.


#b) Undersökning om samband
# Eftersom vi vill undersöka ett matematiskt samband finns mellan två variabler, 
# samt hur starkt sambandet är (vilken förklaringsgrad det har) använder jag mig av Spearman. 
# Vikten är på kvotskala medan bakteriediversitet är på ordinalskala med 5 nivåer.
# Vi kan därför inte använda det parametriska Pearsons korrelationtest, eller ett regressiontest (som antar att värden finns mellan våra ranker). 
# Diversiteten är alltså rangordnad och vi ser att det måste vara ett test för rang. 
# Vi ska analysera samband utan anledning att tro att den ena faktorn styr den andra. 
# Vi väljer därför korrelation och Spearmans metod. 

#Icke parametriskt korrelationstest
x <-fek$vikt #Vikten sparas i variabeln x
y <-fek$div #divisionen sparasi variabeln y

cor.test(x, y, method = "spearman") #Korrelationstest
#Svaret blir:
#  Spearman's rank correlation rho

#data:  x and y
#S = 1408, p-value = 0.8059
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#        rho 
#-0.05868087

#P>0.05 och därför antar vi att det inte finns ett samband som isåfall kan förklaras av slumpen. Figur: 
plot(fek$vikt,fek$div) 
#Både figur och test antyder att det inte finns något samband och vi behåller därför H0. 
#Från rho -0.05868087 kan vi se att sambandet är negativt. 

boxplot(x~y, xlab="Bakteriediversitet", ylab="vikt")

#c) Förklaringsgraden
#För att få förklaringsgraden extraheras rho och kvadrerars

cor.test(x,y, method = "spearman")$estimate^2

#vilket ger svaret 0.0034, dvs 0.34 %. 

#Rättning
# a) 0.5 p för hypotes om samband, 0.5 p för korrekt H0=inget samband, H1=samband. 
# Avdrag: ‐0.5 p om man anger orsakssammanhang (antar regression), tar med medelvärden i 
#hypoteserna (tex u=u), saknar ena hypotesen eller har med irrelevant information om hypoteserna. 

#b) figur för linjärt samband 0.5 p, resonemang om ordinal skala 0.5 p, val av Spearman 0.5 p, 
#korrekt redovisning av p‐värde 0.5 p. 
# Avdrag: ‐0.5 p vardera om saknar plot, saknar motivering för Spearman, 
#Pearson istället för Spearman, inget eller felaktigt p‐värde/tolkning relativt hypoteser 
#eller felaktig/irrelevant ’garderingsinformation’ som inte är klart motiverad. 

# c) rho 0.5 p, korrekt rho^2 0.5 p. (eller Pearson r), 
# Avdrag: -0.5 p för fel % eller ingen %

#### ============================================================================ ####
#                      FRÅGA 2 - Diskret logistisk tillväxt
#### ============================================================================ ####

#a) Populationsutveckling

# Vi får i uppgiften veta att populationen av gräshoppor tillväxer enligt den diskreta logistiska modellen. 
# Vi löser denna uppgift i R. Först skapar vi en vektor till populationsstorleken som vi kallar n och 
# sätter sedan in antalet gräshoppor som finns vid tiden 1, samt sätter bärförmågan k till 50 000. 
# Vi låter sedan en for‐loop beräkna populationens utveckling över kommande 25 månader och 
# lagrar det i variabeln N. 

#Funktionsdefinition med namnet dlt
dlt <- function(n,r,k) { #Definerar funkitonen med tre variabler som argument
  n_ny <- n + r*n*(1-n/k) #Diskret logistiskt tillväxt
  return(n_ny) #Retunerar värdet till den anropande funktionen
}

#Defenition av startvärden
n<-c()
n[1]<-200 #Startvärde för n
r<-3.1 #Maximal perkapitareproduktion
k<-50000 #Bärkraft, Ekosystem med en bärkraft för 50 000 individer.
sluttid<-25 #Vi vill följa dem under 25 månader framöver

#Forloop för de nästa 25 månaderna
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av n mot t
plot(t,n, type="l", #Type=l för en linje ist för prickar
     lwd=2, #Lwd står för line width
     ylim=c(0,65000), #Ett spann på yt värdet mellan 0-65 000
     xlab='Generationer', 
     ylab='Populationsstorlek')

# Deras populationsutveckling ser ut att variera mellan ca 10 000 och lite över 60 000 individer 
# under spannet på 20 månader. Och vid 20 månader går antalet till 0, där en tolkning kan vara 
# att de svälter för att det inte finns någon mat i kvar området, populationen dör altså ut efter 21
# månader.


#c) Innan vilken månad skulle man behöva göra åtgärder?

# Om vi vill avgöra när de är vid 80% av sin bärförmåga så kan vi få detta från vår vektor n 
# för det index där populationsstorleken överstiger 0.8*k. 
# Enkelt kan detta göras genom att visa n och manuellt leta upp det index där n är större än 0.8*k.
# Detta kan beräknas med kommandot:
which(n>0.8*k)[1]
# Vilket ger svar med värdet 5. Eftersom vi behöver stoppa svärmning innan det sker så blir första index‐1 det 
# i letar efter. Alltså är behöver vi sätta in åtgärder innan månad 5. 

# Rättning
# a) 1 p korrekt figur, 1 p att populationen kraschar efter 20 månader, 1 p för 
#redovisat korrek t tillvägagångssätt,  
# Avdrag: ‐0.5 om figur inte går till 25 månader, ‐0.5 om någon förklaring till kod saknas, 
#‐0.5 p om figuren saknar linje, ‐0.5 om figuren inte begränsas i y‐led av 0. Att ange tid i 
#år istället för månader ger 0 p.

#b) 0.5 p för beräkning av 80% av K och which(), 0.5 p för korrekt 
#bedömning (månad 4‐5 har accepterats), 
# Avdrag:‐0.5 p för enbart grafisk bedömning, 
#‐0.5 p för korrekt lösning med felaktig tolkning eller slarvfel 

#### ============================================================================ ####
#                 FRÅGA 3 - T.test, undersökning av medelvärden
#### ============================================================================ ####

#a) Adekvata hypoteser och undersökning av normalfördelning
# Nollhypotes H0: Det finns ingen skillnad i upptag av arsenik i hjärtmuskel och pankreas hos möss.
# Alternativ hypotes H1: Det finns en skillnad i upptag av arsenik i hjärtmuskel och pankreas hos möss.

# För att få utföra ett t.test kontrolleras först
# normaliteten med histogram och qqplot.
par(mfrow=c(2,2))
hist(cox$heart)
hist(cox$panc)
qqnorm(cox$heart);qqline(cox$heart)
qqnorm(cox$panc);qqline(cox$panc)
# Jag accepterar detta som approximativt normalfördelat och väljer ett parametriskt test, dvs
# t-test med två oberoende stickprov som testar om de observerade gruppernas medelvärden är
# identiska.

#b) Lämpligt statiskts test är t.test
t.test(cox$heart, cox$panc)
# Detta ger oss ett p värde på 0.0005889 och även de två medelvärdena för arsenikhalterna som är 
# 156.3800 för hjärtat och 129.5133 för pankreas. Då p<0.05 förkastas H0 och vi accepterar den alternativa hypotesen 
# att det är en skillnad i arsenikhalt i de båda organen, och hjärtats arsenikhalt är större än den i pankreas. 

boxplot(cox$heart,cox$panc,xaxt="n",las=1,ylab="Arsenikhalt")
axis(side=1,at=c(1,2),labels=c("Hjärtmuskel","Pankreas"))

#Rättning:
# a) Korrekta hypoteser 1 p. Korrekt kontroll av normalfördelning 1 p. 
# b) t‐test oberoende eller parvis (med motivering) 1 p. Wilcoxon är också OK. 
# c) H0 förkastas 0,5 p, Rätt skillnad 0,5 p. 

#### ============================================================================ ####
#                            FRÅGA 5 - CHI 2 Test - Oberoende
#### ============================================================================ ####

# a) Adekvata hypoteser för ett oberoende CHI 2 test
# Detta Chi 2 test är oberoende för att Rad- och kolumnvariablerna är oberoende av varandra

# H0: Det finns inget beroende mellan variablerna behandling och influensa
# H1: Det finns ett beroende mellan variablerna behandling och influensa 

r1 <- c(14, 1056)
r2 <- c(95, 437)
vac <- rbind(r1,r2)
rownames(vac)<-c("Vaccinerade", "Placebo")
colnames(vac)<-c("Influensa","Friska")
vac # 2x2 tabell med rubriker

# b)
chisq.test(vac) #Chi2 test
#Det vi får är:
#Pearson's Chi-squared test with Yates' continuity correction
#data: vac
#X-squared = 150.86, df = 1, p-value < 2.2e-16 

# c) Adekvata slutsatser
#Det erhållna p-värdet är mycket mindre än 0.05 och nollhypotesen förkastas. Vi accepterar
# den alternativa hypotesen och antar att det finns ett beroende mellan variablerna 
# behandling och influensa Det finns således anledning att tro att vaccinet är effektivt.

#Rättning:
# a) Korrekta hypoteser 1 p, frekvenser/nominalskala motivering 1 p. 
# b) Oberoende chi-2 1 p, 
# c) H0 förkastas 0,5 p, vaccination har effekt 0,5 p. 

#### ============================================================================ ####
#                                FRÅGA 7 - Matris
#### ============================================================================ ####

# a) Undersökning om det kan ske ett uttag

# Lesliematris
r1<-c( 0.83  , 0     , 0     , 0.2   ,1.56   , 2.63   )
r2<-c( 0.06  , 0.73  , 0     , 0     , 0     , 0      )
r3<-c( 0     , 0.01  , 0.93  , 0     , 0     , 0      )
r4<-c( 0     , 0     , 0.07  , 0.95  , 0     , 0      )
r5<-c( 0     , 0     , 0     , 0.05  , 0.94  , 0      )
r6<-c( 0     , 0     , 0     , 0     , 0.04  , 0.83   )

palm <-rbind(r1,r2,r3,r4,r5,r6)

#Om denna matris har ett största egenvärde som är >1 kan uttag ske. 
#Vi får fram dominanta egenvärdet med:

#eigen() ger egenvärden och egenvektorer som utdata
egen <- eigen(palm)
#Dominanta egenvärdet, reslutatet av egenvärdet
egen$values
#Reella delen av det dominanta egenvärdet
Re(egen$values[1])

#Går även att göra genom:
max(Re(eigen(palm)$values)) 

# Vi får svaret 0.990235 vilket indikerar att det inte är bra att hugga ner några träd eftersom 
# populationen redan minskar med ca 1% per år. 

#b) Modifiering av överlevnad och analys av egenvärdet
# Vi ökar överlevnaden och tar fram det nya egenvärdet med att:

palm_mod1 <- palm # Skapa en kopia att modifiera
palm_mod1[2,2] <- 0.92 #Sätter värdet i andra kolumnen i andra raden tll 0.92
max(Re(eigen(palm_mod1)$values)) #Egenvärdet

# Svaret blir 1.008891 vilket är över 1 som indikerar att palmpopulationen kan klara visst uttag. 

#c)
#Vi tar nu hänsyn till uttaget genom att sätta överlevanden till 90% och beräknaser sedan det 
#nya egenvärdet: 
palm_mod2 <- palm_mod1 
palm_mod2[4,4] <- 0.90
egen<-eigen(palm_mod2)
max(Re(eigen(palm_mod2)$values)) #Egenvärdet

#Dominanta egenvärdet, reslutatet av egenvärdet
egen$values

#Detta uttag minskar egenvärdet till 0.9980 alltså är under 1, så vi kan inte skörda så mycket på längre sikt. 

# Rättning:
# 1 p korrekt  matris, 1 p egenvärdet, 1 p korrekt tolkning av egenvärdet, 
# Avdrag: ‐0.5 p om egenvärdet saknas, ‐0.5 p om fel tolkning/svar(inkl fel %), 
#‐0.5 p för gardering (om ej efterfrågad information ges tex stadiefördelning). 

# b) och c)  0.5 p för ändring av rätt plats, 0.5 p för korrekt slutsats, 
# Avdrag: ‐0.5 p om egenvärdet/tolkning saknas, ‐0.5 p om den nya matrisen saknas/ändringen inte 
# redovisats/felaktig. (ändring av överlevnad som L_mod[4,4]*0.9 gav rätt), ‐0.5 p om c besvaras med 
# a) och inte b) matrisen. 

#### ============================================================================ ####
#                                FRÅGA 8 - Tvåvägs ANOVA
#### ============================================================================ ####

# a) Anova med Adekvata hypoteser
#H0: Det finns ingen skillnad i blodtryck mellan dietgrupperna och inte heller mellan kvinnor och män.
#H1: Det finns skillnad i blodtryck mellan dietgrupperna samt mellan kvinnor och män.
#H0: Växelverkan mellan diet och kön leder inte till skillnad i blodtryck
#H1: Växelverkan mellan diet och kön leder till skillnad i blodtryck 

# b) Undersökning av normalitet och varianshomogenitet
tryck$gender<-as.factor(tryck$gender) #Variabeln genotype som text ersätts med variabeln genotype som omvandlats till faktor
tryck$diet<-as.factor(tryck$diet)

modell<-lm(bp~gender*diet, data=tryck)
par(mfrow=c(2,2)); plot(modell) #residualgraf av tvåvägs-ANOVAn

# Det verkar helt OK med normalitet och varianshomogenitet. Vi kan således gå vidare med en
# parametrisk prövning. 

anova(modell)

# Resultat av variansprövning
#               Df Sum Sq Mean Sq  F value Pr(>F)    
#  gender        1  15873 15873.4 127.5801 <2e-16 ***
#  diet          2  34595 17297.5 139.0263 <2e-16 ***
#  gender:diet   2     54    27.2   0.2185 0.8038  

# c) 
# Nollhypoteserna för diet och kön kan förkastas och det finns skillnader i blodtryck mellan
# dietgrupperna samt mellan könen. Vi ser dock ingen växelverkan mellan faktorerna.
# Vi kan undersöka var skillnaderna ligger med TukeyHSD och boxplot. 

par(mfrow=c(1,2)) 
boxplot(tryck$bp~tryck$gender)      
boxplot(tryck$bp~tryck$diet) 

TukeyHSD(aov(modell)) ##### Modeller
plot(TukeyHSD(aov(modell)))  ##### Plot av Modeller

#Rättning:
# 2-vägs ANOVA 0,5 p., Korrekta hypoteser 1 p. 
# b) Residualernormalfördelade med varianshomogenitet 1 p, rätt ANOVA 0,5 p. - interaktionsplot ej 
# nödvändig men hjälpsam. 
# c) Diet påverkar blodtrycket 0,5 p, kön påverkar blodtryck 0,5,
# Ingen växelverkan mellan diet och kön beträffande blodtrycket 1 p. 













#### ============================================================================ ####
#                                    TENTAMEN 2019
#### ============================================================================ ####
#### ============================================================================ ####
#                   FRÅGA 1 - T-TEST MELLAN TVÅ OBEROENDE GRUPPER
#### ============================================================================ ####

#a) Kolla så att förutstättningarna är uppfyllda 

#I denna uppgift sker en gruppvis jämförelse med oparade värden vilket går att analysera med ett t-test.
#Vi vill se om det finns en skillnad i effekt mellan en infekterad och en kontrollgrupp av möss
# mha medelvärdena av nivån av interleukin-10 produktionen i T-celler

#Det första vi gör är att kolla variansen mellan grupperna genom en boxplot:
boxplot(mice$inf,mice$kon,xaxt="n",las=1,ylab="MFI") 
axis(side=1,at=c(1,2),labels=c("Infekterad","Kontroll"))
#Det läses av som att de har skilda varianser

#För att kunna genomföra ett t.test måste vi först kolla att kravet 
#för ett t.test är uppfyllt, datan måste vara normalfördelad. Detta göres mha 
#avläsning av histogram och qq plot för de båda grupperna. 

library(plotrix)

#Histogram och QQ plot av MFI för infekterade möss
hist(mice$inf) #Histogram för MFI för infekterade möss
qqnorm(mice$inf, col="red", pch=16) #QQplot för MFI för infekterade möss
qqline(mice$inf)#QQ linje

#Histogram och QQ plot av MFI för kontrollgruppen av möss
hist(mice$kon) #Histogram för MFI för kontrollgruppen av möss
qqnorm(mice$kon, col="red", pch=16) #QQplot för MFI för kontrollgruppen av möss
qqline(mice$kon)#QQ linje

#b) Hypoteser

#  Adekvata hypoteser: 
#  H0: Det finns ingen skillnad i uppregleringen av interleukin-10 produktion i T-celler hos möss
#  H1: Det finns en skillnad i uppregleringen av interleukin-10 produktion i T-celler hos möss

#c)

#Tidigare diagramm läses av som approximativt normalfördelat. Då väljer vi ett t-test för
#               för två oberoende grupper och skild varians, ett Welch two sample t-test.

# Welch two sample t-test:
t.test(mice$inf, mice$kon) 

#Slutsats
#Det vi får ut är att p = 0.04631. Då p>0.05 innebär detta att vi förkastar 
#nollhypotesen och accepterar den alternativa hypotesen, 
#Det finns en skillnad i uppregleringen av interleukin-10 produktion i T-celler hos möss. 
#De två grupperna skiljer sig åt och eftersom medelvärdet för de två grupperna går att läsas av
#till 79.2 för de infekterade och 66.0 för kontrollgruppen ser vi att uppregleringen är större hos
#de infekterade mössen. 


#### ============================================================================ ####
#                       FRÅGA 2 - DISKRET LOGISTISK MODELLERING
#### ============================================================================ ####

#a) Graf över populationens utveckling under 100 år efter ankomsten till området

#För att följa populationens utveckling använder vi oss av den diskreta logistiska modellen. 
#Det första vi gör är att vi skapar en funktion som beräknar populationsstorleken 
#och en for-loop som låter oss göra det många år.

#Funktionsdefinition med namnet dlt
dlt <- function(n,r,k) { #Definerar funkitonen med tre variabler som argument
  n_ny <- n + r*n*(1-n/k) #Diskret logistiskt tillväxt
  return(n_ny) #Retunerar värdet till den anropande funktionen
}

#Defenition av startvärden.
n<-c()
n[1]<-11 #Startvärde för n
r<-0.092 #Maximal perkapitareproduktion
k<-890 #Bärkraft, Ekosystem med en bärkraft för 890 individer.
sluttid<-100 #Vi vill följa dem under 100 år framöver

#Forloop för de nästa 25 åren
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av populationsstorleken över tid
plot(t,n, type="l", #Type=l för en linje ist för prickar
     lwd=2, #Lwd står för line width
     ylab="Populationsstorlek",
     xlab="Tid (år)")

#b) Hur lång tid tar det innan populationen når 100 individer?

#Vi avläser från grafen att populationen når ca 100 individer vid 25 år. 
#Vi kan då titta på index i n för att se värden runt denna tid.
n[23:30]

#Och vi kan se att populationen når 100 individer för index 27-28 
#vilket motsvarar drygt 27 år. Dvs vi når 100 individer under år 27 för 
#att vid år 28 vara fler än 100.


#### ============================================================================ ####
#                       FRÅGA 3 - SAMBANDSTEST M.H.A.PEARSON
#### ============================================================================ ####

#a) 

#För att kunna hitta sambandet genom beräkning av korrelationskoefficienten 
#måste först en bedömning av om ingående variabler är approximativt normalfördelade
#och om sambandet är linjärt.

#Undersökning om sambandet är linjärt med en plot.
plot(length$langd, length$fev)

#Detta visar ett tydligt linjärt samband. 

#För undersökning om båda variablerna är approximativt normalfördeladepar(mfrow=c(2,2))
hist(length$langd)
hist(length$fev) 
qqnorm(length$langd);qqline(length$langd) 
qqnorm(length$fev);qqline(length$fev)

#b)

#Detta visade normalfördelade variabler och vi kan använda Pearsons produktmomentkorrelationskoefficient. 

#Adekvata hypoteser:
#H0: Det finns inget samband mellan medelvärdet av FEV och medelvärdet av längd i de aktuella längdgrupperna för pojkar mellan 10-15 år.
#H1: Det finns ett samband mellan medelvärdet av FEV och medelvärdet av längd i de aktuella längdgrupperna för pojkar mellan 10-15 år.

cor.test(length$langd, length$fev) #Pearsons-korrelationstest 

# c)

# Slutsats:
# Detta ger oss ett p värde på 1.762e-09. Då p<0.05 förkastar vi nollhypotesen och vi
# accepterar den alternativa hypotesen H1. Det innebär att det finns ett starkt positivt
# statistiskt samband mellan de båda variablerna.Det finns också skäl att detta samband
# har en biologisk grund.Korrigering med avseende på längd bör alltså göras innan FEV 
# används som standard vid bedömning av lungfunktion.



#### ============================================================================ ####
#                       FRÅGA 5 - CHI 2 TEST FÖR HOMOGENITET
#### ============================================================================ ####

# a) 
# Jag väljer ett Chi 2 test för homogenitet då detta innefattar frekvenser. 

# b)
# Adekvata hypoteser:
# H0:  Åldern vid första barnafödandet och prevalensen av bröstcancer har inget med varandra att göra.
# H1:  Åldern vid första barnafödandet kan påverka prevalensen av bröstcancer.

# c)

# De förväntade frekvenserna med hjälp av:
chisq.test(ca_m)$expected
# Den minsta förväntade frekvensen är 149.7

# d) 
chisq.test(ca_m)

#Resultatet är högst signifikant (p<<0,05) och nollhypotesen förkastas. 
#Vi kan således dra slutsatsen är det finns ett signifikant påverkan av åldern 
#vid första barnafödandet och prevalensen hos bröstcancer.



#### ============================================================================ ####
#                                FRÅGA 6 - MATRIS
#### ============================================================================ ####


#a) Övergångsmatris för ansjovis
r1<-c( 0.012 , 0     , 0     , 56    , 209   , 223    )
r2<-c( 0.0556, 0.0490, 0     , 0     , 0     , 0      )
r3<-c( 0     , 0.0261, 0.0960, 0     , 0     , 0      )
r4<-c( 0     , 0     , 0.0255, 0.978 , 0     , 0      )
r5<-c( 0     , 0     , 0     , 0.016 , 0.982 , 0      )
r6<-c( 0     , 0     , 0     , 0     , 0.011 , 0.982  )

#Diagonalen är sannolikheten att stanna
#Underdiagonal är sannolikheten att tillväxa

L <-rbind(r1,r2,r3,r4,r5,r6)

# b) Den stabila stadiefördelningen i procent

# Den stabila stadiefördelningen motsvaras av egenvektorn till det dominanta egenvärdet 
# Följande sekvens tar fram egenvärdet och räknar om det till procent

egen <- eigen(L)
egen$values # visar alla egenvärden så att vi kan se vilket som är dominant
egen$vectors # visar alla egenvektorerna 
fordelning<-Re(egen$vectors[,1]) 
procent<-fordelning/sum(fordelning)*100 
procent

# Den stabila stadiefördelningen är: 93.7588245  5.4969067  0.1591716  0.2097627  0.2186474  0.1566871

# c) När populationen befinner sig vid den stabila stadiefördelningen, hur mycket ändras den varje år?
# Hur mycket den förändras per år ges av det Dominanta egenvärdet.

#Dominanta egenvärdetegenvärdet
egen$values

#Reella delen av det dominanta egenvärdet
Re(egen$values[1])

# Svar: 0,997 -> Populationen  minskar med 0.3 % varje år. 

# d) Ändring av värden

L_mod <- L # Skapar en kopia att modifiera

#Doninanta egenvärdet i ett svep
gf <- c() #nollställning 

#Sätter värdet i rader ch kolumner till andra värden
L_mod[1,1] <- 0.112
L_mod[2,2] <- 0.1490
L_mod[2,1] <- 0.1556
egen<-eigen(L_mod)
gf [1] <- Re(eigen(L_mod)$values[1])
gf [1]

#Detta ger ett egenvärde på 1,010467, alltså en årlig ökning på 1,05 %.

L_mod <- L # Skapar en kopia att modifiera
gf <- c() #nollställning 
L_mod[5,5] <- 0.987
L_mod[6,6] <- 0.987
egen<-eigen(L_mod)
gf [2]<- Re(eigen(L_mod)$values[1])
gf [2]

# Detta ger ett egenvärde på 1,00052 alltså en årlig ökning av populationen på 0,052 %.

#Svar: Sammantaget är alltså strategi 1 den bästa för populationen, 
#      eftersom den ger den största årliga ökningen.

#### ============================================================================ ####
#                       FRÅGA 7 - REGRESSION ALT ENGÄVS-ANOVA
#### ============================================================================ ####

# Regression undersöker när en variablen påverkar en annan och båda variablerna är kontinuerliga

# a) Adekvata hypoteser
# H0: Det finns inget samband mellan halten protein och kycklingarnas vikt
# H1: Det finns ett samband mellan halten protein och kycklingarnas vikt

# b) Undersökning om samband.
# Vi börjar med att undersöka om eventuella samband är linjära med en figur

#Plot för att se hur sambandet ser ut 
plot(chick$Crude_protein, chick$weight, pch = 12)

#Det är I alla fall inte uppenbart icke-linjärt. 
#Vi går därför vidare och skapar en linjär modell för att kunna granska residualerna.

#Regressionsmodell
regr<-lm(chick$Crude_protein~chick$weight) 

par(mfrow=c(2,2))

#Plot av regressionsanalysen. Fyra residualgrafer i ett fyrdelat fönster
plot(regr)

# Vi ser inga större problem med skillnader i varians eller avvikelser från normalfördelning. 
# Vi går vidare med en analys
summary.aov(regr)

# Genom att avläsa p värdet ser vi att effekten av protein har en signifikant effekt på vikten.

# c) Hur ser sambandet ut?
#För att läsa av sambandet kollar man på modellen:
regr

#och ser att även här att sambandet är positivt med en lutning på 103.09. 
#Sambandet kan beskrivas som vikt (g) = 92.4 + 104 * andel protein (%)

par(mfrow=c(1,1))
plot(chick$Crude_protein,chick$weight, xlab="Protein (%)", ylab="Vikt (g)") 
clip(min(chick$Crude_protein),max(chick$Crude_protein),min(chick$weight),max(chick$weight)) 
abline(regr, col="blue")

# d) Vad är ett bra svar om kycklingfarmaren undrade hur mycket kycklingarna skulle växa om hen ökade
# andelen råprotein till 25 %? 

#Om man sätter in 25 i ekvationen får man följande:
#vikt (g) = 92.4 + 104 * 25% = 2669.6 g
# Vi påpekar dock att denna höga andel protein ligger utanför det studerade området 
# så vi vet egentligen inte vad som händer vid en sådan nivå.



#### ============================================================================ ####
#                                FRÅGA 8 - Tvåvägs ANOVA
#### ============================================================================ ####

# a) Adekvata hypoteser 
#H0: det finns ingen effekt av miljö och/eller behandling på frisläppandet av kol
#H1a: det finns en effekt av växtlighet på frisläppandet av kol
#H1b: det finns en effekt av tempreturbehandling på frisläppandet av kol
#H1c: växtlighet och temperaturbehandling interagerar i sin effekt på frisläppandet av kol

# b) 
#Variabelerna vegtype och treatment som text ersätts och omvandlats till faktor
arctic$vegtype<-as.factor(arctic$vegtype)
arctic$treatment<-as.factor(arctic$treatment)

#Data visar att vi har två faktorer och en responsvariabel på kvotskala vilket ger oss att 
#analysen lämpligast görs med en tvåvägs-ANOVA. Vi börjar därför med att titta på residualerna 
#för att se om data uppfyller kraven.

modell<-lm(resp~vegtype*treatment, data=arctic)
par(mfrow=c(2,2)); plot(modell) #residualgraf av tvåvägs-ANOVAn

#Residualerna ser bra ut och en log-transformation av resp ger inget bättre resultat
#så vi fortsätter på otransformerade data. Vi tar fram anova-tabellen på analysen genom
anova(modell)

#Vi ser att det finns en signifikant interaktionseffekt mellan vegetationstyp och behandling.
#Effekterna av behandling beror alltså på vegetationstyp.

# c) 

interaction.plot(arctic$treatment, arctic$vegtype, arctic$resp) 
TukeyHSD(aov(modell))

#Det bara är de varma områdena för båda miljöer som inte skiljer sig från övriga kombinationer.
#Interaktionen kommer alltså från att buskar respirerar mindre än gräs när 
#temperaturen växlar men är lika när det är varmare.

# Svar: Respirationen beror på miljö och temperatur utom när det är konstant +2 C (thawed). 
# När temperaturen växlar mellan -4C (cycle) och +2C frigörs mer kol i gräsmiljön än i buskmiljön. 
# Överlag frigörs mer kol i den varma miljö n än i den som växlade i temperatur.











#### ============================================================================ ####
#                                 TENTAMEN 2018
#### ============================================================================ ####

#### ============================================================================ ####
#                     FRÅGA 1 - CHI 2 TEST OCH NOLLHYPOTESPRÖVNING
#### ============================================================================ ####

### a) Välj ett lämpligt test för att analysera frågeställningen, och motivera ditt svar #### 
# Jag väljer ett Chi 2 test. Detta är ett problem med två faktorer, 
#medicin (Pirenzepine och Trithiozine) och, 
#magsår (läkt eller kvarvarande). 
#Responsen är antal personer och vi har bara ett värde per cell. Vi kan därför inte köra 
#en tvåvägs-ANOVA utan måste använda oss av ett chi2-test


#### b) Vilken är nollhypotesen för testet? #### 
# Nollhypotes: Det är ingen skillnad i effekt hos de båda läkemedlen 
# Alternativ hypotes: Det finns en skillnad i läkemedelsverkan hos läkemedeln 

#Enligt tentan:
# Fördelningarna lika mellan grupperna eller annan godtagbar nollhypotes

#### c) Genomför testet och tolka utfallet – har läkemedelsföretaget rätt? #### 

#Efterom denna inte finns i en datafil måste vi skapa den

#Matris enligt Quiz 6: 
matris<-rbind(pir,tri)
pir <- c(23, 7)
tri <- c(18, 13)
chisq.test(matris, p=61) #Funkar även utan p=61

# Svar: Nej de har fel
# Eftersom p värdet ger oss ett värde på 0.2025  alltså >0.05 accepterar vi
# nollhypotesen om att det inte finns någon skillnad i läkemedelsverkan

#Enligt tentan:
magmed<-matrix(c(23,18,7,13), nrow = 2, ncol = 2, dimnames = list(c("Pir","Tri"),c("Lakt","Kvar")))
magmed #Matris med titlar på både övre och undre
chisq.test(magmed) #Chi 2 test

#Vi ser från svaret att p > 0,05 så vi behållar alltså nollhypotesen att de 
#två medicinerna inte skiljer sig åt. Våra data ger oss alltså inte stöd att 
#påstå att det finns skillnader mellan de två medicinerna i framgången att läka magsår. 
#Läkemedelsföretaget har alltså troligen fel.



#### ============================================================================ ####
#                    FRÅGA 2 - PARAT T-TST OCH NOLLHYPOTESPRÖVNING
#### ============================================================================ ####
#### a) Välj ett lämpligt test, och motivera ditt val utifrån förutsättningarna #### 
# Jag skulle välja ett parat t.test. Då denna kräver att observationerna gjorts parvis,
# alltså att en parvis koppling finns mellan mätvärdena (ex: man har mätt på
# samma person före och efter en behandling som de gäller här)

# Testar om väntevärdet för differenserna för varje par är noll.

#Enligt tentan:
#Vi har här en grupp av personer som mätts vid två olika tillfällen.
#Eftersom dessa mätningar då inte är oberoende måste vi vilja ett test
#som tar hänsyn till detta. Vi misstänker att parat t-test och undersöker
#därför först skillnaderna i de två mätningarna är normalfördelade. 

#### b) Formulera hypoteser för prövningen #### 
#Nollhypotes: Det är ingen skillnad i blodtrycksnivåerna
#Alternativ hypotes: det finns en skillnad i blodtrycksnivåerna

#### c) Genomför testet och redovisa dess resultat #### 

#Lista över nödvändiga värden
fore <-c(200, 174, 198, 170, 179, 182, 193, 209, 185, 155, 169, 210)
efter <-c(191, 170, 177, 167, 159, 151, 176, 183, 159, 145, 146, 177)

# För att få utföra ett t test krävs det att datan är normalfördelad, dessa avläses genom ett
# histogram och qq plot

#Histogram och qq plot för både före och efter

hist(fore) #Histogram
qqnorm(fore, col="red", pch=16) #QQplot
qqline(fore)#QQ linje

hist(efter) #Histogram
qqnorm(efter, col="red", pch=16) #QQplot
qqline(efter)#QQ linje

t.test(fore, efter, paired=T) #T.test
boxplot(fore, efter, names=c("Före", "Efter")) #Boxplot som visar att efter har generellt lägre värden

# Enligt tentan:
# kapto$diff<-kapto$efter-kapto$fore qqnorm(kapto$diff);qqline(kapto$diff)
t.test(kapto$fore,kapto$efter, paired=T)
boxplot(kapto$fore,kapto$efter, names=c("Före", "Efter"))

#### d) Vilken eller vilka slutsatser drar du från resultatet av prövningen? #### 
# Genom avläsning av det oparade t.testet gav det oss ett pvärde som var 5.285e-05 vilket är
# Mindre än 0.05, alltså p<0.05 vilket innebär att vi förkastar nollhypotesen och accepterar
# den alternativa som är att Kaptopril har en påverkan på blodtrycket

#Enligt tentan:
#Vi ser här att p < 0,05 så vi förkastar H0 att medicinen inte har någon effekt
#och antar H1 att medicinen påverkar blocktrycket. En figur visar oss enklast 
#vad som händer (vi kan även se att differensen från testet är 18,58 vilket
#betyder att värdena före är större en de efter)

## SLUTSATS: Vi drar slutsatsen att kaptopril kan användas för att minska blodtryck 
#             hos personer med högt blodtryck.










#### ============================================================================ ####
#                    FRÅGA 3 - DISKRET LOGISTISKT TILLVÄXT MED LEJON
#### ============================================================================ ####
#### a) Visa med en graf deras populationsutveckling under de närmaste 50 åren, 
#       och redogör kortfattat för hur du skapat grafen #### 

#Funktionsdefinition med namnet dlt
dlt <- function(n,r,k) { #Definerar funkitonen med tre variabler som argument
  n_ny <- n + r*n*(1-n/k) #Diskret logistiskt tillväxt
  return(n_ny) #Retunerar värdet till den anropande funktionen
}

#Defenition av startvärden
n<-c()
n[1]<-17 #Startvärde för n
r<-0.11 #Maximal perkapitareproduktion
k<-520 #Bärkraft, Ekosystem med en bärkraft för 520 individer.
sluttid<-50 #Vi vill följa dem under 50 år framöver

#Forloop för de nästa 50 åren
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av n mot t
plot(t,n, type="l", #Type=l för en linje ist för prickar
     lwd=2, #Lwd står för line width
     ylim=c(0,600)) #Ett spann på y axeln värdet mellan 0-700 

# Enligt tentan:
#Vi får i uppgiften veta att populationen av makaker tillväxer enligt
#den diskreta logistiska modellen. Först skapar vi en vektor till 
#populationsstorleken som vi kallar n och sätter sedan in antalet 
#makaker som finns vid tiden 1, samt sätter bärförmågan K till 520.

n<-c() 
n[1]<-17
K<-520

#Eftersom vi nu måste beräkna populationsstorleken för 50 år tar vi hjälp av en for-loop. 
#Vi sätter tiden till 50 och gör beräkningar enligt den diskreta logistiska modellen:
#n(t+1)=n(t)+r*n(t)*(1-n(t)/K)
for (i in 1:50){ n[i+1]<-n[i]+0.11*n[i]*(1-n[i]/K)
}
#Detta skapar en vektor n med populationsstorleken för varje år. 
#Denna information kan vi nu rita i en figur. 
#Eftersom vi beräknas 50 värden utöver vårt startvärde måste vi be R rita 51 värden.
plot(1:51,n)




#### ============================================================================ ####
#                    FRÅGA 4 - SAMBAND MELLAN PARASIT OCH RANG MED PEARSON
#### ============================================================================ ####
#### a) Har individer med högre social status en lägre parasitförekomst? #### 
cor.test(parasit$antal, parasit$status, method = "spearman")
#Vi kan se p<0,05 och vi antar därför att det finns ett samband som inte förklaras 
#av slumpen. Från rho=0,45 kan vi se att sambandet är positivt. 
#Slutsatsen är att högre social status är korrelerat med fler parasitägg i avföringen.

#b) r , dvs 0,45x0,45=0,20. Variationen i sambandet mellan parasitförekomst och social
#status förklaras till 20%. Kan beräknas med overkillkod: 
#cor.test(parasit$status,parasit$antal, method = "spearman")$estimate^2


#### b) Hur lång tid tar det innan de nått upp till hälften av bärkraften? #### 
#Avläser när n är större är bärkraften x 0.5 (50%)
which(n>k*0.5)[1]

#Enligt tentan: 
#Vi skall sedan ta reda på hur lång tid det tog för populationen att uppnå halva bärkraften,
#dvs K/2. Denna information kan vi få från vår vektor n för det index där 
#populationsstorleken överstiger K/2. Enkelt kan detta göras genom att visa n och 
#manuellt leta upp det index där n<K/2. Vi kan även låta R göra jobbet genom att 
#fråga för vilka index som n är större än K/2
which(n>K/2)
#vilket ger svar med värdena 33 till 51.
#Alltså är populationen större än halva bärkraften efter 32 år.





#### ============================================================================ ####
#                   fråga 6 - Populationsmodell i tre stadier med matris
#### ============================================================================ ####
#### a) Skapa en Lesliematris som kan användas för att modellera populationen #### 
r1<-c( 0     , 0     , 0.42  )
r2<-c( 0.60  , 0     , 0     )
r3<-c( 0     , 0.75  , 0.95  )

L <-rbind(r1,r2,r3)


#### b) Bestäm den stabila stadiefördelningen i procent #### 
#eigen() ger egenvärden och egenvektorer som utdata
egen <- eigen(L)
Re(egen$vectors)
fordelning <- egen$vectors[,1]
Re(fordelning)
#Den stabila stadiefördelningen är 23.95%, 13.01%, 63.03%


#### c) Om populationen befinner sig vi den stabila stadiefördelningen – 
#       kommer den att öka eller minska, och hur mycket? #### 
egen$values
Re(egen$values[1])

#Öka med 10 %

#### d) Populationen flyttar till ett område med mer näringsrikt bete vilket 
#       ökar de vuxnas sannolikhet att överleva till 96 %. Dock finns det mer 
#       vargar vilket minskar kalvarnas sannolikhet att klara första året från 
#       60 % till 50 %. På vilka sätt påverkar detta populationen? #### 

#### ============================================================================ ####
#                   FRÅGA 7 - TVÅVÄGS ANOVA MED DIET OCH TRÄNING
#### ============================================================================ ####
####a)
health$diet<-as.factor(health$diet) #Variabeln diet som text ersätts med variabeln diet som omvandlats till faktor
health$exercise<-as.factor(health$exercise)#Variabeln season som text ersätts med variabeln season som omvandlats till faktor

modell1<-lm(bp~diet*exercise, data=health)

par(mfrow=c(2,2)); plot(modell1) #residualgraf av tvåvägs-ANOVAn

anova(modell1)

TukeyHSD(aov(modell1)) ##### Modeller

#b) Förklarande modell
plot(TukeyHSD(aov(modell1))) ##### Modeller

















#### ============================================================================ ####
#                             TENTAMENSÖVNINGAR 2018
#### ============================================================================ ####

#### Fråga 1 - T.test mellan skallar ####
#a) Jämförelse medelvärdet på två skallar från olika tidsåldrar
#1. Det första vi måste göra är att udnersöka om datan är normalfördelad för att sedan gå vidare
# med ett lämpligt test. Detta kan göras med en qqplot

qqnorm(egypt$year4000BCE) # QQplot för 4000 BCE
qqline(egypt$year4000BCE) # QQline för 4000 BCE

qqnorm(egypt$year150CE) 
qqline(egypt$year150CE)

# Tidigare diagramm läses av som approximativt normalfördelat. Då väljer vi ett t-test för
#               för två oberoende grupper och skild varians, ett Welch two sample t-test.

t.test(egypt$year4000BCE,egypt$year150CE)

# Svar: Detta ger p = 0,02724, vilket är mindre än 0,05. Slutsatsen blir att vi förkastar nollhypotesen, 
# och att vi antar att skallarnas storlek förändrats under åren som gått mellan 4000 BCE och 150 CE.

#### Fråga 2 - Stegning med forloop ####
n<-numeric()
n[1]<-1 # vi startar med en gren
for(i in 2:40) { # chansning på att 40 ska räcka
  n[i]<-n[i-1]+0.8*n[i-1]
}
n[17]
#Mellan 16 och 17 år. Alltså 17 år krävs för att komma över 10 000

#### Fråga 3 - Envägs anova med grupperna A, B och C ####
#Data antas i uppgiften vara normalfördelade, så vi behöver inte testa detta. Med andra ord kan vi
#direkt gå över till en ANOVA då vi vill se SKILLNADEN inte korrelationen:

modell<-lm(godning$avkastning~godning$godsel)
anova(modell)

#Detta ger ett p-värde på 0,008594 och alltså kan vi förkasta nollhypotesen att de tre medlen ger
#Samma avkastning och det finns en skillnad

#För att se vilka som skiljer sig åt använder vi ett Tukeytest:
TukeyHSD(aov(modell))
boxplot(godning$avkastning~godning$godsel)
#Detta visar att det endast är B och A som ger en signifikant skillnad, och att A är bättre än B.

#### Fråga 4 - Signifikant medelvärde mellan två grupper ####
man <-c(37, 39, 37, 42, 39, 45, 42, 39)
kvinnor <-c(44, 40, 39, 45, 47, 47, 43, 41)
t.test(man,kvinnor) #p-value = 0.04321

#Svar: Vi förkastar H0 om att det inte finns en skillnad och vi antar att det finns en skillnad. 
#      Alltså ja de skiljer sig åt

#### Fråga 5 - 95% konfidensinterval för värden ####
majs<-c(1903, 1935, 1910, 2496, 2108, 1961, 2060, 1444, 1612, 1316, 1511) 
#Fnktionen t.test används, vilken automatiskt innehåller ett 95%-igt konfidensintervall:
t.test(majs) # 1611.200 och 2071.709
#Resultatet blir alltså att konfidensintervallet för avkastningen blir 1611,2 till 2071,7.

#
#### Fråga 7 - Korrelationstest ####
plot(smarta$fore,smarta$efter) #Plot av nitratnivåerna VS antalet crassa. Visar att datan inte är linjär

Kolla normalfördelningen
qqnorm(smarta$fore);qqline(smarta$fore) #qq plot över nitratnivåerna. Datan är fördelad
hist(smarta$fore) #Histogram för nitratnivåerna. Visar om de är normalfördelat
boxplot(smarta$fore,smarta$efter)

cor.test(smarta$fore,smarta$efter)

#P värdet är över 0.05 vilket gör att vi behålla nollhypotesen och antar att det inte finns ett samband. 
. 
# En signifikant korrelation skulle inte peka på att läkemedlet är effektivt för smärtlindring. 
# Om läkemedlet är totalt ineffektivt och varje förepoäng är lika med efterpoängen skulle
# korrelationskoefficienten bli 1 och därmed peka på en signifikant korrelation.

#### Fråga 8 - Chi2 test med 5 % signifikansnivå ####
gnuer<-c(13,2)
chisq.test(gnuer)

# Vi får 0.004509 alltså skiljer sig antalet gnuer i skyddade respektive
# oskyddade områden.

#### Fråga 9 - Tvåvägsanova med konstiga svar ####
antidot$dos    <-as.factor(antidot$dos) #Endast dos som behöver konverteras 

modell<-lm(log(antidot$blod)~antidot$antidot*antidot$dos) #Ser bättre ut med logamitrering

par(mfrow=c(2,2)); plot(modell) #residualgraf av tvåvägs-ANOVAn

anova(modell) #Antidot och dos har en påverkan men samverkar inte

TukeyHSD(aov(modell))

par(mfrow=c(1,1))
boxplot(antidot$blod~antidot$antidot*antidot$dos) 

# Slutsats: Det är en signifikant påverkan från båda antidoterna och skillnad mellan de båda
# antidoterna. Det är signifikant skillnad mellan alla dosniåver utom dos 20 mot dos 15.

#### Fråga 10 - Matris ####

r1<-c( 0     , 0     , 0     , 3.1   , 5.4   , 5.2   , 2.3   )
r2<-c( 0.2   , 0     , 0     , 0     , 0     , 0     , 0     )
r3<-c( 0     , 0.3   , 0     , 0     , 0     , 0     , 0     )
r4<-c( 0     , 0     , 0.8   , 0     , 0     , 0     , 0     )
r5<-c( 0     , 0     , 0     , 0.9   , 0     , 0     , 0     )
r6<-c( 0     , 0     , 0     , 0     , 0.8   , 0     , 0     )
r7<-c( 0     , 0     , 0     , 0     , 0     , 0.7   , 0     )

#Lagring av lesliamatrisen i variabeln L
L <-rbind(r1,r2,r3,r4,r5,r6,r7)

#eigen() ger egenvärden och egenvektorer som utdata
egen <- eigen(L)

#Dominanta egenvärdet, reslutatet av egenvärdet
egen$values

#Reella delen av det dominanta egenvärdet
Re(egen$values[1]) #Ger oss 0.91

# a) Populationen minskar med 8.7%

# b) Den stabila åldersfördelningen 
fordelning <- egen$vectors[,1]

Re(fordelning) #Relavtiva fördelningen, Re står för realdel. 
relativ_fordelning <- fordelning/sum(fordelning)
Re(relativ_fordelning*100)

#Åldersklass   %
#   1:      66.085843 
#   2:      14.483802  
#   3:      4.761546  
#   4:      4.174285  
#   5:      4.116886  
#   6:      3.609134  
#   7:      2.768504 

# c) Ändra överlevnadschanser m.m.

#Strategi 1
L_mod1 <- L
L_mod1[2,1] <- 0.4 #Sätter värdet i andra kolumnen först i raden tll 0.4
egen<-eigen(L_mod1)
max(Re(eigen(L_mod1)$values)) #Egenvärdet

#Strategi 2
L_mod2 <- L
L_mod2[4,3] <- 0.95 
L_mod2[5,4] <- 0.95 
L_mod2[6,5] <- 0.95 
L_mod2[7,6] <- 0.80 

egen<-eigen(L_mod2)
max(Re(eigen(L_mod2)$values)) #Egenvärdet

# Strategi 1 ger ett egenvärde på 1,04
# och Strategi 2 ger ett på 0,97.

# Slutsatsen blir att Strategi 1 är den
# som bör väljas



#### Fråga 11 - Tvåvägs ANOVA #### 

platorch$genotyp<-as.factor(platorch$genotyp) #Variabeln genotype som text ersätts med variabeln genotype som omvandlats till faktor
modell<-lm(mpi~genotyp*kon, data=platorch)
par(mfrow=c(2,2)); plot(modell) #residualgraf av tvåvägs-ANOVAn
anova(modell)

#Resultat:
# Effekt av genotyp: p = 0,7032
# Effekt av kön: p = 0,4538
# Interaktion: p = 0,7063

#  Varken genotyp eller kön påverkar halterna av Mpi, och det finns heller ingen effekt på Mpi
#  av en interaktion mellan dessa faktorer.


#### Fråga 12 - Wilcox test mellan två grupper med två staplar med värden ####

#Vi ska jämföra värden från två grupper, och vi har information om att de kommer från skeva
#fördelningar. Vi tillämpar Wilcoxon-Mann-Whitney

wilcox.test(polymorf$fst~polymorf$klass) # ~ för att kunna använda grupperna

#p-value = 0.8689. Alltså finns inget samband
# Det finns inget stöd i dessa data för att polymorfismer hos ostronen där man studerat
# DNA skiljer sig från dem där man använt proteindata.
#### Fråga 13 - Chi 2 test ####
matris<-rbind(sten,spring) #Behöver en matris för chi2
sten <- c(48, 40, 198, 244)
spring <- c(8, 11, 40, 86 )
chisq.test(matris) 
matris

#Går även at köra
chisq.test(antilop)

#p = 0,03058. De två arterna fördelar sig inte lika mellan de fyra områdena


#### ============================================================================ ####
#                                  TENTAMEN 2017
#### ============================================================================ ####

#### ============================================================================ ####
#           FRÅGA 1 - SAMBAND MELLAN PINNARS LÄNGD, BLADAREA OCH VIKTFÖRÄNDRING
#### ============================================================================ ####

## a) Undersök om det finns ett samband mellan pinnarnas initiala längd och uppäten bladarea.

# Data antas vara normalfördelade så vi kör parametrisk test. Samband undersöks med 
# korrelationstest eller eventuellt regression. Vi väljer korrelation i detta fall 
# eftersom vi nöjer oss med att se om det finns ett samband.

cor.test(sticks$length_before,sticks$area_eaten) 
plot(sticks$length_before,sticks$area_eaten)

## Slutsats: Korrelationstestet visar på ett signifikant (p=0.0012) samband mellan 
# pinnarnas initiala längd och uppäten bladarea. Vi ser att korrelationen är positiv 
# (r=0.48) vilket även kan skönjas i figuren. Detta innebär att förklaringsgraden är 23% då 0.48^2 = 0.23.
# Vi antar därför att längre pinnar konsumerade större bladyta även om det bara är 23 % 
# av variationen som förklaras.

## b) Finns ett samband mellan uppäten bladarea och pinnarnas viktförändring?

#Vi undersöker sambandet mellan viktförändring och uppäten bladyta. 
#Vi måste då beräkna viktförändring som vikt i slutet minus vikt i början.
sticks$vikt<-sticks$weight_after-sticks$weight_before

#Efter det går vi vidare med ett korrelationstest
cor.test(sticks$vikt,sticks$area_eaten) 
plot(sticks$vikt,sticks$area_eaten)

## Slutsats: Korrelationstestet visar på ett signifikant (p<0.001) samband mellan 
# pinnarnas viktförändring och uppäten bladarea. Vi ser att korrelationen är 
# positiv (r=0.55) vilket även kan skönjas i figuren. Slutsatsen är att större 
# viktökning är kopplat till mer konsumerat blad.

## c) Baserat på dessa resultat, kan vi anta att pinnarna klarar sig på Saintpaulia under vintern?

# Slutsats: På kort sikt, dvs de två veckor som pinnarna studerades i detta försök, 
# såg vi framför allt att de ökade i vikt. Detta tyder på att saintpaulia låter dem växa 
# och därmed överleva. Vikt är här ett bättre mått än längd därför att även svältande individer 
# kan fortsätta växa i längd utan mat under en kortare period genom att ta av energireserver. 
# Hade sambandet mellan uppäten area och viktförändring varit negativt hade det kunnat tyda 
# på att saintpaulia var toxiskt och större konsumtion skulle då kunna leda till en mindre ökning 
# eller till och med minskning i vikt.

# Vad som händer över en längre period kan vi egentligen 
# inte uttala oss om eftersom det kan uppstå brist på viktiga ämnen som inte märks inom en 
# kortare period. Men vi antar tillsvidare att pinnarna klarar sig på saintpaulia under vintern 
# tills en längre studie genomförts. (FYI: de klarar sig bra på saintpaulia även om de föredrar 
# de andra växterna).

# RÄTTNING:
# a) korrekt typ av analys av rätt variabler (linjär regression eller korrelation) 0.5p , 
#     korrekt tolkning 0.5p
# b) räknat ut viktskillnade 1p, korrekt typ av analys av rätt variabler 
#     (linjär regression eller korrelation) 0.5p , korrekt tolkning 0.5p
# c) korrekt dragen slutsats eller diskussion från resultat på delfråga a och b

#### ============================================================================ ####
#                        FRÅGA 3 - DISKRET LOGISTISK TILLVÄXT
#### ============================================================================ ####

## a) Formel för diskret logistisk tillväxt 

#Funktionsdefinition med namnet dlt
dlt <- function(n,r,k) { #Definerar funkitonen med tre variabler som argument
  n_ny <- n + r*n*(1-n/k) #Diskret logistiskt tillväxt
  return(n_ny) #Retunerar värdet till den anropande funktionen
}

## b) Graf över populationsutveckling

#Defenition av startvärden
n<-c()
n[1]<-70 #Startvärde för n
r<-0.04 #Maximal perkapitareproduktion
k<-2400 #Bärkraft, Ekosystem med en bärkraft för 2400 individer.
sluttid<-300 #Vi vill följa dem under 25 månader framöver

#Forloop för de nästa 25 månaderna
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av n mot t
plot(t,n, type="l", #Type=l för en linje ist för prickar
     lwd=2, #Lwd står för line width
     ylim=c(0,2500), #Ett spann på yt värdet mellan 0-65 000
     xlab='Generationer', 
     ylab='Populationsstorlek')

## c) När nås 90% av bärkraften

which(n>0.9*k)[1]
#Det tar knappt 145 månader (eller drygt 12 år) innan populationen uppnått 90% av öns bärförmåga.

#RÄTTNNING:
# a) 1p för korrekt formel, oavsett form den skrivits i
# b) 1p metod redovisad, 1p graf ritad
# c) 0,5p metod, 0,5p korrekt svar


#### ============================================================================ ####
#                        FRÅGA 5 - WILCOX FÖRE OCH EFTER KOFFEIN
#### ============================================================================ ####

## a) Adekvata hypoteser 
#Nollhypotesen H0 är att det inte finns skillnader i median mellan mätvärdena för 
#   myokardiellt blodflöde innan och efter tillsats av koffein.
#Alternativhypotesten H1 är att det finns skillnad i median mellan mätvärdena för 
#   myokardiellt blodflöde innan och efter tillsats av koffein.

## b) Förutsättnign för test
#Vi har ett litet stickprov och känner inte till fördelningen som dessa stickprov kommer ifrån. 
# Vi kan alltså inte anta att de kommer från en normalfördelning och de få stickproven 
# gör att vi inte heller kan använda dem för att se om de verkar normalfördelade. 
# Vi använder därför icke-parametriska test som inte antar att data inte följder 
# normalfördelningen. Vi noterar att mätningar skett på samma individer för och efter 
# de getts en dos koffein.

## c) Lämpligt statistiskt test (Wilcox)
# Med anledning av ovanstående resonemang gör vi ett parat Wilcoxon-test. 
wilcox.test(koffein$fore,koffein$efter, paired = T)

#Testet varnar för ties, det vill säga att flera värden är lika. Vi undersöker därför om 
# dessa utgör en stor del av våra mätningar. Eftersom vi har så få värden kan vi granska dessa direkt

koffein$fore-koffein$efter
0.71 0.14 1.31 0.49 0.49 -0.04 -0.04 -0.02 0.45 0.12

#Vi ser att det finns två skillnader på 0.49 och två på -0.04 vilket utgör 20% av alla mätpar. 
#Vi har detta iåtanke när vi tolkar resultaten. Vi noterar även att p = 0.032 vilket är 
#relativt nära 0.05. Medianen för värden före och efter
median(koffein$fore) median(koffein$efter)

2.41 2.21
#Visar att medianen är lägre efter koffeintillsatsen.

#Slutsats: Vi såg att tillsats av koffein minskade det mycardiella blodflödet, 
# men noterar att vi hade ett litet stickprov och inte helt klara resultat. Vi ser detta
# som en intressant pilotstudie och det skulle vara meningsfullt att genomföra ytterligare en
# studie men med flera försökspersoner.

#### ============================================================================ ####
#                        FRÅGA 6 - REAGENS PÅ KÖLDRESPONS OCH KÖN
#### ============================================================================ ####

## a)

#Vid antagande av  att data är normalfördelade och anger därför hypoteser i termer av 
# medelvärden. Vi kommer att testa tre hypoteser som vi här bakar in i en och samma mening:

# Nollhypotesrena är att det inte finns någon effekt av kön (H0a) eller art 
# (H0b) på mängden alanin, liksom inte heller någon interaktion mellan dessa två faktorer (H0c). 
# Alternativa hypoteserna är att kön (H1a) och/eller art (H1b) inverkar på mängden alanin och/eller
# att de två faktorerna interagerar i sin inverkan.

## b) 
#Respons = Alanine, vilket är numeriska värden. 
#Faktorer = art och kön 

#Vi undersöker här om alaninvärden verkar följa en normalfördelat. Vi gör en linjär 
# modell som en tvåvägs-ANOVA med faktorerna sex och species och undersöker residualerna.
alana<-lm(alanin~species*sex, data=amino) 
par(mfrow=c(2,2))
plot(alana)

#Om det ser bra ut sammanställer vi en anova-tabell på vår linjära modell.
anova(alana)

#Vi tittar först på interaktionen mellan art och kön och ser att den inte är signifikant. 
# Vi tittar därför på de enskilda faktorerna och noterar att både art (p=0.0005) och (p<0.001) 
# kön inverkar på halterna av alanin. Vi behåller därför hypotes H0c men antar tillsvidare 
# hypoteserna H1a och H1b.

# Vi kan nu undersöka mellan vilka arter som halterna av alanin skiljer sig och på vilket 
# sätt, liksom om hanar eller honor har de höge halterna. Vi kör en interaktionsplot:
interaction.plot(amino$species, amino$sex, amino$alanin,fun=mean)

#Här kan vi se att hanar generellt har högre halten än honor. Det verkar även som om 
# art a ligger högre än art c som ligger högre än art b. Men eftersom plotten in visar
# hur stor osäkerhet som omger medelvärdena gör vi parvisa test med hjälpa av Tukeys 
# metod. Eftersom interaktionen i huvudtestet inte var signifikant gör vi Tukeys test 
# på den additiva modellen (de vill säga vi bortser från faktorn species:sex)
TukeyHSD(aov(amino$alanin~amino$species+amino$sex))

# Vi ser från den översta listan på parvisa skillnader mellan arterna att art a är större 
# än art b (skillnaden b-a är negativ och signifikant, p = 0.0002), 

# att art c är mindre än a (c-a är negativ och signifikant, p = 0.036)
# Skillnaden mellan c och b är så liten att vi tillsvidare antar att det är lika (p=0.08).

## d) Slutsats: Vi fann att medelhalterna av alanin i tre arter av tusenfotingar inte 
# interagerade med kön. Hanar hade generellt högre halter alanin än honor och art a 
# hade generellt högre halter än art b och c. Skillnaden i halter mellan art b och c 
# var inte tillräckligt stora för att vi skall anse de skilda åt.

#RÄTTNING:

# a) korrekta hypoteser 1p, saknas någon komponent 0,5p
# b) korrekt test med motivering 2p, saknas någon del av testet eller del av motivering avdrag 0,5p
# c) korrekt tolkning av resultat från adekvat test 2p, saknas del av test eller motivering avdrag 0,5p



#### ============================================================================ ####
#                                  FRÅGA 7 - CHI 2 TEST 
#### ============================================================================ ####


## a)
#Frågeställningen gäller om det finns effekter av mänsklig aktivitet på förekomsten 
# av gemsbock och hartebeest och om dessa två arter reagerar olika på aktiviteten. 
# Vi har bara tillgång till data i tabellen och dessa verkar vara antal observerade 
# individer. Eftersom vi inte har någon replikation utan bara ett värde för varje 
# kombination av art och markanvändningsområde kan vi inte använda någon analysmetod 
# som bygger på variansanalys. Då väljer vi chi2 test. 

gem <-c(5,7,12,67)
red <-c(6,8,54,13)
matris<-rbind(gem,red)

#Ett chi2-test av varianten oberoendetest ger oss om ett finns anledningar 
# att tro att antalet observerade individer skiljer sig mellan arterna mellan 
# områdena. Detta gör vi med chisq.test()
chisq.test(matris)

# P-värdet är mindre än 0.05 så vi antar att det finns skillnader i förekomsten av 
# de två arterna mellan de olika områdena.

# Slutsats: Chi2-testet visar att arterna förekommer i olika antal inom de olika 
# områdena och en möjlig förklaring är att mänskliga aktiviteten påverkar förekomsten 
# av arterna i olika grad.

#RÄTTNING:

# 1p valt rätt typ av test (chi2)
# 2p ställt upp rätt sorts chi2-test, redovisat metod
# 1p korrekt tolkning av resultaten utifrån de förutsättningar man känner till


#### ============================================================================ ####
#                                  FRÅGA 8 - MATRIS
#### ============================================================================ ####

## a) 

#Övergångsmatrisen skapar vi genom att sätt värdet för fekunditeten i översta raden,
#överlevnad på diagonalen och tillväxten på platsen under diagonalen.

#Övergångsmatris:
r1<-c( 0     , 0.004 , 0.1   , 0      )
r2<-c( 0.86  , 0.89  , 0     , 0      )
r3<-c( 0     , 0.07  , 0.89  , 0      )
r4<-c( 0     , 0     , 0.04  , 0.94   )

L <-rbind(r1,r2,r3,r4)

## b) 
# Den stabila åldersfördelningen får vi genom att ta fram egenvektorerna med eigen(leslie). 
# Genom att specificera anropet kan vi skala bort de delar som vi inte är intresserade av. 
# För att bara extrahera de första egenvektorerna skriver vi därför

eigen(L)$vectors[,1]/sum(eigen(L)$vectors[,1])*100 #Stabila åldersfördelningen ges av det dominanta egenvärdet

#3.020786 32.252049 28.028135 36.699029 %

## c)
# Populationen minskar med
eigen(leslie)$value[1]
#0.9705492

#Populationen minskar med 2.95% per år. 
#Slutsats del c: Populationen minskar med 2.95% per år.

## d)
#Strategi 1:
L1<-L 
L1[2,1]<-0.95

Egenvärdet för denna matris är då 
eigen(L1)$value[1] 
# Där vi är intresserade av realdelen, det vill säga 0,9745... 
### Detta motsvarar en populationsminskning med 2,5% per år för strategi 1.

#Strategi 2:
L_mod2 <- L # Skapa en kopia att modifiera
L_mod2[2,2] <- 0.95
L_mod2[3,3] <- 0.95 #Sätter värdet i andra kolumnen i andra raden tll 0.95
max(Re(eigen(L_mod2)$values)) #Egenvärdet
# Får 1.028208
###Detta motsvarar en populationstillväxt med 2.8% per år i strategi 2.

#Slutsats: Av de två alternativa strategierna leder 1 till en minskning på 2,5% 
# per år medan strategi 2 leder till en populationstillväxt med 2.8%. Strategi 2 är 
# klart bäst för populationen.

