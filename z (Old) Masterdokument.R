#### ============================================================================ ####
#                               MASTERDOKUMENT I KVB
#### ============================================================================ ####




#### ============================================================================ ####
####                        QUIZ 1 - INTRO OCH FIBONACCI                          ####
#### ============================================================================ ####

######## DEL 1 - INTRODUKTION TILL RSTUDIO ####
langd<-c(172,190,161,176,168) #Lista med värden

langd<-langd/100 #Omvandling av data till m

sum(langd)/5 #Medellängden 

vikt<-c(69,90.5,55,72,57) #Lista av viktvärden
medelvikt<-mean(vikt) #Lagring i ett värde

plot(langd,vikt,pch=16,col="blue") #Plot av lngd mot vikten

######## DEL 2 - FIBONACCI ####

#### For-loop: Samma sak flera gånger #### 
for (hej in 1:5){ print(hej) } #5 första tal i en serie

#10 första värdena i en fibonacciloop. 
y<-c() #Tom lista
y[1]<-1 #Värde 1 i en indexklammer
y[2]<-1 #Värde 2 i en indexklammer
for (i in 3:10) {
  y[i]<-y[i-1]+y[i-2]
}
#y[i] är summan av de två föregående talen.

#25 första värdena i en fibonacciloop. 
y<-c() #Tom lista
y[1]<-1 #Värde 1 i en indexklammer
y[2]<-1 #Värde 2 i en indexklammer
for (i in 3:25) {
  y[i]<-y[i-1]+y[i-2]
}

y[1:25] #25 Första värdena i Fibonacciloopen

plot(y, ylab="Fibonaccital", xlab="n") #Plot över Fibonaccital

plot(log(y), ylab="Fibonaccital", xlab="n") #Logaritmerad variant

#### Kvotberäkning #### 
kvot<-c()
for(i in 2:10) { #Start vid 2 
  kvot [i]<-y[i]/y[i-1] #Div. med aktuella talet för att få kvoten
}

plot(kvot, ylab="Kvot aktuellt/Föregående", xlab="n",) #Plot över kvoten
lines(kvot) #Tillägg av Linjer
#Kvotens stabilisation innebär att Fibonaccitalen konvengerar mot samma tal
#Kvoten som konvengear inte talen själva





#### ============================================================================ ####
####           QUIZ 2 - LÄSA IN, BEARBETA DATA OCH BINOMIALFÖRDELNING             ####
#### ============================================================================ ####

######## DEL 3 & 4 - LÄSA IN OCH BEARBETA DATA ####
#### Inläsning av data ####
read.csv("Data_diabetes")
diabetes <- read.csv2(file.choose())
diabetes<-read.csv2("diabetes.csv") 
diabetes <-read.csv(file.choose(),sep=";", dec=",", header=T)
#Tänk på att det är det svenska formatet, andra behöver komman 
#Om du nu skriver in diabetes i console kommer alla värden komma upp

#Ändra en viss variabel i en dataframe:
dataframe$variabel

#Ändra från tum till cm
diabetes$height<-diabetes$height*2.54

#Midja till cm
diabetes$waist<-diabetes$waist*2.54

#Höft till cm
diabetes$hip<-diabetes$hip*2.54

#Pund till kilo
diabetes$weight<-diabetes$weight/2.2

head(diabetes$height) #6 första värdena, för att få en överblick.
diabetes$height #För alla

#Ny kolumn för BMI värdet
diabetes$BMI<-diabetes$weight/(diabetes$height/100)^2

#Spannet på värden i variablen
range(diabetes$BMI)

#### Medelvärde, Median och Standardavvikelse ####
mean() #Medelvärde
median() #Median
sd() #Standardavvikelse

mean(diabetes$height) #Medelvärde av längd i cm
median(diabetes$weight) #Median av vikt i kg
sd(diabetes$stab.glu) #Standardavvikelse för variabeln för stabilt glukos i mg/dL

#### Medelvärdets medelfel (måste beräknas) ####
sd(x)/sqrt(length(x)) 
#x byts ut till den vektor vi vill beräkna felet hos
#length(x) är hur många värden som finns i vektorn x o
#sqrt() tar kvadratroten ur värdet som anges inom parentesen

#Midjemåttens medelfel
sd(diabetes$waist)/sqrt(length(diabetes$waist)) 

#### Binomalfördelning ####

#Avläsning av binomialdata gör lättas i tabeller. 
#Ex Vi kan se hur många av försökspersonerna som är från vardera stad och av respektive kön.
#Det finns två städer och två kön i studien och tillhör man den ena gruppen kan man inte tillhöra den andra. 
#Vi tar bara hänsyn till grupperna som studien använde sig av och inte faktum att kön är en mer komplex benämning än bara man och kvinna

#Tabellen skapas med kommandot table(). 
#Fördelningen av personer från de två städerna och deras kön görs genom att köra:
table(diabetes$location) #Tabell av antal personer från städerna
table(diabetes$gender) #Tabell över antal personer av könen

table(diabetes$location, diabetes$gender) #Tabell över antal personer av könen i städerna



#### ============================================================================ ####
####                  QUIZ 4 - SKILLNADER MELLAN TVÅ GRUPPER 1 & 2                ####
#### ============================================================================ ####

######## DEL 1 - SKILLNADER MELLAN TVÅ GRUPPER 1 ########
#Adekvat nollhypotes = Ingen skillnad på medelvärdena
#Alternativa nollhypoteser = Medelvärdena avviker från varandra

#inläsning av data
zn <- read.csv2("sökväg\zntill.csv")
zn <- read.csv2(file.choose())
library(plotrix) #GLÖM INTE PLOTRIX NÄR DU VILL PLOTTA

#Histogram och QQ plot av plasmakoncentration av Zn
hist(zn$ktrl_p) #Histogram
qqnorm(zn$ktrl_p, col="red", pch=16) #QQplot
qqline(zn$ktrl_p)#QQ linje

#Histogram och QQ plot av tillväxthämmning 
hist(zn$tih_p) #Histogram
qqnorm(zn$tih_p, col="red", pch=16) #QQplot
qqline(zn$tih_p)#QQ linje

#### oparat t-test och t.test med Welch modifikation ####
t.test(grupp1, grupp2,var.equal=T) # antagande om samma varians, om populationsvarianserna är lika använder vi t-test
t.test(grupp1, grupp2).    #om inte används Welch variant

#T-test mellan två kontrollgrupper genom welch metod. Får fram p värdet
t.test(zn$ktrl_p,zn$tih_p)
#P ärdet = 0,04291
#P värdet är sannolikheten att få det observerade utfallet eller något 
#som avviker ännu mer från nollhypotesen är 4,3 %
#Om den uträknade sannolikheten (p-värdet) är mindre än 5 % förkastas nollhypotesen till förmån för den alternativa.
#Pga avvikelsen kan nollhypotesen förkastas

#### Felplot av ktrl och tih####
plotCl()

#Medelvärde för ktrl och tih. Ska användas till felplot
medel_ktrl<-mean(zn$ktrl_p)
medel_tih<-mean(zn$tih_p)
medel<-c(medel_ktrl,medel_tih)

#Gränserna för konfidensintervallet
test_ktrl <- t.test(zn$ktrl_p)
test_tih <- t.test(zn$tih_p)

#Nedre gränsen av konfidensintervallet för ktrl_p
test_ktrl$conf.int[1] 

#Nedre och övre gränser för våra två konfidensintervall
nedre_ktrl<-test_ktrl$conf.int[1]
nedre_tih<-test_tih$conf.int[1]
nedre<-c(nedre_ktrl, nedre_tih)

ovre_ktrl<-test_ktrl$conf.int[2]
ovre_tih<-test_tih$conf.int[2]
ovre<-c(ovre_ktrl, ovre_tih)

#Felplot av medelvärden och tillsnyggning 
plotCI(x, medel, ui=ovre, li=nedre, 
       xlim=c(0.5,2.5), 
       ylim=c(0,1.2), 
       pch=16, 
       col="blue", 
       xlab="Grupp", 
       ylab="Zn-koncentration", 
       xaxt="n"
)

axis(1,x,c("Kontroll","Behandling"))


######## DEL 2 - SKILLNADER MELLAN TVÅ GRUPPER 2 ########

ld <- read.csv2(file.choose())  #Inläsning av filer
library(plotrix)                #Inläsning för plot

#Ok diagram för hist och normalfördelning
hist(ld$diff) #Histogram för diff
qqnorm(ld$diff, col="red", pch=16) #QQplot
qqline(ld$diff)#QQ linje

#t-test som grundas på normalfördelning. Differensen i resultaten mellan före och efter behandling kan komma från en normalfördelad population
?t.test

t.test(ld$before , ld$after, paired=T)
#Givet att nollhypotesen är sann, är detta sannolikheten för att få det utfall vi fått, eller något mer extremt.

hg <- read.csv2(file.choose())

#Om ett diagram ser för skevt ut, ex med fr avvikande svansar är det bättre att övergå till en icke parametrisk metod.

#### Wilcox test: Icke parametriska test ####
wilcox.test(hg$before, hg$after,paired = T)

#pga att datan inte är normalfördelad och värdena är lite skeva är de bästa att använda en boxplot
boxplot(hg$before, hg$after)






#### ============================================================================ ####
####          QUIZ 5 - DISKRET LOGISTISK TILLVÄXT OCH SÅRBARHETSANALYS            ####
#### ============================================================================ ####

######## DEL 1 - D.L: FUNKTIONSDEFNITIONER OCH GRAFER ########

#Namnet är kubik.
#Den tar variabeln x som indata.
#Den returnerar x i kubik.
kubik <- function (x) { return(x^3) }

#Anropa funktionen och beräkna  7^3 och sedan lagra det i variabeln svar
svar<-kubik(7)

#### EXEMPEL - Blomväxtarts etablering i ett område under 30 år ####

#Funktionsdefinition med namnet dlt
dlt <- function(n,r,k) { #Definerar funkitonen med tre variabler som argument
  n_ny <- n + r*n*(1-n/k) #Diskret logistiskt tillväxt
  return(n_ny) #Retunerar värdet till den anropande funktionen
}

#Defenition av startvärden
n<-c()
n[1]<-10 #Startvärde för n
r<-0.4 #Maximal perkapitareproduktion
k<-1000 #Bärkraft, Ekosystem med en bärkraft för 1000 individer.
sluttid<-30 #Vi vill följa dem under 30 år framöver

#Forloop för de nästa 30 åren
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av n mot t
plot(t,n, type="l", #Type=l för en linje ist för prickar
     lwd=2, #Lwd står för line width
     ylim=c(0,2000)) #Ett spann på yt värdet mellan 0-2 000


######## DEL 2 - DETERMINISTISKA OCH STOKASTISKA MODELLER ########

#### EXEMPEL: En bättre art som pollinerar som höjer r till 0.7 ####

#Defenition av startvärden
n<-c()
n[1]<-10 #Startvärde för n
r<-0.7 #Maximal perkapitareproduktion (denna gång 0.7)
k<-1000 #Bärkraft, Ekosystem med en bärkraft för 1000 individer.
sluttid<-30 #Vi vill följa dem under 30 år framöver

#Forloop för de nästa 30 åren
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av n mot t
lines(t,n, type="l", #Tillägg av en linje i ploten (körs efter den tidigare)
      lwd=2, #Lwd står för line width
      col="blue", #Blå linje
      ylim=c(0,2000)) #Ett spann på yt värdet mellan 0-2 000

#### EXEMPEL: Graf för vid start med en störrre population ####

#Tillägg av en större population
n<-c()
n[1]<-3000 #Startvärde för n, denna gång 3000
r<-0.4 #Maximal perkapitareproduktion
k<-1000 #Bärkraft
sluttid<-30 #Vi vill följa dem under 30 år framöver

#Forloop för de nästa 30 åren
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1) #addering av forloop i värde

lines(t,n, type="l", lwd="2", col="blue", ylim=c(0,2000)) #tillägg av linje

#### EXEMPEL: Maximal perkapitareproduktion kan leda till periodiskt beteende ####
n<-c()
n[1]<-10 #Startvärde för n
r<-2 #Maximal perkapitareproduktion (2) och 2.5 för fyra stabila punkter
k<-1000 #Bärkraft, Ekosystem med en bärkraft för 1000 individer.
sluttid<-60 #Vi vill följa dem under 30 år framöver

#Forloop för de nästa 30 åren
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av n mot t
plot(t,n, type="l", #Type=l för en linje ist för prickar
     lwd=2, #Lwd står för line width
     ylim=c(0,1200)) #Ett spann på yt värdet mellan 0-2 000

#### EXEMPEL: Individer kommer till ett nytt område och etableras, med med en bestämd bärkraft ####

#Funktionsdefinition med namnet dlt
dlt <- function(n,r,k) { #Definerar funkitonen med tre variabler som argument
  n_ny <- n + r*n*(1-n/k) #Diskret logistiskt tillväxt
  return(n_ny) #Retunerar värdet till den anropande funktionen
}

#Defenition av startvärden
n<-c()
n[1]<-17 #Startvärde för n
r<-0.32 #Maximal perkapitareproduktion
k<-450 #Bärkraft, Ekosystem med en bärkraft för 1000 individer.
sluttid<-25 #Vi vill följa dem under 25 år framöver

#Forloop för de nästa 25 åren
for(i in 1:sluttid) {
  n[i+1] <- dlt(n[i],r,k)
} 

t<-1:(sluttid+1)

#Plot av n mot t
plot(t,n, type="l", #Type=l för en linje ist för prickar
     lwd=2, #Lwd står för line width
)

abline(h=0.9*k)
which(n>0.9*k) #När bublyerna har nått 90% av bärkraften (talet innan det första)

which(n>k*0.9)
which(n>405)
which(n>k*0.9)[1]
which(n>405)[1]


#### EXEMPEL: Histogram & Stokastisk modellering av invaderande bulbyler ####
#Parametrarna r och k varierar slumpmässigt - antingen var och en för sig eller båda tillsammans
# rnorm(antal,my,sd) kommer användas. antal = hur många nr vi vill använda

for(i in 1:sluttid) {
  R <-rnorm(1000, 0.32, 0.30) #Plus/minus två standardavvikelser täcker ungefär 95 % av hela fördelningen
  n[i+1] <- dlt(n[i],r,k)
  
} 

t<-1:(sluttid+1)

#Histogram av normalfördelningen rnorm
hist(R, col=rainbow(14))

#### EXEMPEL: Stokastisk modellering av invaderande bulbyler ####

#Defenition av startvärden
n<-c()
n[1]<-17 #Startvärde för n
r <- 0.32 #Maximal perkapitareproduktion
k<-450 #Bärkraft
sluttid<-50 #Vi vill följa dem under 50 år framöver

for(i in 1:sluttid) {
  k<-rnorm(1 , 450, 50)
  r<-rnorm(1, 0.4, 0.18)
  n[i+1] <- dlt(n[i],r,k)
  
}

t<-1:(sluttid+1)

plot(t,n, ylim = c(0,600), type="l")


######## DEL 3 - SÅRBARHETSANALYS ########

#### Stokastisk modellering, risken för lokalt utdöende hos den studerade populationen #### 
dlt <- function(n,r,k) { #Definerar funkitonen med tre variabler som argument
  n_ny <- n + r*n*(1-n/k) #Diskret logistiskt tillväxt
  return(n_ny) #Retunerar värdet till den anropande funktionen
}

n<-c()
n[1]<-50 #Startvärde för n
#r <- 0 #Maximal perkapitareproduktion
k <- 1000 #Bärkraft
sluttid<-20 #Vi vill följa dem under 30 år framöver
under <-0 #räknar hur många gånger vi hamnar under 10

# if (villkor) {kod som körs om det är sant}


#Forloop för 10 000 försök 
for (forsok in 1:10000) {
  
  for(i in 1:sluttid) {
    r<-rnorm(1, 0, 0.25)
    n[i+1] <- dlt(n[i],r,k)
  }
  if(min(n) < 10) {under <- under+1} #min(n) ger koll på populationens minsta värde under varje försök 
}
# placera counterfor-loopen innanför yttre forloopen. 
# Varje gång körningen gjorts för att se om populationen har dött ut
# kommer if att avgöra om den ska räknas som att populationen har dött ut eller inte
# Om det är fallet så adderas 1 

t<-1:(sluttid+1)

plot(t,n, ylim = c(0,200), type="l")

#Risken för utdöende är 
under                         # 2714
2714/10000*100                # procent av antalet gång populationen dör ut  


for(i in 1:sluttid) {
  r<-rnorm(1,0.14,0.30)
  n[i+1] <- dlt(n[i],r,k)
}




#### ============================================================================ ####
####                  QUIZ 6 - MATRISER OCH NATURVÅRD & CHI2.TEST                 ####
#### ============================================================================ ####

######## DEL 1 - MATRISER OCH NATURVÅRD ########

#### Lesliematris ####
r1<-c( 0     , 0     , 0     , 0     , 127   , 4     , 80    )
r2<-c( 0.6747, 0.7370, 0     , 0     , 0     , 0     , 0     )
r3<-c( 0     , 0.0486, 0.6610, 0     , 0     , 0     , 0     )
r4<-c( 0     , 0     , 0.0147, 0.6907, 0     , 0     , 0     )
r5<-c( 0     , 0     , 0     , 0.0518, 0     , 0     , 0     )
r6<-c( 0     , 0     , 0     , 0     , 0.8091, 0     , 0     )
r7<-c( 0     , 0     , 0     , 0     , 0     , 0.8091, 0.8091)

#Diagonalen är sannolikheten att stanna
#Underdiagonal är sannolikheten att tillväxa

#Två möjliga sätt
#rbind(rad1, rad2, osv...)
#cbind(kolumn1, kolumn2, osv...)

#Lagring av lesliamatrisen i variabeln L
L <-rbind(r1,r2,r3,r4,r5,r6,r7)

#eigen() ger egenvärden och egenvektorer som utdata
egen <- eigen(L)

#Dominanta egenvärdet, reslutatet av egenvärdet
egen$values

#Reella delen av det dominanta egenvärdet
Re(egen$values[1])

#Egenvektorn = Den stabila stadiefördelningen vid olika delar av livsstadiet
egen$vectors #Kollar på våra vektorer
L[1, 5] # example of how to locate specific values in the matrix
L[, 3] # if you want a whole column, you just "matrix[,x]" or
#row "matrix[x,]

#Första kolumnen och lagring i fordelning
fordelning <- egen$vectors[,1]

Re(fordelning) # Re står för realdel

#Relavtiva fördelningen
relativ_fordelning <- fordelning/sum(fordelning)
Re(relativ_fordelning*100)

#skapar en kopia att modifiera
L_mod <- L

#Sätter värdet i andra kolumnen först i raden tll 1
L_mod[2,1] <- 0.92

#Tillväxtfaktorn = Genom beräkning av det dominanta egenvärdet

#Doninanta egenvärdet i ett svep
gf <- c() #nollställning 

#### EXEMPEL: Stegning i matrisen ####
#Plan 1 öka p(survival) i - Alla sköldpaddor överlever första stadiet
L_mod <- L
L_mod[2,1] <- 1
egen<-eigen(L_mod)
gf[1] <- Re(eigen(L_mod)$values[1])

#Plan 2 - Alla små unga exemplar överlever
L_mod <- L
L_mod[2,2] <- 0.9514
egen<-eigen(L_mod)
gf[2] <- Re(eigen(L_mod)$values[1])
gf #Dubbelkolla att allting fungerar

#Plan 3 - Alla stora unga exemplar överlever
L_mod <- L #nytt L för plan 3
L_mod[3,3] <- 0.9853  #nytt värde fö rad 2 kolumn 3
egen<-eigen(L_mod)
gf[3] <- Re(eigen(L_mod)$values[1]) #sätter in realvärdet i filen gf()

#Plan 4 - Alla sub vuxna överlever
L_mod <- L
L_mod[4,4] <- 0.9482
egen<-eigen(L_mod)
gf[4] <- Re(eigen(L_mod)$values[1])

#Plan 5 - alla nya könsmogna går till nästa stadie
L_mod <- L
L_mod[6,5] <- 1
egen<-eigen(L_mod)
gf[5] <- Re(eigen(L_mod)$values[1])

#Plan  - Alla första årets överflyttare överlever för att bli könsmogna
L_mod <- L
L_mod[7,6] <- 1
egen<-eigen(L_mod)
gf[6] <- Re(eigen(L_mod)$values[1])

gf

#Barplot av värdena 
barplot(gf)
farg <- rgb(0, (7:1)/7, 0)  #olika skalor av grönt 
barplot(gf, col=farg)
axis(1, c(1,2,3,4,5,6,7))
abline(h=1) #skapar en linje där (m,k) och sätter värden för linjen y=kx+m

######## DEL 2 - CHI 2 TEST (anpassningstest) ########
turtles<-c(2998,12459,2882,197,10,1,12) #Värden från verkligheten
relativ_fordelning <- c(0.2065519760, 0.6697304431, 0.1145746547, 0.0066208692, 0.0003628892, 0.0003106745, 0.0018484934)

chisq.test(turtles, p=relativ_fordelning) #Chi^2 test

#Defenition av nollhypotesen: Sannolikheten att få de data vi observerat, 
#eller något mer extremt, givet att nollhypotesen är sann.

#Matris, Detta ger en 4x2-matris
matris<-rbind(gem,rhb)
gem <- c(5,7,12,67)
rhb <- c(6,8, 54, 13)
chisq.test(198, p=244)
matris

G <- c(gem,rhb)
Kuk1 <-c(198,244)
Kuk2 <-c(244)
chisq.test(Kuk1)















#### ============================================================================ ####
####  QUIZ 7 - KORRELATION, REGRESSION OCH SKILLNADER MELLAN FLER ÄN TVÅ GRUPPER  ####
#### ============================================================================ ####

######## DEL 1 - KORRELATION ######## 
# Samvariation, det finns ett samband mellan x och y variabeln som 
# I detta fall finns det ett samband mellan nitrinivåerna 

#Nollhypotesen är att det finns ingen samvariation, mellan nitratnivåerna och kolonistorleken 

#### EXEMPEL: Undersökning av samvariation mellan nitratnivåerna och kolonistorleken hos crassa ####

x <-crassa$nitrit #I x sparas alla observerade nitritnivåer
y <-crassa$antal #I Y sparas alla observerade crassaantal

#Diagram för data över nitratnivåerna för Crassa, visar om datan är linjär
plot(x,y) #Plot av nitratnivåerna VS antalet crassa. Visar att datan inte är linjär
qqnorm(x);qqline(x) #qq plot över nitratnivåerna. Datan är fördelad
hist(x) #Histogram för nitratnivåerna. Visar om de är normalfördelat
boxplot(x,y) #Boxplot över nitratnivåerna. Indikation på att den inte följer normalfördelning men inte tillräcklig

#Korrelationstest -  A. crassa med Pearsons korrelation och P. raciborskii med Spearmans korrelation.
cor.test(crassa$nitrit,crassa$antal); 
cor.test(raci$nitrit, raci$langd, method = "spearman")
# Crassa-data är parametriska och default är Pearson korrelation, 
# medan raci-data är icke-parametrisk så vi måste ange Spearmans korrelation

#Variationen mellan mätningar förklaras av sambandet som vi just undersökt
#Förklaringsgraden är r2 eller rho2

#Pearson är ett känsligare test så ett lägre r kan ge signifikant jämfört med samma rho, 
#dock är skillnaden här för stor för att vara enda förklaringen



######## DEL 2 - REGRESSION ######## 
#Regression undersöker när en variablen påverkar en annan och båda variablerna är kontinuerliga

#Plot för att se hur sambandet ser ut nu
plot(NVDI$income, NVDI$plant, pch = 16) #plant vs income plant

#Regressionsmodell
regr<-lm(NVDI$plant~NVDI$income) #Att föredra
regr<-aov(plant~income, data=NVDI)
regr<-aov(NVDI$plant~NVDI$income)
regr<-lm(plant~income, data=NVDI) #Att föredra
regr

#Residualanalysen. 4 grafer i ett 2x2 nät
par(mfrow=c(2,2))

#Plot av regressionsanalysen. Fyra residualgrafer i ett fyrdelat fönster
plot(regr)

#Resultaten av regression
anova(regr) #Tabell med intressant information och p värde
summary(regr) #Tabell med intressant informaiton som p värde och regressionslinje
regr$coefficients #om man bara vill ha intercept och lutningskoefficienten

#Tolkning av summary
#P = 0,011
#intercept = 0,302
#lutningskoefficient = 9,5x10-7
#justerat r2 = 0,4089

par(mfrow = c(1, 1))     #Tillbaka till en graf

###### i frågan ######
x <-NVDI$plant
y <-NVDI$income
o <-NVDI$area

plot(
  NVDI$plant ~ NVDI$income,
  pch = 16,
  col = "blue",
  xlab = "Medelinkomst (kr/?r)",
  ylab = "NVDI",
  main = "Relation mellan Inkomst och NDVI",
  xlim = c(200000, 350000),
  ylim = c(0.50, 0.63)
) #plotting x vs y

clip(200000, 350000, 0.50, 0.63)  #creating max and min values
abline(regr, col = 'red') 

text(
  y,
  x,
  labels = o,
  pos = 4,
  #inserting text
  pch = 2,
  col = "blue",
  cex = 0.75
)

clip(0.40, 0.62, 200000, 350000)  # byt x och y till min och maxvärden för att alla namn skall synas
# byt x-pos, y-pos och namn mot rätt kolumner i NVDI, n kan

# anta värden 1, 2, 3 eller 4, testa dig fram till vilket du tycker blir bäst

summary(NVDI$plant) #för att hitta max och min värden
summary(NVDI$income) #finding min and max valuesSO















######## DEL 3 - SKILLNADER MELLAN FLER ÄN TVÅ GRUPPER (GRUPPERADE EFTER EN FAKTOR) ######## 


###### EXEMPEL: STEENBOK PER KILOMETER ÖVER OLIKA AREOR ###### 
#Lådogram över steenbok per kilometer över olika areor
boxplot(steenbok$ind_per_km~steenbok$area )

#### Utväljning av data gruppvis #### 
hist(steenbok$ind_per_km[steenbok$area=="cga"]) 

#### 4 histogram för varje område
hist(steenbok$ind_per_km[steenbok$area=="cga"]) #Histogram för cga
hist(steenbok$ind_per_km[steenbok$area=="fr"]) #Histogram för fr
hist(steenbok$ind_per_km[steenbok$area=="wma"]) #Histogram för wma
hist(steenbok$ind_per_km[steenbok$area=="np"]) #Histogram för np

par(mfrow = c(2,2)) #4 subgrafer, köres innan histogrammen

#QQ plot för de fyra områdena
qqnorm(steenbok$ind_per_km[steenbok$area=="cga"], pch = 1, frame = FALSE)
qqline(steenbok$ind_per_km[steenbok$area=="cga"], col = "steelblue", lwd = 2)

qqnorm(steenbok$ind_per_km[steenbok$area=="fr"], pch = 1, frame = FALSE, xlab="hallåeller")
qqline(steenbok$ind_per_km[steenbok$area=="fr"], col = "red", lwd = 2)

qqnorm(steenbok$ind_per_km[steenbok$area=="wma"], pch = 1, frame = FALSE)
qqline(steenbok$ind_per_km[steenbok$area=="wma"], col = "darkgreen", lwd = 2)

qqnorm(steenbok$ind_per_km[steenbok$area=="np"], pch = 1, frame = FALSE, xlab="go göteborgare")
qqline(steenbok$ind_per_km[steenbok$area=="np"], col = "purple", lwd = 2)

#Modell för resudialanalys
modell <- lm(steenbok$ind_per_km ~ steenbok$area) 

plot(modell) #plot av alla modeller

par(mfrow=c(2,2)) #Fyra grafer

#Resudalvariansen är större ju större medelvärdet är

#### Transformation av data genom logaritmering #### 
steenbok$log_ipkm <- log(steenbok$ind_per_km)

#### 4 histogram för varje område #### 
hist(steenbok$log_ipkm[steenbok$area=="cga"]) #Histogram för logcga
hist(steenbok$log_ipkm[steenbok$area=="fr"]) #Histogram för logfr
hist(steenbok$log_ipkm[steenbok$area=="wma"]) #Histogram för logwma
hist(steenbok$log_ipkm[steenbok$area=="np"]) #Histogram för lognp

par(mfrow = c(2,2)) #4 subgrafer, köres innan histogrammen

#### QQ plot för de fyra områdena #### 
qqnorm(steenbok$log_ipkm[steenbok$area=="cga"], pch = 1, frame = FALSE)
qqline(steenbok$log_ipkm[steenbok$area=="cga"], col = "steelblue", lwd = 2)

qqnorm(steenbok$log_ipkm[steenbok$area=="fr"], pch = 1, frame = FALSE, xlab="hallåeller")
qqline(steenbok$log_ipkm[steenbok$area=="fr"], col = "red", lwd = 2)

qqnorm(steenbok$log_ipkm[steenbok$area=="wma"], pch = 1, frame = FALSE)
qqline(steenbok$log_ipkm[steenbok$area=="wma"], col = "darkgreen", lwd = 2)

qqnorm(steenbok$log_ipkm[steenbok$area=="np"], pch = 1, frame = FALSE, xlab="go göteborgare")
qqline(steenbok$log_ipkm[steenbok$area=="np"], col = "purple", lwd = 2)

#### Modell för resudialanalys #### 
logmodell <- lm(steenbok$log_ipkm ~ steenbok$area) 

plot(logmodell) #plot av alla modeller

par(mfrow=c(2,2)) #Fyra grafer

#### ANOVA modell #### 
anova(logmodell)
#P värdet är mindre än 0,05 = förkasta nollhypotesen

#### Logtransformation, medelvärde, standardavvikelse och grafplot för ANOVA #### 
# Detta skript �r endast t�nkt att vara hj�lp till delen som ritar graferna till uppgiften.
# Graferna som ritas h�r anv�nder endast enkla kommandon,
# och d�rf�r blir ber�kningarna lite l�ngre �n vad man kan g�ra med mer avancerade kommandon eller loopar.

# F�r sj�lva testen ges inget skript, detta �r endast ett grafiskt komplement.

library(plotrix)  

# OBS! Detta skript kr�ver att dataframen steenbok �r laddad (finns i kalahari_block7.RData).
#steenbok<-read.csv2(file.choose()) # alternativ laddning av CSV

# logtransform av individer per km
steenbok$log_ipkm<-log(steenbok$ind_per_km)

# Ber�kna medelvärde och standardavvikelse
# dessa ber�kningar �r bara gjorda med klassisk klipp och klistra, och lite ers�ttningar

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

# Plotta graf som illustration till envägs ANOVA

plotCI(x = 1:4,
       y=medel,stdev, 
       xlab="Markanv�ndningstyp", 
       ylab="log (individer/km)",
       bty="n",                           # s�tter boxtype till "none", det vill s�ga ingen ruta runt grafytan
       xaxt="n"                           # s�tter x-axis type till "none", det vill s�ga ingen synlig x-axel
)

namn<-c("Fenced Ranches","Communal Grazing Areas","Wildlife Management Areas","National Park")
axis(1, at=1:4, labels=namn)

#### Tukey test #### 
TukeyHSD(aov(modell)) ##### Modeller

plot(TukeyHSD(aov(modell)))  #####Modeller

#### EXEMPEL: Om gräsets täckningsnivåer skiljer sig åt mellan 4 ställen #### 
steenbok$log_ipkm <- log(steenbok$ind_per_km)

log_grasscover <- log(grass$cover + 1) #Logaritmen av talet noll går inte, lägg till 1 till alla värden.

hist(log_grasscover[grass$area=="cga"]) #Histogram för cga
hist(log_grasscover[grass$area=="fr"]) #Histogram för fr
hist(log_grasscover[grass$area=="wma"]) #Histogram för wma
hist(log_grasscover[grass$area=="np"]) #Histogram för np

par(mfrow = c(2,2)) #4 subgrafer, köres innan histogrammen

#QQ plot för de fyra områdena
qqnorm(log_grasscover[grass$area=="cga"], pch = 1, frame = FALSE)
qqline(log_grasscover[grass$area=="cga"], col = "steelblue", lwd = 2)

qqnorm(log_grasscover[grass$area=="fr"], pch = 1, frame = FALSE, xlab="hallåeller")
qqline(log_grasscover[grass$area=="fr"], col = "red", lwd = 2)

qqnorm(log_grasscover[grass$area=="wma"], pch = 1, frame = FALSE)
qqline(log_grasscover[grass$area=="wma"], col = "darkgreen", lwd = 2)

qqnorm(log_grasscover[grass$area=="np"], pch = 1, frame = FALSE, xlab="go göteborgare")
qqline(log_grasscover[grass$area=="np"], col = "purple", lwd = 2)

#Modell för resudialanalys
loggrassmodell <- lm(log_grasscover ~ grass$area) 

plot(loggrassmodell) #plot av alla modeller

par(mfrow=c(2,2)) #Fyra grafer

#### Logaritmera responsvariabeln #### 

hist(grass$cover[grass$area=="cga"]) #Histogram för cga
hist(grass$cover[grass$area=="fr"]) #Histogram för fr
hist(grass$cover[grass$area=="wma"]) #Histogram för wma
hist(grass$cover[grass$area=="np"]) #Histogram för np

par(mfrow = c(2,2)) #4 subgrafer, köres innan histogrammen

#QQ plot för de fyra områdena
qqnorm(grass$cover[grass$area=="cga"], pch = 1, frame = FALSE)
qqline(grass$cover[grass$area=="cga"], col = "steelblue", lwd = 2)

qqnorm(grass$cover[grass$area=="fr"], pch = 1, frame = FALSE, xlab="hallåeller")
qqline(grass$cover[grass$area=="fr"], col = "red", lwd = 2)

qqnorm(grass$cover[grass$area=="wma"], pch = 1, frame = FALSE)
qqline(grass$cover[grass$area=="wma"], col = "darkgreen", lwd = 2)

qqnorm(grass$cover[grass$area=="np"], pch = 1, frame = FALSE, xlab="go göteborgare")
qqline(grass$cover[grass$area=="np"], col = "purple", lwd = 2)

#### Modell för resudialanalys #### 
grassmodell <- lm(grass$cover ~ grass$area) 

plot(grassmodell) #plot av alla modeller

par(mfrow=c(2,2)) #Fyra grafer

#Denna duger fortfarande inte, vi går vidare till...

###Kruskal-Wallistest - skillnader i medianer
kruskal.test(grass$cover ~ grass$area)

######## DEL 4 - SKILLNADER MELLAN FLER ÄN TVÅ GRUPPER (GRUPPERADE EFTER TVÅ FAKTORER) ######## 


#### EXEMPEL: Om en genetiskt modifierad laxs aptit regleras annorlunda än vildtyp med avseende på CCK #### 
#### Omvandling av data i textformat till faktorer ####
cck$genotype<-as.factor(cck$genotype) #Variabeln genotype som text ersätts med variabeln genotype som omvandlats till faktor
cck$season<-as.factor(cck$season)#Variabeln season som text ersätts med variabeln season som omvandlats till faktor

#### Normalfördelningsanalys #### 
hist(cck$level) #Histogram för leveln för normalfördelningsanalys
qqnorm(cck$level) #qqplot för level
qqline(cck$level) #qqline för level

loglevel <- log(cck$level) #Logaritmen ur level för en parametrisk metod

hist(loglevel) #Histogram för leveln för normalfördelningsanalys
qqnorm(loglevel) #qqplot för level
qqline(loglevel) 

table(cck$genotype, cck$season) #2x2 Tabell över faktorer 
table(cck$genotype:cck$season) #Tabell över faktorer

#### Tvåvägs-ANOVA #### 
modell1<-lm(log(level)~genotype*season, data=cck)
modell1<-aov(log(level)~genotype*season, data=cck)

par(mfrow=c(2,2)); plot(modell1) #residualgraf av tvåvägs-ANOVAn

anova(modell1)

#### Analys av ANOVAN #### 
#Om den genetiskt modifierade laxens aptit regleras 
#annorlunda än vildtypen med avseende på just CCK.

#Det är ingen signifikant interaktion, men säsongen spelar roll vilket inte genotypen gör 
#Bara p-värdet för faktor säsong är <0,05 så detta är enda som spelar roll

interaction.plot(cck$genotype, cck$season, cck$level)  
interaction.plot(cck$season, cck$genotype, cck$level)

cck$group <- with(cck, interaction(genotype,  season))
boxplot(cck$level~cck$group)
plotCI(cck$level~cck$group)
cck$loglevel<-log(cck$level)

plotCI(x = 1:4,
       y=medel,stdev, 
       xlab="Markanv�ndningstyp", 
       ylab="log (individer/km)",
       bty="n",                           # s�tter boxtype till "none", det vill s�ga ingen ruta runt grafytan
       xaxt="n"                           # s�tter x-axis type till "none", det vill s�ga ingen synlig x-axel
)
namn<-c("FR Dag","FR Natt","CGA Dag","CGA Natt","WMA Dag","WMA Natt","NP Dag","NP Natt")
axis(1, at=1:8, labels=namn)s
mtext("Sommar", side=1, line=2.8, at=1.5, cex=1.5) 

require(plotrix)
medel<-aggregate(cck$loglevel,
                 list(cck$season, cck$genotype), mean) 
sem<-aggregate(cck$loglevel, list(cck$season,
                                  cck$genotype), std.error)
plotCI(1:4, medel[,3], sem[,3],
       bty="n", xaxt="n", xlab="") 
axis(1,1:4,labels=medel[,2]) 
mtext(medel[,1],1,line=2,at=1:4) 







