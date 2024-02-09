# Konfidensintervall med känd population ####

# Illustration av hur p < 0.05 betyder att 5% av alla konfidensintervall inte 
# kommer omfatta det sanna medelvärdet

# Vi har en komplett population om 65 individer som vi mätt (värden finns i 
# vektorn length). Notera att detta anses som alla individer i populationen, 
# eller alla tänkbara mätningar som kan göras på populationen, men i 
# verkligheten har vi ingen möjlighet att mäta alla individer och 
# måste därför nöja oss med stickprov och uttala oss om populationens värden,
# exempelvis medelvärde och variation, baserat på stickprovet. 
# Därför tar vi 10 (sample_size) slumpmässigt valda mätningar från vår 
# "sanna population". Från detta stickprov om 10 mätningar beräknas medelvärde
# och konfidensintervall. Vi gör denna procedur 20 gånger mha en for-loop. 
 
# Plottar upp medelvärden och 95% konfidensintervall för varje stickprov
# Lägger till populations faktiska medelvärde.
# Noterar hur många av stickprovens konfidensintervall som inte innefattar
# det sanna medelvärdet.

require(plotrix)           # ladda paketet plotrix för att kunna göra figuren

# Körningarna kommer utnyttja "slump" och om man vill kunna återskapa samma
# slump igen kan man använda 
#set.seed(100)  # set.seed(x) gör att 'slumptalen' alltid blir desamma för x
# för nya slumptal vid varje körning, skippa denna rad

# data på längd hos 65 personer, dvs hela populationen
length<-c(180,185,176,174,175,155,192,150,170,172,176,161,181,181,214,160,184,
          195,173,169,159,166,176,179,170,178,190,156,181,172,176,179,180.5,
          168,165,173.5,163,177.5,185,175,195,170,170,161,175,175,146,173,166,
          177,173,172,183,190,178,166,178,178,164,174,188,181,188,173,170)

# Det sanna medelvärdet hos populationen 
mean_pop <- mean(length)

# Förberedelser
sample_size<-10    # antal replikat per stickprov
no_samples <- 20    # antal oberoende stickprov 

samples_mean <-numeric()    # skapar tomma vektorer
samples_sd <- numeric()
CIlow <- numeric()
CIhigh <- numeric()
color<-c()

for(i in 1:no_samples) {                      # starta loopen och kör no_samples gånger
    samples <- sample(length, sample_size)    # sample slumpar ut sample_size antal från vektorn length
    samples_mean[i] <- mean(samples)          # beräknar medelvärde på i:te stickprovet

    CIlow[i] <- t.test(samples)$conf.int[1]   # extraherar nedre KI gränsen
    CIhigh[i] <- t.test(samples)$conf.int[2]  # extraherar övre KI gränsen
    if (CIlow[i] > mean_pop | CIhigh[i] < mean_pop){   # sätt färg till röd om    
        color[i]<-"red"                       # 95% intervall inte omfattar
    } else {                                  # populationens sanna medelvärde, 
        color[i]<-"black"                     # annars svart 
    }                                         # strecket | står för "eller"
}

plotCI(x=1:no_samples,                        # skapar x-värden från 1 till no_samples
       y=samples_mean,                        # ritar in medelvärden
       li=CIlow,                              # ritar in nedre KI
       ui=CIhigh,                             # ritar in övre KI
       pch=19,                                # sätter medelvärdets symbol som en fylld cirkel
       col=color,
       ylab="Medelvärde med 95% K.I.",        # y-axelns förklaring 
       xlab="Stickprov")                      # x-axelns förklaring

# rita in grön horisontell linje som är värdet på mean_pop, dvs populationens sanna medelvärde
abline(h=mean_pop, col="green")


# Konfidensintervall med slumptal ####

# Här skapar vi inför varje körning en population på 1000 individer
# sedan tas prover om 20 från denna population och för varje prov beräknas
# 95 % konfidensintervall och plottas mot populationens sanna medelvärde
# Konfidensintervall som inte omfattar medelvärdet markeras med rött


library(plotrix)          # ladda paketet plotrix för att göra errorplot

# Initiera variabler
tot_pop<-c()
ciLow<-numeric()
ciHigh<-numeric()
mean_sample<-numeric()
color<-c()

tot_pop<-rnorm(1000,50,4)     # skapar en population på 1000 tal med medelvärde 50 och sd 4
mean_pop<-mean(tot_pop)       # beräkna populationen sanna medelvärde
no_samples<-20

for (i in 1:no_samples){                   # loop som tar 100 prov från populationen
    sample<-sample(tot_pop, 20, replace=FALSE) # ta slumpmässigt 20 unika tal från populationen
    mean_sample[i]<-t.test(sample)$estimate  # extrahera medelvärdet från provet
    ciLow[i]<-t.test(sample)$conf.int[1]     # extrahera undre 95% intervallsgränsen 
    ciHigh[i]<-t.test(sample)$conf.int[2]    # extrahera övre 95% intervallsgränsen
    
    if (ciLow[i] > mean_pop | ciHigh[i] < mean_pop){   # sätt färg till röd om 95% intervall inte omfattar   
        color[i]<-"red"                                # populationens sanna medelvärde, strecket | står för "eller"
    } else {
        color[i]<-"black"
    }}


#plotta figuren med konfidensintervallen f?r de 100 proverna
plotCI(1:no_samples, y = mean_sample,  
       ui = ciHigh, li = ciLow, 
       pch = 16, col = color,
       xlab = "Provnummer",
       ylab = "Medelvärden",
       ylim = c(44,56))                               # fix y-axel för att se samma span
abline(h=mean_pop, col="green")                       # lägg in populationens sanna medelvärde

