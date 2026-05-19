install.packages("Hmisc")
library(Hmisc)

install.packages("dplyr")
library(dplyr)

#install.packages("https://cran.r-project.org/src/contrib/Archive/ggpubr/ggpubr_0.2.4.tar.gz", repo=NULL, type="source")
install.packages("ggpubr")
library(ggpubr)

install.packages("car")
library(car)

install.packages("dunn.test")
library(dunn.test)

install.packages("FSA")
library(FSA)

##################################################

dane <- read.csv2("przykladoweDaneZBrakami.csv", sep = ";")
dane

######################### is.na() i na.omit() #########################

wektorZBrakamiDanych <- dane$HGB
wektorZBrakamiDanych

is.na(wektorZBrakamiDanych)
sum(is.na(wektorZBrakamiDanych))
which(is.na(wektorZBrakamiDanych))

wektorBezBrakówDanych <- na.omit(wektorZBrakamiDanych) 
wektorBezBrakówDanych

length(wektorZBrakamiDanych)
length(wektorBezBrakówDanych)

######################### complete.cases() #########################

dane
complete.cases(dane) #zwraca wektor T i F dla wierszy
which(complete.cases(dane) == FALSE)
noweDane <- dane[complete.cases(dane), ]
noweDane

######################### impute() #########################

wektorZBrakamiDanych <- dane$HGB
wektorZBrakamiDanych
impute(wektorZBrakamiDanych, 10)		# imputuj wartość 10 w miejsca NA
impute(wektorZBrakamiDanych, mean)		# imputuj średnią
mean(wektorZBrakamiDanych, na.rm = TRUE)

wektorZBrakamiDanych <- impute(wektorZBrakamiDanych, mean)
wektorZBrakamiDanych

######################### statystyki opisowe #########################

dane <- read.csv2("przykladoweDaneBezBrakow.csv", sep = ";")
dane$wiek
attach(dane) 
wiek

range(wiek)
mean(wiek)
median(wiek)
IQR(wiek)
var(wiek)
sd(wiek)

boxplot(wiek, plot = FALSE)
boxplot(wiek)

######################### summary() i table() #########################
summary(wiek)


#kwartyle - test
length(wiek)
length(wiek[which(wiek < 26.5)])
cat(round((length(wiek[which(wiek < 26.5)]) * 100)/length(wiek)),"%")
#dla parametru wiek, na podstawie Q1 = 26.50 wiemy, że
#75% zbadanych osób ma więcej niż 26.5 lat, 
#25% zbadanych osób ma mniej niż 26.5 lat,
#podczas gdy na podstawie Q3 = 34.5  wiemy, że
#25% zbadanych osób ma więcej niż 34.5 lat  
#a 75% osób ma mniej niż 34.5 lat

summary(plec)

table(plec)
table(grupa, plec)

######################### summarise() #########################
dane$grupa
#zmiana kolejności w poziomie danych
dane$grupa <- ordered(dane$grupa, levels = c("KONTROLA", "CHOR1", "CHOR2"))
dane$grupa
levels(dane$grupa)

#pakiet dplyr, funkcja group_by(), summerise() 
podumowanie_wiek <- group_by(dane, grupa) %>%
  summarise(
    count = n(),
    mean = format(round(mean(wiek, na.rm = TRUE), 2), nsmall = 2),
    sd = format(round(sd(wiek, na.rm = TRUE), 2), nsmall = 2),
    median = format(round(median(wiek, na.rm = TRUE), 2), nsmall = 2)
  )
print(podumowanie_wiek)


######################### rozkład normalny #########################
######################### shapiro.test() bez grupowania #########################

KONTROLA <- with(dane, dane[grupa == "KONTROLA", ])
KONTROLA
CHOR1 <- with(dane, dane[grupa == "CHOR1", ])
CHOR2 <- with(dane, dane[grupa == "CHOR2", ])

shapiro.test(CHOR1$hsCRP)
shapiro.test(CHOR2$hsCRP)
shapiro.test(KONTROLA$hsCRP)


######################### shapiro.test() z grupowaniem #########################

pvalueShapiroTestHSCRP <- group_by(dane, grupa) %>%
  summarise(
    statistic = shapiro.test(hsCRP)$statistic,
    p.value = shapiro.test(hsCRP)$p.value
  )
pvalueShapiroTestHSCRP

pvalueShapiroTestHSCRP$p.value
pvalueShapiroTestHSCRP$p.value[(pvalueShapiroTestHSCRP$grupa == "CHOR1")]

for(i in 1:length(pvalueShapiroTestHSCRP$p.value)){
  if(pvalueShapiroTestHSCRP$p.value[i] < 0.05){
    cat("\n", pvalueShapiroTestHSCRP$p.value[i], "< 0.05 - nie można założyć zgodności z rozkładem normalnym")
  }else{
    cat("\n", pvalueShapiroTestHSCRP$p.value[i], "> 0.05 - można założyć zgodność z rozkładem normalnym")
  }
}

######################### wykres gęstości #########################

ggdensity(dane, x = "hsCRP",
          color = "grupa", fill = "grupa",
          palette = c("#99cc00", "#660099", "#0047b3"),
          ylab = "gęstość",
          xlab = "hsCRP [mg/l]"
)


ggdensity(dane, x = "hsCRP",
          color = "grupa", fill = "grupa",
          palette = c("#99cc00", "#660099", "#0047b3"),
          ylab = "gęstośc",
          xlab = "hsCRP [mg/l]"
) + facet_wrap(~ grupa, scales = "free")


#alternatywa gdy ggpubr nie działa
par(mfrow=c(1, 3))
plot(density(CHOR1$hsCRP))
plot(density(CHOR2$hsCRP))
plot(density(KONTROLA$hsCRP))
dev.off()

######################### jednorodność wariancji #########################
######################### levene.test() pakiet car #########################

leveneTest(hsCRP ~ grupa, data = dane)

leveneTestResult <- leveneTest(hsCRP ~ grupa, data = dane)
leveneTestResult
leveneTestResult$"Pr(>F)"[1]


######################### porównywanie grup #########################
######################### kruskal.test() #########################

kruskal.test(HGB ~ grupa, data = dane)
pvalueKWtestHGB <- kruskal.test(HGB ~ grupa, data = dane)$p.value
pvalueKWtestHGB

if(pvalueKWtestHGB < 0.05){
  cat(pvalueKWtestHGB, "< 0.05 - są różnice pomiędzy grupami")
}else{
  cat(pvalueKWtestHGB, "> 0.05 - brak różnic pomiędzy grupami")
}

######################### post hoc dunnTest #########################

dunnTest(dane$HGB, dane$grupa)

######################### aov() #########################

aov(MCHC ~ grupa, data = dane)
summary(aov(MCHC ~ grupa, data = dane))
pvalueAOVtestMCHC <- summary(aov(MCHC ~ grupa, data = dane))[[1]][["Pr(>F)"]][[1]]
pvalueAOVtestMCHC

if(pvalueAOVtestMCHC < 0.05){
  cat(pvalueAOVtestMCHC, "< 0.05 - są różnice pomiędzy grupami")
}else{
  cat(pvalueAOVtestMCHC, "> 0.05 - brak różnic pomiędzy grupami")
}


######################### post hoc TukeyHSD() #########################

TukeyHSD(aov(MCHC ~ grupa, data = dane))

######################### chisq.test() #########################

table(dane$grupa, dane$plec) 

chisq.test(dane$grupa, dane$plec)
pvalueChisqPlec <- chisq.test(dane$grupa, dane$plec)$p.value
pvalueChisqPlec

barplot(table(dane$plec, dane$grupa),
        ylim = c(0,22),
        beside = TRUE,
        col = c("#ffb3b3", "#b3d1ff"),
        xlab = "grupa",
        ylab = "płeć",
        legend = c("kobieta", "mężczyzna")
)
text(1.5, 20, paste("p-value", round(pvalueChisqPlec, digits = 3)))

######################### porównywanie 2 grup #########################

dwieGrupy <- read.csv2("przykladowe2Grupy.csv", sep = ";")
dwieGrupy

######################### wilcox.test() #########################
#dane niezgodne z rozkładem normalnym

wilcox.test(hsCRP ~ grupa, data = dwieGrupy)
pvalueWilcoxTest <- wilcox.test(hsCRP ~ grupa, data = dwieGrupy)$p.value
pvalueWilcoxTest

######################### t.test() var.equal = TRUE #########################
#dane zgodne z rozkładem normalnym i dane jednorodne

t.test(LEU ~ grupa, data = dwieGrupy, var.equal = TRUE)
pvalueTtest <- t.test(LEU ~ grupa, data = dwieGrupy, var.equal = TRUE)$p.value
pvalueTtest

######################### t.test() var.equal = FALSE #########################
#dane zgodne z rozkładem normalnym, ale dane niejednorodne

t.test(LEU ~ grupa, data = dwieGrupy, var.equal = FALSE)
pvalueTtest <- t.test(LEU ~ grupa, data = dwieGrupy, var.equal = FALSE)$p.value
pvalueTtest

######################### testy korelacji #########################

CHOR1 <- dane %>% filter(grupa == "CHOR1")
CHOR1

wynikTestuKorelacjiSpearmana <- cor.test(CHOR1$HGB, CHOR1$ERY, method = "spearman")
wynikTestuKorelacjiSpearmana
wynikTestuKorelacjiSpearmana$estimate
wynikTestuKorelacjiSpearmana$p.value 

if(wynikTestuKorelacjiSpearmana$p.value  < 0.05){
  cat(wynikTestuKorelacjiSpearmana$p.value , "< 0.05 - istnieje istotna korelacja")
}else{
  cat(wynikTestuKorelacjiSpearmana$p.value , "> 0.05 - brak korelacji")
}



KONTROLA <- dane %>% filter(grupa == "KONTROLA")
KONTROLA

wynikTestuKorelacjiPearsona <- cor.test(KONTROLA$HGB, KONTROLA$ERY, method = "pearson")
wynikTestuKorelacjiPearsona
wynikTestuKorelacjiPearsona$estimate
wynikTestuKorelacjiPearsona$p.value 

if(wynikTestuKorelacjiPearsona$p.value  < 0.05){
  cat(wynikTestuKorelacjiPearsona$p.value , "< 0.05 - istnieje istotna korelacja")
}else{
  cat(wynikTestuKorelacjiPearsona$p.value , "> 0.05 - brak korelacji")
}


######################### wykres korelacji #########################

ggscatter(KONTROLA, x = "HGB", y = "ERY", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          color = "grupa", fill = "grupa",
          palette = c("#99cc00"),
          ylab = "HGB [gl/dl]", 
          xlab = "ERY [t/l]"
)
