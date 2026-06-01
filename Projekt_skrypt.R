# Etap 1: wczytanie danych, kontrola struktury i braki danych 
pakiety<-c("dplyr", "ggplot2", "Hmisc", "car", "FSA", "ggpubr", "corrplot")  

for (pakiet in pakiety){
  if(!require(pakiet, character.only = TRUE)){
    install.packages(pakiet)
    library(pakiet, character.only = TRUE)
  }
}

plik_danych<-"przykladoweDane-Projekt.csv"

folder_wyniki<-"wyniki" 
folder_tabele<-file.path(folder_wyniki, "tabele") 
folder_wykresy <- file.path(folder_wyniki, "wykresy")

dir.create(folder_wyniki, showWarnings = FALSE)
dir.create(folder_tabele, showWarnings = FALSE)
dir.create(folder_wykresy, showWarnings = FALSE) 

#wczytanie danych 
dane<-read.csv(plik_danych, sep=";") 
cat("Dane zostaly wczytane poprawnie.\n")
cat("Liczba wierszy:", nrow(dane), "\n")
cat("Liczba kolumn:", ncol(dane), "\n\n")

#podglad danych: 
cat("Nazwy kolumn:\n")
print(names(dane))

cat("\nPierwsze wiersze danych:\n")
print(head(dane))

cat("\nStruktura danych:\n")
str(dane)

dane$grupa <- as.factor(dane$grupa)
dane$plec <- as.factor(dane$plec)

cat("\nPoziomy zmiennej grupa:\n")
print(levels(dane$grupa))

cat("\nPoziomy zmiennej plec:\n")
print(levels(dane$plec))
#sprawdzenie brakow danych 

