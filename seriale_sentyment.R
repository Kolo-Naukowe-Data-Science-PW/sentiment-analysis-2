library("RSQLite")
library("stringi")
library("tm")

##zapamietujemy sciezke
sciezka<-"D:/Dokumenty/studia/kolo data science/projekt - analiza sentymentow dla seriali/"

##łączymy się z bazą
baza_seriali<-dbConnect(SQLite(),dbname=paste0(sciezka,"baza_seriali.sql"))

(nazwy_tabeli<-dbListTables(baza_seriali))

zrodla<-seriale<-dbReadTable(baza_seriali,nazwy_tabeli[4])
seriale<-dbReadTable(baza_seriali,nazwy_tabeli[3])

analizaKomentarzy <- function(ramka){
   slownik<-read.csv2(paste0(sciezka,"/AFINN - slownik/AFINN-111.txt"),sep="\t")

   komentarze<-ramka$Comment

   komentarze<-stri_trans_tolower(komentarze)
   komentarze<-sapply(komentarze,removeWords,stopwords("english"),USE.NAMES=FALSE)
   komentarze<-sapply(komentarze,stripWhitespace,USE.NAMES=FALSE)
   komentarze<-sapply(komentarze,removePunctuation,USE.NAMES=FALSE)
   komentarze<-sapply(komentarze,stripWhitespace,USE.NAMES=FALSE)
   komentarze<-stri_extract_all_words(komentarze)

   wydzwiek<-sapply(komentarze,function(lista)
      {
      n<-length(lista) #zapamietujemy liczbe slow w wypowiedzi
      wartosci<-numeric(n) #wektor wartosci wydzwieku dla slow
      ile<-0 #ile slow wykryto
      for(i in seq_len(n))
      {
         ktory<-which(lista[i]==slownik[,1]) #zapamietujemy czy slowo jest
            #w slowniku
         if(length(ktory)>0) #gdy jest to zapamietujemy jego wydzwiek
         {
            ile<-ile+1
            wartosci[i]<-slownik[ktory,2]
         }
      }
      if(ile>0){ #zapamietujemy wartosc dla wypowiedzi
         lista<-sum(wartosci)/ile
      }
      else{
         lista<-0
      }
      })
   return(wydzwiek)
}

#IMDb.com
imdb<-seriale[which(seriale$Source_ID==1),]

#TV.com
tv<-seriale[which(seriale$Source_ID==2),]
wydzwiek_calosc<-analizaKomentarzy(tv)
podzial_na_sezony<-function(lista){
      ile_sezonow<-unique(lista)
      ostatni_rekord_sezonu<-numeric(length(ile_sezonow))
      for(i in ile_sezonow){
         ostatni_rekord_sezonu[i]<-sum(lista==i)
         if(i>1){
            ostatni_rekord_sezonu[i]<-ostatni_rekord_sezonu[i]+ostatni_rekord_sezonu[i-1]
         }
      }
      ostatni_rekord_sezonu
   }
podzial<-podzial_na_sezony(tv[tv$Title_ID==1,]$Season_NR) #dla Arrow
wydzwiek_sezon<-wektory<-vector(mode="list",length=length(podzial))
wektory[[1]]<-1:podzial[1]
wektory[[2]]<-(podzial[1]+1):podzial[2]
wektory[[3]]<-(podzial[2]+1):podzial[3]

for(i in 1:length(podzial)){
   wydzwiek_sezon[[i]]<-wydzwiek_calosc[wektory[[i]]]
}

boxplot(wydzwiek_calosc)
boxplot(wydzwiek_sezon)

dbDisconnect(baza_seriali)
