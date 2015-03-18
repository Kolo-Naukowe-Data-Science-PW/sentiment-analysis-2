############################################################################
#####################            tv.com            #########################
############################################################################
#Skrypt pobiera komentarze z serwisu tv.com dotyczace trzech wybranych
#seriali:
#Arrow
#The Walking Dead
#Game of Thrones
#
#Po jego wykonaniu otrzymujemy ramki danych:
#InformacjeA     oraz   SlownikOdcinkowA
#InformacjeTWD   oraz   SlownikOdcinkowTWD
#InformacjeGOT   oraz   SlownikGOT
#
#
#o strukturach
#InformacjeA:
#NrTytulu,NrSezonu,NrOdcinka,Login,Ocena,Komentarz
#
#SlownikOdcinkowA:
#NrTytulu,NrSezonu,NrOdcinka,TytulOdcinka
#
#
#NrTytulu są kodowane w nastepujacy sposób:
#   1 - Arrow
#   2 - Game of Thrones
#   3 - The Walking Dead

############################################################################
#####################       tv.com  -  ARROW       #########################
############################################################################
#Skrypt ściąga komentarze dotyczące poszczegolnych odcinkow serialu Arrow
#z serwisu tv.com

library(rvest)
library(stringi)
library(XML)
#Korzystam z wiedzy, że są trzy sezony
IloscSezonow<-3

#Tworzę puste ramki danych z zaśmieconym pierwszym wierszem
#Pomaga mi to utrzymać strukturę ramki danych - potem go usunę
InformacjeA<-data.frame(NrTytulu=1,NrSezonu=0,NrOdcinka=0,
   Login="aaa",Ocena=0,Komentarz="")
SlownikOdcinkowA<-data.frame(NrTytulu=1,NrSezonu=0,NrOdcinka=0,
   TytulOdcinka="")

for(i in 1:IloscSezonow){
   StronaSezonu<-html(stri_paste("http://www.tv.com/shows/arrow/season-",
      i,"/",collapse="",sep=""))
   SezonWezly<-html_nodes(StronaSezonu,".ep_info , .title")
   Odcinki<-html_text(SezonWezly)
   OdcinkiLinki<-html_attr(SezonWezly,"href")
   OdcinkiLinki<-OdcinkiLinki[which(!is.na(OdcinkiLinki))]
   OdcinkiTytuly<-Odcinki[seq(1,length(Odcinki),by=2)]
   NrOdcinkow<-as.numeric(unlist(stri_match_all_regex(
      Odcinki[seq(2,length(Odcinki),by=2)],"(?<=Episode ).+?(?=\r)")))

   slownik<-data.frame(NrTytulu=rep(1,times=length(OdcinkiTytuly)),
      NrSezonu=rep(i,times=length(OdcinkiTytuly)),NrOdcinka=NrOdcinkow,
      TytulOdcinka=OdcinkiTytuly)
   SlownikOdcinkowA<-rbind(SlownikOdcinkowA,slownik)

   for(j in seq_along(OdcinkiLinki)){
      KomentarzeStrona<-html(stri_paste("http://www.tv.com",
         OdcinkiLinki[j],"/reviews/",collapse="",sep=""))
      KomentarzeWezly<-html_nodes(KomentarzeStrona,
         "._rating , .author , .text")
      KomentarzeTekst<-html_text(KomentarzeWezly)
      n<-length(KomentarzeTekst)
      if(length(KomentarzeTekst)==1){next}

      #Ostatni element to informacja od forum - niepotrzebna
      KomentarzeTekst<-KomentarzeTekst[-n]
      n<-n-1

      Oceny<-as.numeric(KomentarzeTekst[seq(1,n,by=3)])
      Loginy<-KomentarzeTekst[seq(2,n,by=3)]
      Komentarz<-KomentarzeTekst[seq(3,n,by=3)]
      n<-length(Oceny)

      #Zapis danych
      Dane<-data.frame(NrTytulu=rep(1,times=n),
         NrSezonu=rep(i,times=n),
         NrOdcinka=rep(NrOdcinkow[j],times=n),Login=Loginy,Ocena=Oceny,
         Komentarz=Komentarz)
      InformacjeA<-rbind(InformacjeA,Dane)
   }
}

#Czyszczę pierwsze linijki
InformacjeA<-InformacjeA[-1,]
SlownikOdcinkowA<-SlownikOdcinkowA[-1,]
k<-which(is.na(SlownikOdcinkowA[,"NrOdcinka"]))
if(length(k)>0){SlownikOdcinkowA<-SlownikOdcinkowA[-k,]}
SlownikOdcinkowA<-unique(SlownikOdcinkowA)

############################################################################
###################   tv.com  -  THE WALKING DEAD    #######################
############################################################################
#Skrypt ściąga komentarze dotyczące poszczegolnych odcinkow serialu
#The Walking Dead z serwisu tv.com


#Korzystam z wiedzy, że jest pięć sezonów
IloscSezonow<-5
#Tworzę puste ramki danych z zaśmieconym pierwszym wierszem
#Pomaga mi to utrzymać strukturę ramki danych - potem go usunę
InformacjeTWD<-data.frame(NrTytulu=3,NrSezonu=0,NrOdcinka=0,
   Login="aaa",Ocena=0,Komentarz="")
SlownikOdcinkowTWD<-data.frame(NrTytulu=3,NrSezonu=0,NrOdcinka=0,
   TytulOdcinka="")

for(i in 1:IloscSezonow){
   StronaSezonu<-html(stri_paste(
      "http://www.tv.com/shows/the-walking-dead/season-",
      i,"/",collapse="",sep=""))
   SezonWezly<-html_nodes(StronaSezonu,".ep_info , .title")
   Odcinki<-html_text(SezonWezly)
   OdcinkiLinki<-html_attr(SezonWezly,"href")
   OdcinkiLinki<-OdcinkiLinki[which(!is.na(OdcinkiLinki))]
   OdcinkiTytuly<-Odcinki[seq(1,length(Odcinki),by=2)]
   NrOdcinkow<-as.numeric(unlist(stri_match_all_regex(
      Odcinki[seq(2,length(Odcinki),by=2)],"(?<=Episode ).+?(?=\r)")))

   slownik<-data.frame(NrTytulu=rep(3,times=length(OdcinkiTytuly)),
      NrSezonu=rep(i,times=length(OdcinkiTytuly)),NrOdcinka=NrOdcinkow,
      TytulOdcinka=OdcinkiTytuly)
   SlownikOdcinkowTWD<-rbind(SlownikOdcinkowTWD,slownik)

   for(j in seq_along(OdcinkiLinki)){
      KomentarzeStrona<-html(stri_paste("http://www.tv.com",
         OdcinkiLinki[j],"/reviews/",collapse="",sep=""))
      KomentarzeWezly<-html_nodes(KomentarzeStrona,
         "._rating , .author , .text")
      KomentarzeTekst<-html_text(KomentarzeWezly)
      n<-length(KomentarzeTekst)
      if(length(KomentarzeTekst)==1){next}

      #Ostatni element to informacja od forum - niepotrzebna
      KomentarzeTekst<-KomentarzeTekst[-n]
      n<-n-1

      Oceny<-as.numeric(KomentarzeTekst[seq(1,n,by=3)])
      Loginy<-KomentarzeTekst[seq(2,n,by=3)]
      Komentarz<-KomentarzeTekst[seq(3,n,by=3)]
      n<-length(Oceny)

      #Zapis danych
      Dane<-data.frame(NrTytulu=rep(3,times=n),
         NrSezonu=rep(i,times=n),
         NrOdcinka=rep(NrOdcinkow[j],times=n),Login=Loginy,Ocena=Oceny,
         Komentarz=Komentarz)
      InformacjeTWD<-rbind(InformacjeTWD,Dane)
   }
}

#Czyszczę pierwsze linijki
InformacjeTWD<-InformacjeTWD[-1,]
SlownikOdcinkowTWD<-SlownikOdcinkowTWD[-1,]
k<-which(is.na(SlownikOdcinkowTWD[,"NrOdcinka"]))
if(length(k)>0){SlownikOdcinkowTWD<-SlownikOdcinkowTWD[-k,]}
SlownikOdcinkowTWD<-unique(SlownikOdcinkowTWD)



############################################################################
###################   tv.com  -  GAME OF THRONES     #######################
############################################################################
#Skrypt ściąga komentarze dotyczące poszczegolnych odcinkow serialu
#Game of Thrones z serwisu tv.com


#Korzystam z wiedzy, że są cztery sezony
IloscSezonow<-4
#Tworzę puste ramki danych z zaśmieconym pierwszym wierszem
#Pomaga mi to utrzymać strukturę ramki danych - potem go usunę
InformacjeGOT<-data.frame(NrTytulu=2,NrSezonu=0,NrOdcinka=0,
   Login="aaa",Ocena=0,Komentarz="")
SlownikOdcinkowGOT<-data.frame(NrTytulu=2,NrSezonu=0,NrOdcinka=0,
   TytulOdcinka="")

for(i in 1:IloscSezonow){
   StronaSezonu<-html(stri_paste(
      "http://www.tv.com/shows/game-of-thrones/season-",
      i,"/",collapse="",sep=""))
   SezonWezly<-html_nodes(StronaSezonu,".ep_info , .title")
   Odcinki<-html_text(SezonWezly)
   OdcinkiLinki<-html_attr(SezonWezly,"href")
   OdcinkiLinki<-OdcinkiLinki[which(!is.na(OdcinkiLinki))]
   OdcinkiTytuly<-Odcinki[seq(1,length(Odcinki),by=2)]
   NrOdcinkow<-as.numeric(unlist(stri_match_all_regex(
      Odcinki[seq(2,length(Odcinki),by=2)],"(?<=Episode ).+?(?=\r)")))

   slownik<-data.frame(NrTytulu=rep(2,times=length(OdcinkiTytuly)),
      NrSezonu=rep(i,times=length(OdcinkiTytuly)),NrOdcinka=NrOdcinkow,
      TytulOdcinka=OdcinkiTytuly)
   SlownikOdcinkowGOT<-rbind(SlownikOdcinkowGOT,slownik)

   for(j in seq_along(OdcinkiLinki)){
      KomentarzeStrona<-html(stri_paste("http://www.tv.com",
         OdcinkiLinki[j],"/reviews/",collapse="",sep=""))
      KomentarzeWezly<-html_nodes(KomentarzeStrona,
         "._rating , .author , .text")
      KomentarzeTekst<-html_text(KomentarzeWezly)
      n<-length(KomentarzeTekst)
      if(length(KomentarzeTekst)==1){next}

      #Ostatni element to informacja od forum - niepotrzebna
      KomentarzeTekst<-KomentarzeTekst[-n]
      n<-n-1

      Oceny<-as.numeric(KomentarzeTekst[seq(1,n,by=3)])
      Loginy<-KomentarzeTekst[seq(2,n,by=3)]
      Komentarz<-KomentarzeTekst[seq(3,n,by=3)]
      n<-length(Oceny)

      #Zapis danych
      Dane<-data.frame(NrTytulu=rep(2,times=n),
         NrSezonu=rep(i,times=n),
         NrOdcinka=rep(NrOdcinkow[j],times=n),Login=Loginy,Ocena=Oceny,
         Komentarz=Komentarz)
      InformacjeGOT<-rbind(InformacjeGOT,Dane)
   }
}

#Czyszczę pierwsze linijki
InformacjeGOT<-InformacjeGOT[-1,]
SlownikOdcinkowGOT<-SlownikOdcinkowGOT[-1,]
k<-which(is.na(SlownikOdcinkowGOT[,"NrOdcinka"]))
if(length(k)>0){SlownikOdcinkowGOT<-SlownikOdcinkowGOT[-k,]}
SlownikOdcinkowGOT<-unique(SlownikOdcinkowGOT)

