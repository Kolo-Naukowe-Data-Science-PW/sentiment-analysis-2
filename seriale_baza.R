library("RSQLite")
library("stringi")

##zapamietujemy sciezke
sciezka<-"D:/Dokumenty/studia/kolo data science/projekt - analiza sentymentow dla seriali/"

##łączymy się z bazą
baza_seriali<-dbConnect(SQLite(),dbname=paste0(sciezka,"baza_seriali.sql"))

##tworzymy tabelki
dbSendQuery(baza_seriali,
   "CREATE TABLE Serial_Titles (Title_ID int NOT NULL PRIMARY KEY, Title nvarchar(50) NOT NULL )")
dbSendQuery(baza_seriali,"CREATE TABLE Episode_Titles (Title_ID int NOT NULL, Season_NR int NOT NULL,
   Episode_NR int NOT NULL, Episode_Title nvarchar(100) NOT NULL, PRIMARY KEY (Title_ID, Season_NR, Episode_NR) )")
dbSendQuery(baza_seriali,"CREATE TABLE Sources (Source_ID int NOT NULL PRIMARY KEY, Source nvarchar(50) NOT NULL )")
dbSendQuery(baza_seriali,"CREATE TABLE Serials (ID_Number int NOT NULL PRIMARY KEY, Title_ID int NOT NULL,
   Season_NR int NOT NULL, Episode_NR int NOT NULL, Source_ID int NOT NULL, Login nvarchar(30), Mark decimal(3,1), Comment ntext,
   FOREIGN KEY (Title_ID, Season_NR, Episode_NR) REFERENCES Episode_Titles(Title_ID, Season_NR, Episode_NR),
   FOREIGN KEY (Title_ID) REFERENCES Serial_Titles(Title_ID),
   FOREIGN KEY (Source_ID) REFERENCES Sources(Source_ID))")

##uzupelniamy tabelke z tytulami seriali
dbSendQuery(baza_seriali, "INSERT INTO Serial_Titles VALUES (1,'Arrow')")
dbSendQuery(baza_seriali, "INSERT INTO Serial_Titles VALUES (2,'Game of Thrones')")
dbSendQuery(baza_seriali, "INSERT INTO Serial_Titles VALUES (3,'The Walking Dead')")

##uzupelniamy tabelke ze zrodlami
dbSendQuery(baza_seriali, "INSERT INTO Sources VALUES (1,'IMDb.com')")
dbSendQuery(baza_seriali, "INSERT INTO Sources VALUES (2,'tv.com')")

##uzupelniamy tabelke ze tytulami odcinkow

#wyciagamy ramki danych z plików
sciezka_ramki<-paste0(sciezka,"ramki danych dla seriali")
sciezki_ramek_slownikow<-list.files(sciezka_ramki,pattern="Slownik(.)*?\\.txt$",full.names=TRUE)
n<-length(sciezki_ramek_slownikow)
ramki_slownikow<-vector("list",n)

#wrzucamy ramki danych do tabelki w bazie
for(i in 1:n)
{
   ramki_slownikow[[i]]<-read.table(sciezki_ramek_slownikow[i])
   dbWriteTable(baza_seriali,"pomocnicza",ramki_slownikow[[i]])
   dbSendQuery(baza_seriali,"INSERT INTO Episode_Titles (Title_ID, Season_NR, Episode_NR, Episode_Title)
      SELECT NrTytulu, NrSezonu, NrOdcinka, TytulOdcinka FROM pomocnicza")
   dbRemoveTable(baza_seriali, "pomocnicza")

#  wiersz po wierszu bez pomocniczej:
#  m<-nrow(ramki_slownikow[[i]])
#  for(j in 1:m)
#  {
#     dbSendQuery(baza_seriali,paste0("INSERT INTO Episode_Titles VALUES (",ramki_slownikow[[i]][j,1],",",
#        ramki_slownikow[[i]][j,2],",",ramki_slownikow[[i]][j,3],",\"",ramki_slownikow[[i]][j,4],"\")"))
#  }
}

##uzupelniamy tabelke glowna z serialami

#IMDb.com

sciezka_imdb<-list.files(sciezka_ramki,pattern="IMDb(.)*?\\.txt$",full.names=TRUE)
imdb<-read.table(sciezka_imdb)
ktore_id<-nrow(imdb)+1 #zapamietujemy bo sie przyda
dbWriteTable(baza_seriali,"pomocnicza",imdb)
dbSendQuery(baza_seriali,"ALTER TABLE pomocnicza ADD source int")
dbSendQuery(baza_seriali,paste0("UPDATE pomocnicza SET source=1"))
dbSendQuery(baza_seriali,"INSERT INTO Serials (ID_Number, Title_ID, Season_NR, Episode_NR, Source_ID,
   Login, Mark, Comment) SELECT id, Title_ID, Season, Episode, source, login, ocena, Comment FROM pomocnicza")
dbRemoveTable(baza_seriali,"pomocnicza")

#tv.com

#wyciagamy ramki danych z plików
sciezki_tv<-list.files(sciezka_ramki,pattern="Informacje(.)*?\\.txt$",full.names=TRUE)
n<-length(sciezki_tv)
ramki_informacji<-vector("list",n)

#wrzucamy ramki danych do tabelki w bazie
for(i in 1:n)
{
   ramki_informacji[[i]]<-read.table(sciezki_tv[i])
   #pozbywam sie cudzyslowiow z komentarzy
   ramki_informacji[[i]][,6]<-stri_replace_all_regex(ramki_informacji[[i]][,6],"\"","")
#  wiersz po wierszu bez pomocniczej:
   m<-nrow(ramki_informacji[[i]])
   for(j in 1:m)
   {
      dbSendQuery(baza_seriali,paste0("INSERT INTO Serials (ID_Number, Title_ID, Season_NR, Episode_NR, Source_ID,
         Login, Mark, Comment) VALUES (",ktore_id+j-1,",",ramki_informacji[[i]][j,1],",",
         ramki_informacji[[i]][j,2],",",ramki_informacji[[i]][j,3],",2,\"",ramki_informacji[[i]][j,4],"\",",
         ramki_informacji[[i]][j,5],",\"",ramki_informacji[[i]][j,6],"\")"))
   }
   ktore_id<-ktore_id+m
}

#zapamietujemy wektor nazw tabel
(nazwy_tabeli<-dbListTables(baza_seriali))
#wyswietlamy co tam maja w sobie
# for(i in 1:length(nazwy_tabeli))
# {
#    wyswietl<-dbReadTable(baza_seriali,nazwy_tabeli[3])
#    print(wyswietl)
# }


dbDisconnect(baza_seriali)
