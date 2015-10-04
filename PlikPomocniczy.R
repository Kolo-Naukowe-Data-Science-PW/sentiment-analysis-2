load("F:/Norbert/studia/KoloNaukoweDS/Projekty/Analiza sentymentu/dane.rda", )
dane <- dane[-1,]
colnames(dane) <- c("nick","date","comment","i")
dane <- dane[sample(1:nrow(dane),10000,replace = FALSE),]
dane <- dane[order(dane$i),]
affin <- read.csv2("F:/Norbert/studia/KoloNaukoweDS/Projekty/Analiza sentymentu/sentiment-analysis-2/AFINN - slownik/AFINN-111.txt",
                   sep="\t")

postacie <- c("Cersei",
             "Tyrion",
             "Daenerys",
             "Arya",
             "Jon",
             "Sansa",
             "Jaime",
             "Tywin",
             "Joffrey",
             "Robb",
             "Ned",
             "Stannis",
             "Margaery")

rody <- c("stark","Lannister","Targaryen")

library(stringi)
library("RSQLite")
con <- dbConnect(SQLite(),"F:/Norbert/studia/KoloNaukoweDS/Projekty/Analiza sentymentu/sentiment-analysis-2/baza_seriali.sql")
dbListTables(con)
head(dbReadTable(con,"Serials"))
komentarze_wszystkie <- dbGetQuery(con,"select Comment from Serials")[[1]]


##############################

library(stringi)
s <- data.frame(c("a","b","c"),c(1,2,3))
t <- c("A b b c c c", "a b b")

#funkcja sentyment zwraca wektor sentymentow dla wektora tekstow
sentyment <- function(teksty,slownik){
  teksty <- stri_trans_tolower(teksty)
  sapply(teksty,function(x) sum(stri_count_fixed(x,slownik[[1]])*slownik[[2]]),USE.NAMES = FALSE)
}
#sentyment(dane[1:100,3],s)


#sentyment_komentarzy <- sentyment(dane[,3],affin)
# 
# 
# library(ggplot2)
# 
# ggplot()
# plot(
#   sentyment_komentarzy*stri_detect_fixed(dane[,3],"tyrion")
# )
# 
# par(new=TRUE)
# 
# plot(
#   sentyment_komentarzy*stri_detect_fixed(dane[,3],"daenerys"),
#   col="red",type="l"
# )
# d <- data.frame(x=1:10000,y=sentyment_komentarzy*stri_detect_fixed(dane[,3],"daenerys"))
# ggplot(d,aes(x=x,y=y))+geom_smooth(method = "loess")
# 
# dane[max(which(stri_detect_fixed(dane[,3]," ned"))),c(2,3)]
