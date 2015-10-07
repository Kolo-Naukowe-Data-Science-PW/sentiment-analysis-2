load("F:/Norbert/studia/KoloNaukoweDS/Projekty/Analiza sentymentu/dane.rda", )
dane <- dane[-1,]
colnames(dane) <- c("nick","date","comment","i")
dane[,3] <- stri_trans_tolower(dane[,3])
dane <- dane[sample(1:nrow(dane),10000,replace = FALSE),]
dane <- dane[order(dane$i),]
affin <- read.csv2("F:/Norbert/studia/KoloNaukoweDS/Projekty/Analiza sentymentu/sentiment-analysis-2/AFINN - slownik/AFINN-111.txt",
                   sep="\t")

postacie <- c("cersei",
             "tyrion",
             "daenerys",
             "arya",
             "jon", # tu trzeba regexa żeby wylapac tylko imie a nie czesc innych slow
             "sansa",
             "jaime",
             "tywin",
             "joffrey",
             "robb",
             "ned",     # tu trzeba regexa
             "stannis",
             "margaery")

rody <- c("stark","lannister","targaryen")

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

print(Sys.time())
sentyment_komentarzy <- sentyment(dane[,3],affin)
dane$sentyment <- sentyment_komentarzy
print(Sys.time())
dane$daenerys <- stri_detect_fixed(dane[,3],"daenerys")
dane$tyrion <- stri_detect_fixed(dane[,3],"tyrion")
dane$arya <- stri_detect_fixed(dane[,3],"arya")
dane$cersei <- stri_detect_fixed(dane[,3],"cersei")
dane$joffrey <- stri_detect_fixed(dane[,3],"joffrey")


 library(ggplot2)

ggplot(dane[dane[,"daenerys"],][-1264,],aes(x=as.Date(date),y=sentyment)) + 
  geom_point() +
  geom_smooth() +
  scale_x_date()

ggplot(dane,aes(x=as.Date(date),y=sentyment)) + 
  ylim(c(-200,300)) +
  geom_point() +
  geom_smooth() +
  scale_x_date() +
  geom_smooth(aes(x=as.Date(date[dane[,"tyrion"]]),y=sentyment[dane[,"tyrion"]]))

ggplot(dane[dane[,"tyrion"],][-1264,],aes(x=as.Date(date),y=sentyment)) + 
  geom_point() +
  geom_smooth() +
  scale_x_date()

ggplot(dane[dane[,"arya"],][-1264,],aes(x=as.Date(date),y=sentyment)) + 
  geom_point() +
  geom_smooth() +
  scale_x_date()

ggplot(dane[dane[,"cersei"],][-1264,],aes(x=as.Date(date),y=sentyment)) + 
  geom_point() +
  geom_smooth() +
  scale_x_date()

ggplot(dane[dane[,"joffrey"],][-1264,],aes(x=as.Date(date),y=sentyment)) + 
  geom_point() +
  geom_smooth() +
  scale_x_date()


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
