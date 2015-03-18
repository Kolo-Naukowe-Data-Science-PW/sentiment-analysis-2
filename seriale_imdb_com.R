library(rvest)
library(stringi)
# Ta funkcja pobiera dane dla konkretnego tytulu z serialu.
# Jako argument podajemy adres strony serialu na IMDb
pobierzIMDB<-function(link)
{
   adres<-html(link)
   tytul<-html_nodes(adres,".header .itemprop")
   tytul<-html_text(tytul)
   if(tytul=="Arrow")
   {
      Title_ID=1
   }
   else
   {
      if(tytul=="Zywe trupy")
      {
         Title_ID=3
      }
      else
      {
         if(tytul=="Gra o tron")
         {
            Title_ID=2
         }
         else
         {
            stop("Tego serialu nie rozpatrujemy.")
         }
      }

   }

   adres1<-html_nodes(adres,".clear+ div")
   adres1<-html_text(adres1)
   sezony<-stri_extract_all_regex(adres1,"[0-9][0-9]*")
   ostatni_sezon<-sum(stri_count_words((unlist(sezony))))
   dane<-data.frame()
   for(i in 1:ostatni_sezon)
   {
      linkisezonow<-stri_paste(link,"episodes?season=")
      linkisezonow<-stri_paste(linkisezonow,i)
      linkisezonow1<-html(linkisezonow)
      linkisezonow1<-html_nodes(linkisezonow1,"#episodes_content strong a")
      linkiodcinkow<-html_attr(linkisezonow1,"href")
      linkiodcinkow<-stri_paste("http://www.imdb.com",linkiodcinkow)
      linkireviews<-unlist(stri_extract_all_regex(linkiodcinkow,"http://www.imdb.com/title/tt[0-9]{7}"))
      linkireviews<-stri_paste(linkireviews,"/reviews?ref_=tt_urv")
      for (j in 1:length(linkireviews))
      {
         adres1<-html(linkireviews[j])
         adresreviews<-html_nodes(adres1,"div+ p")
         recenzje<-html_text(adresreviews)
         recenzje<-stri_replace_all_regex(recenzje,"\n","")
         adresautors<-html_nodes(adres1,"#tn15content div a")
         autorzy<-html_text(adresautors)
         autorzy<-autorzy[autorzy!=""]

         dl<-length(recenzje)
         if(dl!=0)
         {
            ratings<-html_nodes(adres1,"h2 , h2+ img")
            ocena<-html_attr(ratings,"alt")
            ocena1<-na.omit(ocena)
            if(length(ocena1)!=0)
            {
               wek<-1:length(ocena)
               wek1<-wek[-attr(ocena1,"na.action")]
               for(k in 1:length(wek1))
               {
                  wek1[k]<-wek1[k]-k
               }
            }
            else
            {
               wek1<-c()
            }
            ocena1<-unlist(stri_extract_all_regex(ocena1,"[0-9][0-9]*/"))
            ocena1<-stri_replace_all_regex(ocena1,"/","")
            ocena1<-as.numeric(ocena1)
            oceny<-rep(0,(length(ocena)-length(ocena1)))
            oceny[wek1]<-ocena1
            dane_temp<-data.frame("id"=rep(0,dl),"Title_ID"=rep(Title_ID,dl),"Season"=rep(i,dl),"Episode"=rep(j,dl),"login"=autorzy,"Comment"=recenzje,"ocena"=oceny)
            #for(k in 1:dl)
            #{

               #dane_temp<-data.frame("id"=0,"Title_ID"=Title_ID,"Season"=i,"Episode"=j,"login"=autorzy[k],"Comment"=recenzje[k])
               #dane<-rbind(dane,dane_temp)
            #}
            dane<-rbind(dane,dane_temp)
         }

      }



   }
   dane
}

Arrow<-"http://www.imdb.com/title/tt2193021/"
Walking_dead<-"http://www.imdb.com/title/tt1520211/"
Games_of_thrones<-"http://www.imdb.com/title/tt0944947/"
wektornasinteresujacy<-c(Arrow,Walking_dead,Games_of_thrones)

Docalosci<-function(wektoradresow)
{
   tabelka<-data.frame()
   dlg<-length(wektoradresow)
   for(i in 1:dlg)
   {
      dane_temp<-pobierzIMDB(wektoradresow[i])
      tabelka<-rbind(tabelka,dane_temp)
   }
   tabelka$id<-1:nrow(tabelka)
   tabelka
}

(dane<-Docalosci(wektornasinteresujacy))
