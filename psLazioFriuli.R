options(error=function(err) { 
   cat("An error happened you didn't account for\n") 
   cat("\n\n") 
   quit(save='no', status=1) 
})
library(dplyr)
library(plyr)
library(jsonlite)
library(RCurl)
library(EhrscapeR)
library(tidyjson)
library(htmltab)
setwd("/home/argo/workspace/R/psLazioFriuli/")
urlLazio <- "https://www.regione.lazio.it/accessiprontosoccorso/"
urlFriuli <- "https://servizionline.sanita.fvg.it/tempiAttesaService/tempiAttesaPs"
#urlDataFriuli<-"https://servizionline.sanita.fvg.it/psonline/#/index"
#dataLazio<-as.character(NA)
#dataFiuli<-as.character(NA)
#write_file(dataFriuli,"./dataFriuli.txt")
#dataFriuli<-read_file(file="./dataFriuli.txt")
# dataFriuli
#dataLazio<-read.csv()

# ritardo in secondi
#delay<-600
# contatore rilevazioni
# contatore<-as.numeric(c(1:4))
# Ripetizioni del ciclo di durata1C (144 x 10 minuti cioè 600 secondi)
#ripetizioni<-as.numeric(c(1:144))
# Durata ciclo
# durata1C<-(length(contatore)*delay)/3600
# ciclo<-paste("Durata un ciclo: ", durata1C," ore.", sep="")
# print(ciclo)
#durataRC<-(delay*length(ripetizioni))/3600
#ripTot<-paste("Durata totale: ",durataRC," ore.",sep="")
#print(ripTot)
#giorni<-as.numeric(1:30)

###   PRONTO SOCCORSO LAZIO   ###
#r<-1
#nome<-paste("pslazio",r,".csv",sep="")
table<-htmltab(doc = urlLazio, which="//*[@id='tab_right']/table" )
psLazioT<-table
psLazioBak<-psLazioT
psLazioT<-data.frame(psLazioT)
psLazioT<-psLazioT[-(1),]
psLazioT[,c(7:12,14:19,21,23:27,29)] <- sapply(psLazioT[,c(7:12,14:19,21,23:27,29)], as.numeric)
colnames(psLazioT)<-c("Struttura","Comune","Asl","Tipo","Aggiornamento","k6","RossoAt","GialloAt","VerdeAt","BiancoAt","NonAt","TotAt","k13","RossoTr","GialloTr","VerdeTr","BiancoTr","NonTr","TotTr","k20","AttesaRic","k22","RossoOs","GialloOs","VerdeOs","BiancoOs","TotOs","k28","TotTot")
psLazioT3<-psLazioT[,c(-6,-13,-20,-22,-28)]
psLazioT4<-psLazioT3[rowSums(is.na(psLazioT3))==0,]
ps<-psLazioT4
ps$Aggiornamento<-as.character(ps$Aggiornamento)
ps$data<-(substr(ps$Aggiornamento,1,10))
ps$anno<-(substr(ps$data,7,10))
ps$mese<-(substr(ps$data,4,5))
ps$giorno<-(substr(ps$data,1,2))
#ps$data<-(gsub("/","-",ps$data))
ps$ora<-as.character(substr(ps$Aggiornamento,12,13))
ps$minuti<-as.character(substr(ps$Aggiornamento,15,16))
anno<-ps$anno
anno<-as.character(anno[1])
mese<-ps$mese
mese<-as.character(mese[1])
giorno<-ps$giorno
giorno<-as.character(giorno[1])
#data<-ps$data
#data<-as.character(data[1])
ora<-ps$ora
ora<-as.character(ora[1])
minuto<-ps$minuti
minuto<-as.character(minuto[1])

nomeCartellaLazio<-paste("./Lazio/",anno,mese,giorno,"-datiLazio/",sep="")  
nomeFileOrario<-paste(nomeCartellaLazio,anno,mese,giorno,ora,minuto,"-psLazio.csv",sep="")
if (!dir.exists(nomeCartellaLazio)) {
   dir.create(nomeCartellaLazio,recursive=TRUE)
   fileGiornoTotLazio<-paste(nomeCartellaLazio,anno,mese,giorno,"-psLazioTot.csv",sep="")
   write.table(psLazioT4,file=nomeFileOrario,row.names=FALSE,sep=",")
   write.table(psLazioT4,file=fileGiornoTotLazio,row.names=FALSE,sep=",")
} else {
   fileGiornoTotLazio<-paste(nomeCartellaLazio,anno,mese,giorno,"-psLazioTot.csv",sep="")
   psLazioGiornoTot<-read.csv(fileGiornoTotLazio)
   psLazioTot<-rbind(psLazioGiornoTot,psLazioT4)
   write.table(psLazioT4,file=nomeFileOrario,row.names=FALSE,sep=",")
   write.table(psLazioTot,file=fileGiornoTotLazio,row.names=FALSE,sep=",")
   risultato<-paste("Ho salvato la tabella della regione Lazio, del",giorno,mese,anno,"alle",ora,"e",minuto,"minuti.",sep=" ")
   print(risultato)
}

###   PRONTO SOCCORSO FRIULI VENEZIA GIULIA   ###
#h<-1
hospital<-(1:20)
datiT<-fromJSON(urlFriuli, simplifyDataFrame = T)
for (h in hospital) {
   if (h==1) {
      # Questo estrae la tabella del Cattinara...
      # Stampa nome ospedale / Azienda Sanitaria:
      datiTab<-datiT[[2]][[3]][[1]][[3]][[1]][[2]][[1]]
      ospedale<-datiT[[2]][[3]][[1]][[3]][[1]][[2]][[1]]
      datiTab<-data.frame(datiTab)
      n<-c(2:33)
      for (i in n) {
         datiTab[1,i]<-NA
      }
      colnames(datiTab)<-c("Struttura","Comune","Asl","Tipo","Aggiornamento","k6","RossoAt","GialloAt","VerdeAt","BiancoAt","NonAt","TotAt","k13","RossoTr","GialloTr","VerdeTr","BiancoTr","NonTr","TotTr","k20","AttesaRic","k22","RossoOs","GialloOs","VerdeOs","BiancoOs","TotOs","k28","TotTot","AtRosso","AtGiallo","AtVerde","AtBianco")
      # ottieni la tabella dei codici
      datiPs<-datiT[[2]][[3]][[1]][[3]][[1]][[10]][[1]]
      datiPs<-arrange(datiPs, desc(id))
   }
   if (h==2) {
   ospedale<-datiT[[2]][[3]][[1]][[3]][[1]][[2]][[2]]
   # ottieni la tabella dei codici
   datiPs<-datiT[[2]][[3]][[1]][[3]][[1]][[10]][[2]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==3) {
   ospedale<-datiT[[2]][[3]][[2]][[3]][[1]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[2]][[3]][[1]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==4) {
   ospedale<-datiT[[2]][[3]][[3]][[3]][[1]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[3]][[3]][[1]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==5) {
   ospedale<-datiT[[2]][[3]][[3]][[3]][[2]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[3]][[3]][[2]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==6) {
   ospedale<-datiT[[2]][[3]][[3]][[3]][[3]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[3]][[3]][[3]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==7) {
   ospedale<-datiT[[2]][[3]][[3]][[3]][[4]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[3]][[3]][[4]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==8) {
   ospedale<-datiT[[2]][[3]][[3]][[3]][[5]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[3]][[3]][[5]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   
   # Lignano da maggio a settembre
   
   if (h==9) {
   ospedale<-datiT[[2]][[3]][[4]][[3]][[1]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[4]][[3]][[1]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==10) {
   ospedale<-datiT[[2]][[3]][[4]][[3]][[2]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[4]][[3]][[2]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==11) {
   ospedale<-datiT[[2]][[3]][[4]][[3]][[3]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[4]][[3]][[3]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==12) {
   ospedale<-datiT[[2]][[3]][[5]][[3]][[1]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[5]][[3]][[1]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==13) {
   ospedale<-datiT[[2]][[3]][[5]][[3]][[2]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[5]][[3]][[2]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==14) {
   ospedale<-datiT[[2]][[3]][[5]][[3]][[2]][[2]][[2]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[5]][[3]][[2]][[10]][[2]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==15) {
   ospedale<-datiT[[2]][[3]][[6]][[3]][[1]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[6]][[3]][[1]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==16) {
   ospedale<-datiT[[2]][[3]][[6]][[3]][[1]][[2]][[2]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[6]][[3]][[1]][[10]][[2]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==17) {
   ospedale<-datiT[[2]][[3]][[6]][[3]][[1]][[2]][[3]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[6]][[3]][[1]][[10]][[3]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==18) {
   ospedale<-datiT[[2]][[3]][[6]][[3]][[1]][[2]][[4]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[6]][[3]][[1]][[10]][[4]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==19) {
   ospedale<-datiT[[2]][[3]][[6]][[3]][[2]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[6]][[3]][[2]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }
   if (h==20) {
   ospedale<-datiT[[2]][[3]][[6]][[3]][[3]][[2]][[1]]
   datiTab[h,1]<-ospedale
   datiPs<-datiT[[2]][[3]][[6]][[3]][[3]][[10]][[1]]
   datiPs<-arrange(datiPs, desc(id))
   }

   # estrae i codici colori
   datiPsRosso<-datiPs[4,5]
   datiPsGiallo<-datiPs[3,5]
   datiPsVerde<-datiPs[2,5]
   datiPsBianco<-datiPs[1,5]
   # inserisce i codici ATTESA in tabella principale
   pazienti<-c(4,2,1)
   Ro<-7
   Gi<-8
   Ve<-9
   Bi<-10
   #h<-1
   nCol<-1
   for (nCol in pazienti) {
      datiTab[h,Ro]<-datiPsRosso[,nCol]
      datiTab[h,Gi]<-datiPsGiallo[,nCol]
      datiTab[h,Ve]<-datiPsVerde[,nCol]
      datiTab[h,Bi]<-datiPsBianco[,nCol]
      totaleAt<-0
      totaleTot<--1
      nat<-c(Ro:Bi)
      for (nt in nat) {
         numeroAt<-as.numeric(datiTab[h,nt])
         totaleAt<-totaleAt+numeroAt
      }
      datiTab[h,Bi+1]<-NA
      datiTab[h,Bi+2]<-totaleAt
      datiTab[h,Bi+3]<-NA
      Ro<-Ro+7
      Gi<-Gi+7
      Ve<-Ve+7
      Bi<-Bi+7
      if (Ro == 21) {
         datiTab[h,21]<-NA
         datiTab[h,22]<-NA
         Ro<-23
         Gi<-24
         Ve<-25
         Bi<-26
      }
      if (Ro == 30) {
         totaleTot<-datiTab[h,12]+datiTab[h,19]
         datiTab[h,27]<-datiTab[h,28]
         datiTab[h,28]<-NA
         datiTab[h,29]<-totaleTot
         datiTab[h,30]<-as.character(datiPsRosso[1,3])
         datiTab[h,31]<-as.character(datiPsGiallo[1,3])
         datiTab[h,32]<-as.character(datiPsVerde[1,3])
         datiTab[h,33]<-as.character(datiPsBianco[1,3])
      }
      if (totaleAt == totaleTot) {
         risultato<-paste("Tutto ok, ho finito l'ospedale ", ospedale, " , numero ",h,". I pazienti totali sono ",totaleTot, ".", sep="")
         ospedale<-as.character(ospedale)
         datiTab[,1] <- sapply(datiTab[,1],as.character) 
         datiTab[h,1]<-ospedale
         print(risultato)
         h<-h+1
      }
   }
}
aggiornamentoDataFriuli<-paste(giorno,mese,anno,sep="/")
aggiornamentoOrariuli<-paste(ora,minuto,sep=":")
datiTab$Aggiornamento<-paste(aggiornamentoDataFriuli,aggiornamentoOrariuli,sep=" ")

nomeCartellaFriuli<-paste("./Friuli/",anno,mese,giorno,"-datiFriuli/",sep="")  
nomeFileOrario<-paste(nomeCartellaFriuli,anno,mese,giorno,ora,minuto,"-psFriuli.csv",sep="")
if (!dir.exists(nomeCartellaFriuli)) {
   dir.create(nomeCartellaFriuli,recursive=TRUE)
   fileGiornoTotFriuli<-paste(nomeCartellaFriuli,anno,mese,giorno,"-psFriuliTot.csv",sep="")
   write.table(datiTab,file=nomeFileOrario,row.names=FALSE,sep=",")
   write.table(datiTab,file=fileGiornoTotFriuli,row.names=FALSE,sep=",")
} else {
   fileGiornoTotFriuli<-paste(nomeCartellaFriuli,anno,mese,giorno,"-psFriuliTot.csv",sep="")
   psFriuliGiornoTot<-read.csv(fileGiornoTotFriuli)
   psFriuliTot<-rbind(psFriuliGiornoTot,datiTab)
   write.table(datiTab,file=nomeFileOrario,row.names=FALSE,sep=",")
   write.table(psFriuliTot,file=fileGiornoTotFriuli,row.names=FALSE,sep=",")
   risultato<-paste("Ho salvato la tabella della regione Friuli, del",giorno,mese,anno,"alle",ora,"e",minuto,"minuti.",sep=" ")
   print(risultato)
}
