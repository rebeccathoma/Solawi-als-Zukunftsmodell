### Skript deskriptive Auswertung zum Paper: Solidarische Landwirtschaft als Zukunftsmodell? ###
# Autorin: Rebecca Thoma

library(openxlsx)
library(ggplot2) 
library(dplyr)
library(gridExtra)
library(readxl)
library(tidyr)

## Datenaufbereitung ##
UGDFOmitModelldaten <- read.xlsx("Data/DatensatzModellModcompletecases_neu.xlsx")
UG<-read.xlsx("Data/Umfrage_gesamt_mit_MS_1018.xlsx", na.strings=c("", "99"))
sapply(UG, class)

### Umwandeln der Datentypen zu numeric ###
cols.num <- c("Region","F_1", "F_2", "F_3","F_5"	,"F_6"	,"F_7_1"	,"F_7_2"	,"F_7_3"	,"F_7_4"	,"F_7_5"	,"F_7_6"	,"F_7_7"
              ,"F_7_8"	,"F_7_9"	,"F_7_10"	,"F_7_11"	,"F_7_12"	,"F_7_13"	,"F_7_14"	,"F_7_15"	,"F_7_16"	,"F_7_17"	,"F_7_18"	,"F_7_19"	,"F_7_20"	,"F_7_21"	,"F_7_22"	
              ,"F_7_23"	,"F_7_24"	,"F_7_25"	,"F_7_26"	,"F_7_27"	,"F_7_28"	,"F_7_29"	,"F_7_30"	,"F_7_31"	,"F_7_32"	,"F_7_33"	,"F_7_34"	,"F_7_35","F_8",	"F_9"
              ,"F_10_1"	,"F_11_1","F_12"	,"F_13_1"	,"F_14_1")
UG[cols.num] <- sapply(UG[cols.num],as.numeric)
sapply(UG, class)


## Datenauswertung ##
### Vorbereitung der Auswertung nach Siedlungsstruktur ###
PLZSachsenkomplett <- read.csv("Data/PLZSachsenCSV.csv", header=TRUE, sep=",")
table(PLZSachsenkomplett$landkreis)
PLZSachsenkomplett$siedlungsstruktur <- NA
PLZSachsenkomplett$siedlungsstruktur[which(PLZSachsenkomplett$landkreis=="Landkreis GÃ¶rlitz"|PLZSachsenkomplett$landkreis=="Landkreis Nordsachsen")]<-"duenn besiedelter laendlicher Kreis"
PLZSachsenkomplett$siedlungsstruktur[which(PLZSachsenkomplett$landkreis=="Landkreis Leipzig"|PLZSachsenkomplett$landkreis=="Landkreis Mittelsachsen"|PLZSachsenkomplett$landkreis=="Vogtlandkreis"|PLZSachsenkomplett$landkreis=="Landkreis MeiÃŸen"|PLZSachsenkomplett$landkreis=="Landkreis Bautzen"|PLZSachsenkomplett$landkreis=="Landkreis SÃ¤chsische Schweiz-Osterzgebirge")]<-"laendlicher Kreis mit Verdichtungsansaetzen"
PLZSachsenkomplett$siedlungsstruktur[which(PLZSachsenkomplett$landkreis=="Landkreis Zwickau"|PLZSachsenkomplett$landkreis=="Erzgebirgskreis")]<-"staedtischer Kreis"
PLZSachsenkomplett$siedlungsstruktur[which(PLZSachsenkomplett$landkreis=="")]<-"kreisfreie Großstadt"
PLZSachsenkomplett$plz<- paste0("0", PLZSachsenkomplett$plz)

PLZduennbesiedelt <- PLZSachsenkomplett$plz[which(PLZSachsenkomplett$siedlungsstruktur=="duenn besiedelter laendlicher Kreis")]
PLZlaendlKreis <- PLZSachsenkomplett$plz[which(PLZSachsenkomplett$siedlungsstruktur=="laendlicher Kreis mit Verdichtungsansaetzen")]
PLZstaedtKreis <- PLZSachsenkomplett$plz[which(PLZSachsenkomplett$siedlungsstruktur=="staedtischer Kreis")]
PLZkreisfreieGros <- PLZSachsenkomplett$plz[which(PLZSachsenkomplett$siedlungsstruktur=="kreisfreie Großstadt")]

UG$siedlungsstruktur<-NA
UG$siedlungsstruktur[which(is.element(UG$F_15, PLZduennbesiedelt))] <- 1
UG$siedlungsstruktur[which(is.element(UG$F_15, PLZlaendlKreis))] <- 2
UG$siedlungsstruktur[which(is.element(UG$F_15, PLZstaedtKreis))] <- 3
UG$siedlungsstruktur[which(is.element(UG$F_15, PLZkreisfreieGros))] <- 4

table(UG$siedlungsstruktur, useNA = "ifany")
round(prop.table(table(UG$siedlungsstruktur, useNA = "ifany"))*100,2)

#### Interesse ####
table(UG$F_7_1)
interesse <- subset(UG,select=c("Antwort.ID", "F_3", "F_7_1", "F_7_2", "F_7_3"), check.keys=TRUE)
summary(UG$F_7_1)

interesse$F_7_1[which(interesse$F_7_1=="4"|interesse$F_7_1=="5")]="Ja"
interesse$F_7_1[which(interesse$F_7_1=="1"|interesse$F_7_1=="2")]="Nein"
interesse$F_7_1[which(interesse$F_7_1=="3")]="Teils,\nteils"
interesse$F_7_1[which(interesse$F_7_1=="66"|is.na(interesse$F_7_1))]="Weiß nicht/\nkeine Angabe"

table(interesse$F_7_1, useNA = "ifany")
round(prop.table(table(interesse$F_7_1, useNA = "ifany"))*100,2)
# knapp 31% stimmen voll und ganz oder teilweise zu grundsätzlich an einer Mitgliedschaft interessiert zu sein

#### schon vom Konzept gehört ####
table(UG$F_7_19)
gehoert <- subset(UG,select=c("Antwort.ID", "F_7_19"), check.keys=TRUE)

summary(gehoert$F_7_19)
gehoert$F_7_19[which(gehoert$F_7_19=="4"|gehoert$F_7_19=="5")]="Ja"
gehoert$F_7_19[which(gehoert$F_7_19=="1"|gehoert$F_7_19=="2")]="Nein"
gehoert$F_7_19[which(gehoert$F_7_19=="3")]="Teils,\nteils"
gehoert$F_7_19[which(gehoert$F_7_19=="66"|is.na(gehoert$F_7_19))]="Weiß nicht/\nkeine Angabe"

table(gehoert$F_7_19, useNA = "ifany")
round(prop.table(table(gehoert$F_7_19, useNA = "ifany"))*100,2)


#### Interesse nach Siedlungsstrukturtyp ####
interesseclean  <-  interesse$F_7_1
interessecleandf   <-   as.data.frame(table(interesseclean, useNA = "ifany"))
interessecleandf$percentage   <-  interessecleandf$Freq/ sum(interessecleandf$Freq)
interessecleandf$Region <- "alle Regionen"

UG$InteresseF_7_1 <-  UG$F_7_1
UG$InteresseF_7_1[which(UG$InteresseF_7_1=="4"|UG$InteresseF_7_1=="5")]="Ja"
UG$InteresseF_7_1[which(UG$InteresseF_7_1=="1"|UG$InteresseF_7_1=="2")]="Nein"
UG$InteresseF_7_1[which(UG$InteresseF_7_1=="3")]="Teils,\nteils"
UG$InteresseF_7_1[which(UG$InteresseF_7_1=="66"|is.na(UG$InteresseF_7_1))]="Weiß nicht/\nkeine Angabe"


##### alle Regionen #####
drops <- c(2,3,4)
interessecleandf <- interessecleandf[-drops,]
interessecleandf <- interessecleandf %>% select(-one_of("interesseclean", "Freq"))

interessecleandf_2 <- interessecleandf
interessecleandf_2$Region[which(interessecleandf_2$Region=="alle Regionen")] <- "Sachsen Gesamt"
interessecleandf_2$Inhalt <- "Interesse"

##### dünn besiedelter ländlicher Kreis #####
duennbes <- UG[which(UG$siedlungsstruktur=="1"),]
interessecleanduennbes  <-  duennbes$InteresseF_7_1
interessecleanduennbesdf   <-   as.data.frame(table(interessecleanduennbes, useNA = "ifany"))
interessecleanduennbesdf$percentage   <-  interessecleanduennbesdf$Freq/ sum(interessecleanduennbesdf$Freq)

interessecleanduennbesdf <- interessecleanduennbesdf[-drops,]
interessecleanduennbesdf$Region <- "dünn besiedelter\nländlicher Kreis"
interessecleanduennbesdf <- interessecleanduennbesdf %>% select(-one_of("interessecleanduennbes", "Freq"))

interessecleanduennbesdf_2 <- interessecleanduennbesdf
interessecleanduennbesdf_2$Inhalt <- "Interesse"

##### Grossstadt #####
Grossstadt <- UG[which(UG$siedlungsstruktur=="4"),]
interessecleanGrossstadt  <-  Grossstadt$InteresseF_7_1
interessecleanGrossstadtdf   <-   as.data.frame(table(interessecleanGrossstadt, useNA = "ifany"))
interessecleanGrossstadtdf$percentage   <-  interessecleanGrossstadtdf$Freq/ sum(interessecleanGrossstadtdf$Freq)

interessecleanGrossstadtdf <- interessecleanGrossstadtdf[-drops,]
interessecleanGrossstadtdf$Region <- "kreisfreie\nGroßstadt"
interessecleanGrossstadtdf <- interessecleanGrossstadtdf %>% select(-one_of("interessecleanGrossstadt", "Freq"))

interessecleanGrossstadtdf_2 <- interessecleanGrossstadtdf
interessecleanGrossstadtdf_2$Inhalt <- "Interesse"

##### ländlicher Kreis mit Verdichtungsansätzen #####
laendlKreis <- UG[which(UG$siedlungsstruktur=="2"),]
interessecleanlaendlKreis  <-  laendlKreis$InteresseF_7_1
interessecleanlaendlKreisdf   <-   as.data.frame(table(interessecleanlaendlKreis, useNA = "ifany"))
interessecleanlaendlKreisdf$percentage   <-  interessecleanlaendlKreisdf$Freq/ sum(interessecleanlaendlKreisdf$Freq)

interessecleanlaendlKreisdf <- interessecleanlaendlKreisdf[-drops,]
interessecleanlaendlKreisdf$Region <- "ländlicher Kreis mit\nVerdichtungsansätzen"
interessecleanlaendlKreisdf <- interessecleanlaendlKreisdf %>% select(-one_of("interessecleanlaendlKreis", "Freq"))

interessecleanlaendlKreisdf_2 <- interessecleanlaendlKreisdf
interessecleanlaendlKreisdf_2$Inhalt <- "Interesse"

##### städtischer Kreis #####
staedtKreis <- UG[which(UG$siedlungsstruktur=="3"),]
interessecleanstaedtKreis  <-  staedtKreis$InteresseF_7_1
interessecleanstaedtKreisdf   <-   as.data.frame(table(interessecleanstaedtKreis, useNA = "ifany"))
interessecleanstaedtKreisdf$percentage   <-  interessecleanstaedtKreisdf$Freq/ sum(interessecleanstaedtKreisdf$Freq)

interessecleanstaedtKreisdf <- interessecleanstaedtKreisdf[-drops,]
interessecleanstaedtKreisdf$Region <- "städtischer\nKreis"
interessecleanstaedtKreisdf <- interessecleanstaedtKreisdf %>% select(-one_of("interessecleanstaedtKreis", "Freq"))

interessecleanstaedtKreisdf_2 <- interessecleanstaedtKreisdf
interessecleanstaedtKreisdf_2$Inhalt <- "Interesse"

###### Graph ######
interesseVergleich <- interessecleandf %>% bind_rows(interessecleanduennbesdf, interessecleanlaendlKreisdf, interessecleanstaedtKreisdf, interessecleanGrossstadtdf)
interesseVergleich_2 <- interesseVergleich
interesseVergleich_2$Region[which(interesseVergleich_2$Region=="alle Regionen")] <- "Sachsen Gesamt"
interesseVergleich_2$Inhalt <- "Interesse"

interesseVergleichPlot <- ggplot(interesseVergleich, aes(x=Region, y=percentage, fill=Region))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#165CAA", 
                             "darkgreen", 
                             "azure4", 
                             "darkkhaki", 
                             "burlywood4"))+ 
  scale_y_continuous(labels=scales::percent)+
  expand_limits(y=c(0,0.4))+
  theme(legend.title=element_blank())+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())


#### konkrete Pläne nach Siedlungsstruktur ####
unique(interesse$F_7_2)
interesse$F_7_2[which(interesse$F_7_2=="4"|interesse$F_7_2=="5")]="Ja"
interesse$F_7_2[which(interesse$F_7_2=="1"|interesse$F_7_2=="2")]="Nein"
interesse$F_7_2[which(interesse$F_7_2=="3")]="Teils,\nteils"
interesse$F_7_2[which(interesse$F_7_2=="66"|is.na(interesse$F_7_2))]="Weiß nicht/\nkeine Angabe"

konkretePlaeneclean  <-  interesse$F_7_2
konkretePlaenecleandf   <-   as.data.frame(table(konkretePlaeneclean, useNA = "ifany"))
konkretePlaenecleandf$percentage   <-  konkretePlaenecleandf$Freq/ sum(konkretePlaenecleandf$Freq)
konkretePlaenecleandf$Region <- "alle Regionen"

UG$konkretePlaeneF_7_2 <-  UG$F_7_2
UG$konkretePlaeneF_7_2[which(UG$konkretePlaeneF_7_2=="4"|UG$konkretePlaeneF_7_2=="5")]="Ja"
UG$konkretePlaeneF_7_2[which(UG$konkretePlaeneF_7_2=="1"|UG$konkretePlaeneF_7_2=="2")]="Nein"
UG$konkretePlaeneF_7_2[which(UG$konkretePlaeneF_7_2=="3")]="Teils,\nteils"
UG$konkretePlaeneF_7_2[which(UG$konkretePlaeneF_7_2=="66"|is.na(UG$konkretePlaeneF_7_2))]="Weiß nicht/\nkeine Angabe"

drops <- c(2,3,4)
konkretePlaenecleandf <- konkretePlaenecleandf[-drops,]
konkretePlaenecleandf <- konkretePlaenecleandf %>% select(-one_of("konkretePlaeneclean", "Freq"))

konkretePlaenecleandf_2 <- konkretePlaenecleandf
konkretePlaenecleandf_2$Region[which(konkretePlaenecleandf_2$Region=="alle Regionen")] <- "Sachsen Gesamt"
konkretePlaenecleandf_2$Inhalt <- "konkretePlaene"

##### dünn besiedelter ländlicher Kreis #####
duennbes <- UG[which(UG$siedlungsstruktur=="1"),]
konkretePlaenecleanduennbes  <-  duennbes$konkretePlaeneF_7_2
konkretePlaenecleanduennbesdf   <-   as.data.frame(table(konkretePlaenecleanduennbes, useNA = "ifany"))
konkretePlaenecleanduennbesdf$percentage   <-  konkretePlaenecleanduennbesdf$Freq/ sum(konkretePlaenecleanduennbesdf$Freq)

konkretePlaenecleanduennbesdf <- konkretePlaenecleanduennbesdf[-drops,]
konkretePlaenecleanduennbesdf$Region <- "dünn besiedelter\nländlicher Kreis"
konkretePlaenecleanduennbesdf <- konkretePlaenecleanduennbesdf %>% select(-one_of("konkretePlaenecleanduennbes", "Freq"))

konkretePlaenecleanduennbesdf_2 <- konkretePlaenecleanduennbesdf
konkretePlaenecleanduennbesdf_2$Inhalt <- "konkretePlaene"


##### ländlicher Kreis mit Verdichungsansätzen #####
laendlKreis <- UG[which(UG$siedlungsstruktur=="2"),]
konkretePlaenecleanlaendlKreis  <-  laendlKreis$konkretePlaeneF_7_2
konkretePlaenecleanlaendlKreisdf   <-   as.data.frame(table(konkretePlaenecleanlaendlKreis, useNA = "ifany"))
konkretePlaenecleanlaendlKreisdf$percentage   <-  konkretePlaenecleanlaendlKreisdf$Freq/ sum(konkretePlaenecleanlaendlKreisdf$Freq)

konkretePlaenecleanlaendlKreisdf <- konkretePlaenecleanlaendlKreisdf[-drops,]
konkretePlaenecleanlaendlKreisdf$Region <- "ländlicher Kreis mit\nVerdichtungsansätzen"
konkretePlaenecleanlaendlKreisdf <- konkretePlaenecleanlaendlKreisdf %>% select(-one_of("konkretePlaenecleanlaendlKreis", "Freq", "prozentzeichen"))

konkretePlaenecleanlaendlKreisdf_2 <- konkretePlaenecleanlaendlKreisdf
konkretePlaenecleanlaendlKreisdf_2$Inhalt <- "konkretePlaene"

##### städtischer Kreis #####
staedtKreis <- UG[which(UG$siedlungsstruktur=="3"),]
konkretePlaenecleanstaedtKreis  <-  staedtKreis$konkretePlaeneF_7_2
konkretePlaenecleanstaedtKreisdf   <-   as.data.frame(table(konkretePlaenecleanstaedtKreis, useNA = "ifany"))
konkretePlaenecleanstaedtKreisdf$percentage   <-  konkretePlaenecleanstaedtKreisdf$Freq/ sum(konkretePlaenecleanstaedtKreisdf$Freq)

konkretePlaenecleanstaedtKreisdf <- konkretePlaenecleanstaedtKreisdf[-drops,]
konkretePlaenecleanstaedtKreisdf$Region <- "städtischer\nKreis"
konkretePlaenecleanstaedtKreisdf <- konkretePlaenecleanstaedtKreisdf %>% select(-one_of("konkretePlaenecleanstaedtKreis", "Freq", "prozentzeichen"))

konkretePlaenecleanstaedtKreisdf_2 <- konkretePlaenecleanstaedtKreisdf
konkretePlaenecleanstaedtKreisdf_2$Inhalt <- "konkretePlaene"

##### Kreisfreie Großstadt #####
Grossstadt <- UG[which(UG$siedlungsstruktur=="4"),]
konkretePlaenecleanGrossstadt  <-  Grossstadt$konkretePlaeneF_7_2
konkretePlaenecleanGrossstadtdf   <-   as.data.frame(table(konkretePlaenecleanGrossstadt, useNA = "ifany"))
konkretePlaenecleanGrossstadtdf$percentage   <-  konkretePlaenecleanGrossstadtdf$Freq/ sum(konkretePlaenecleanGrossstadtdf$Freq)

konkretePlaenecleanGrossstadtdf <- konkretePlaenecleanGrossstadtdf[-drops,]
konkretePlaenecleanGrossstadtdf$Region <- "kreisfreie\nGroßstadt"
konkretePlaenecleanGrossstadtdf <- konkretePlaenecleanGrossstadtdf %>% select(-one_of("konkretePlaenecleanGrossstadt", "Freq", "prozentzeichen"))

konkretePlaenecleanGrossstadtdf_2 <- konkretePlaenecleanGrossstadtdf
konkretePlaenecleanGrossstadtdf_2$Inhalt <- "konkretePlaene"

###### Dataframes zusammenfügen & Graph erstellen ######
konkretePlaeneVergleich <- konkretePlaenecleandf %>% bind_rows(konkretePlaenecleanduennbesdf, konkretePlaenecleanlaendlKreisdf, konkretePlaenecleanstaedtKreisdf, konkretePlaenecleanGrossstadtdf)

konkretePlaeneVergleich_2 <- konkretePlaeneVergleich
konkretePlaeneVergleich_2$Region[which(konkretePlaeneVergleich_2$Region=="alle Regionen")] <- "Sachsen Gesamt"
konkretePlaeneVergleich_2$Inhalt <- "konkretePlaene"

konkretePlaeneVergleichPlot <- ggplot(konkretePlaeneVergleich, aes(x=Region, y=percentage, fill=Region))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#165CAA", 
                             "darkgreen", 
                             "azure4", 
                             "darkkhaki", 
                             "burlywood4"))+ 
  scale_y_continuous(labels=scales::percent)+
  expand_limits(y=c(0,0.4))+
  theme(legend.title=element_blank())+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())


plotsVergleiche <- grid.arrange(interesseVergleichPlot, konkretePlaeneVergleichPlot, ncol=1, nrow =2)
ggsave(
  "plotsVergleiche.eps",
  plot = plotsVergleiche,
  device = "eps",
  path = NULL,
  scale = 1,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
) 

#### Interesse und konkrete Pläne in einem Plot ####
einPlot <- rbind(interesseVergleich_2, konkretePlaeneVergleich_2)
einPlot$percentage <- as.numeric(einPlot$percentage)

einPlot$Inhalt[which(einPlot$Inhalt=="konkretePlaene")] <- "konkrete Pläne"

einPlot$Region = factor(einPlot$Region, levels = c('Sachsen Gesamt', 'dünn besiedelter\nländlicher Kreis', "ländlicher Kreis mit\nVerdichtungsansätzen", "städtischer\nKreis", "kreisfreie\nGroßstadt"))

Interesse_Plaene_einPlot <- ggplot(einPlot, aes(x=Inhalt, y=percentage, fill=Region))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#165CAA",
                             "darkgreen",
                             "darkkhaki",
                             "azure4",
                             "burlywood4"))+
  scale_y_continuous(labels=scales::percent)+
  expand_limits(y=c(0,0.4))+
  theme(legend.title=element_blank())+
  theme(axis.title.x=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(legend.spacing.y = unit(0.2, 'cm'))+
  guides(fill = guide_legend(byrow = TRUE))

Interesse_Plaene_einPlot

ggsave(
  "InteressePlaene.eps",
  plot = Interesse_Plaene_einPlot,
  device = "eps",
  path = NULL,
  scale = 1,
  width = 18,
  height = 8,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
) 
#### Vergleich der soziodemographischen Daten mit der sächsischen Bevölkerung ####
##### Alter #####
###### Alterszusammensetzung der sächsischen Bevölkerung ######
altersachsen <- read_excel("Data/AltersstrukturSachsen2020.xlsx") 

colnames(altersachsen) <- c("AlterVon...bisUnter...Jahren","Insgesamt", "Maennlich", "Weiblich")
altersachsen$AlterVon...bisUnter...Jahren

altersachsen <- altersachsen[-which(altersachsen$AlterVon...bisUnter...Jahren=="Zusammen"),]
altersachsen <- altersachsen[-c(1:5,83:86),]
altersachsen <- altersachsen[-(1:18), ]  

altersachsen$startJahr <- as.numeric(sub(" ", "", substr(altersachsen$AlterVon...bisUnter...Jahren,1,3), fixed = TRUE))

altersachsen$Alter <- NA 
altersachsen[which(altersachsen$startJahr%in%(18:19)),"Alter"] <- "18-19 Jahre"
altersachsen[which(altersachsen$startJahr%in%20:24),"Alter"] <- "20-24 Jahre"
altersachsen[which(altersachsen$startJahr%in%25:29),"Alter"] <- "25-29 Jahre"
altersachsen[which(altersachsen$startJahr%in%30:34),"Alter"] <- "30-34 Jahre"
altersachsen[which(altersachsen$startJahr%in%35:39),"Alter"] <- "35-39 Jahre"
altersachsen[which(altersachsen$startJahr%in%40:44),"Alter"] <- "40-44 Jahre"
altersachsen[which(altersachsen$startJahr%in%45:49),"Alter"] <- "45-49 Jahre"
altersachsen[which(altersachsen$startJahr%in%50:54),"Alter"] <- "50-54 Jahre"
altersachsen[which(altersachsen$startJahr%in%55:59),"Alter"] <- "55-59 Jahre"
altersachsen[which(altersachsen$startJahr%in%60:64),"Alter"] <- "60-64 Jahre"
altersachsen[which(altersachsen$startJahr%in%65:69),"Alter"] <- "65-69 Jahre"
altersachsen[which(altersachsen$startJahr%in%70:100),"Alter"] <- "70 Jahre und aelter"

sum(is.na(altersachsen))

altersachsen$Insgesamt <- as.numeric(altersachsen$Insgesamt)
altersachsen$Maennlich <- as.numeric(altersachsen$Maennlich)
altersachsen$Weiblich <- as.numeric(altersachsen$Weiblich)

altersachsenvergleich <- aggregate(cbind(Insgesamt,Maennlich,Weiblich)~Alter,altersachsen,sum)

altersachsenvergleich$relativeHSachsen  <- altersachsenvergleich$Insgesamt/sum(altersachsenvergleich$Insgesamt)

###### Alter Umfrageteilnehmende Gesamt ######
alterUmfrage <- subset(UG, select=F_9)
alterUmfrage$F_9[which(alterUmfrage$F_9=="1")]="18-19 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="2")]="20-24 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="3")]="25-29 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="4")]="30-34 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="5")]="35-39 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="6")]="40-44 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="7")]="45-49 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="8")]="50-54 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="9")]="55-59 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="10")]="60-64 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="11")]="65-69 Jahre"
alterUmfrage$F_9[which(alterUmfrage$F_9=="12")]="70 Jahre und aelter"

alterUmfragetabelle <-table(alterUmfrage$F_9)
summary(alterUmfragetabelle)
table(alterUmfrage$F_9, useNA = "ifany")
alterUmfragetabelle <- data.frame(table(alterUmfrage$F_9))
colnames(alterUmfragetabelle) <- c("Alter","HaeufigkeitUmfrage")
alterUmfragetabelle$relativeHUmfrage <-alterUmfragetabelle$HaeufigkeitUmfrage/ sum(alterUmfragetabelle$HaeufigkeitUmfrage)

alterGesamtUeberblick <- merge(alterUmfragetabelle, altersachsenvergleich, by="Alter",all=T)
alterGesamtUeberblick[is.na(alterGesamtUeberblick)] <- 0
cor(alterGesamtUeberblick$relativeHUmfrage,alterGesamtUeberblick$relativeHSachsen)
write.xlsx(alterGesamtUeberblick, file = "AltersVergleichUmfrageSachsen.xlsx")

alterGesamtUeberblickgraph  <-  subset(alterGesamtUeberblick, select=c(Alter, relativeHUmfrage, relativeHSachsen))
alterGesamtUeberblickgraph  <-  gather(alterGesamtUeberblickgraph, daten, anteil, relativeHUmfrage:relativeHSachsen)

ggplot(data=alterGesamtUeberblickgraph)+
  geom_bar(aes(x=Alter, y=anteil,
               fill=daten),
           stat = "identity",
           position="dodge")+
  theme(axis.text.x = element_text(size = 10, angle=90, hjust=1)) +
  theme(legend.position = "none")+
  xlab("")+
  ylab("")


###### Alter Umfrageteilnehmende Modelldatensatz ######
alterModell <- subset(UGDFOmitModelldaten, select=F_9)
alterModell$F_9[which(alterModell$F_9=="1")]="18-19 Jahre"
alterModell$F_9[which(alterModell$F_9=="2")]="20-24 Jahre"
alterModell$F_9[which(alterModell$F_9=="3")]="25-29 Jahre"
alterModell$F_9[which(alterModell$F_9=="4")]="30-34 Jahre"
alterModell$F_9[which(alterModell$F_9=="5")]="35-39 Jahre"
alterModell$F_9[which(alterModell$F_9=="6")]="40-44 Jahre"
alterModell$F_9[which(alterModell$F_9=="7")]="45-49 Jahre"
alterModell$F_9[which(alterModell$F_9=="8")]="50-54 Jahre"
alterModell$F_9[which(alterModell$F_9=="9")]="55-59 Jahre"
alterModell$F_9[which(alterModell$F_9=="10")]="60-64 Jahre"
alterModell$F_9[which(alterModell$F_9=="11")]="65-69 Jahre"
alterModell$F_9[which(alterModell$F_9=="12")]="70 Jahre und aelter"

alterModelltabelle <-table(alterModell$F_9)
alterModelltabelle <- data.frame(table(alterModell$F_9))
colnames(alterModelltabelle) <- c("Alter","HaeufigkeitModell")
alterModelltabelle$relativeHModell <-alterModelltabelle$HaeufigkeitModell/ sum(alterModelltabelle$HaeufigkeitModell)

alterGesamtUeberblickmitModell <- merge(alterGesamtUeberblick, alterModelltabelle, by="Alter",all=T)
alterGesamtUeberblickmitModell[is.na(alterGesamtUeberblickmitModell)] <- 0
write.xlsx(alterGesamtUeberblickmitModell, file = "AltersVergleichUmfrageSachsenmitModell.xlsx")

alterGesamtUeberblickgraph  <-  subset(alterGesamtUeberblick, select=c(Alter, relativeHUmfrage, relativeHSachsen))
alterGesamtUeberblickgraph  <-  gather(alterGesamtUeberblickgraph, daten, anteil, relativeHUmfrage:relativeHSachsen)

ggplot(data=alterGesamtUeberblickgraph)+
  geom_bar(aes(x=Alter, y=anteil,
               fill=daten),
           stat = "identity",
           position="dodge")

###### Plot Alter der Teilnehmenden ######
ggplot(data=alterUmfrage)+
  geom_bar(
    aes(x=F_9, fill=F_9)
  )

alterGesamtUeberblicknew <- alterGesamtUeberblick %>% select(-c("HaeufigkeitUmfrage", "Insgesamt", "Maennlich", "Weiblich"))

###### Alter in Oberkategorien ######
alterUmfrageOberkat <- subset(UG, select=F_9)
alterUmfrageOberkat$F_9[which(alterUmfrageOberkat$F_9=="1"|alterUmfrageOberkat$F_9=="2"|alterUmfrageOberkat$F_9=="3")]="18-29 Jahre"
alterUmfrageOberkat$F_9[which(alterUmfrageOberkat$F_9=="4"|alterUmfrageOberkat$F_9=="5")]="30-39 Jahre"
alterUmfrageOberkat$F_9[which(alterUmfrageOberkat$F_9=="6"|alterUmfrageOberkat$F_9=="7")]="40-49 Jahre"
alterUmfrageOberkat$F_9[which(alterUmfrageOberkat$F_9=="8"|alterUmfrageOberkat$F_9=="9")]="50-59 Jahre"
alterUmfrageOberkat$F_9[which(alterUmfrageOberkat$F_9=="10"|alterUmfrageOberkat$F_9=="11")]="60-69 Jahre"
alterUmfrageOberkat$F_9[which(alterUmfrageOberkat$F_9=="12")]="70 Jahre und aelter"

alterUmfrageOberkattabelle <-table(alterUmfrageOberkat$F_9)
alterUmfrageOberkattabelle <- data.frame(table(alterUmfrageOberkat$F_9))
colnames(alterUmfrageOberkattabelle) <- c("Alter","HaeufigkeitUmfrage")
alterUmfrageOberkattabelle$relativeHUmfrage <-alterUmfrageOberkattabelle$HaeufigkeitUmfrage/ sum(alterUmfrageOberkattabelle$HaeufigkeitUmfrage)


##### Geschlecht #####
unique(UG$F_8)
GeschlechtUmfrage <- subset(UG, select=F_8)
GeschlechtModell <- subset(UGDFOmitModelldaten, select=F_8)

###### Geschlecht Umfrageteilnehmende Gesamt ######
names(GeschlechtUmfrage)[names(GeschlechtUmfrage)=="F_8"] <- "geschlecht"
GeschlechtUmfrage$geschlecht[which(GeschlechtUmfrage$geschlecht=="1")]="Weiblich"
GeschlechtUmfrage$geschlecht[which(GeschlechtUmfrage$geschlecht=="2")]="Maennlich"
GeschlechtUmfrage$geschlecht[which(GeschlechtUmfrage$geschlecht=="3")]="Divers"
table(GeschlechtUmfrage)
round(prop.table(table(GeschlechtUmfrage, useNA = "ifany"))*100,2)
round(prop.table(table(GeschlechtUmfrage))*100,2)

###### Geschlecht Umfrageteilnehmende Modelldaten ######
names(GeschlechtModell)[names(GeschlechtModell)=="F_8"] <- "geschlecht"
GeschlechtModell$geschlecht[which(GeschlechtModell$geschlecht=="1")]="Weiblich"
GeschlechtModell$geschlecht[which(GeschlechtModell$geschlecht=="2")]="Maennlich"
GeschlechtModell$geschlecht[which(GeschlechtModell$geschlecht=="3")]="Divers"
table(GeschlechtModell)
round(prop.table(table(GeschlechtModell, useNA = "ifany"))*100,2)
round(prop.table(table(GeschlechtModell))*100,2)


##### Nettoeinkommen #####
unique(UG$F_12)
HHNettoeinkommenUmfrage <- subset(UG, select=F_12)
HHNettoeinkommenModell <- subset(UGDFOmitModelldaten, select=F_12)

###### Nettoeinkommen Umfrageteilnehmende Gesamtdaten ######
names(HHNettoeinkommenUmfrage)[names(HHNettoeinkommenUmfrage)=="F_12"] <- "HHNettoeinkommen"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="1")]="Bis 500 €"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="2")]="501 € bis 1.000 €"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="3")]="1.001 € bis 1.500 €"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="4")]="1.501 € bis 2.000 €"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="5")]="2.001 € bis 3.000 €"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="6")]="3.001 € bis 4.000 €"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="7")]="4.001 € bis 5.000 €"
HHNettoeinkommenUmfrage$HHNettoeinkommen[which(HHNettoeinkommenUmfrage$HHNettoeinkommen=="8")]="Mehr als 5.000 €"

median(UG$F_12, na.rm=T)

HHNettoeinkommenUmfragetable <- table(HHNettoeinkommenUmfrage$HHNettoeinkommen)
round(prop.table(table(HHNettoeinkommenUmfrage, useNA = "ifany"))*100,2)
round(prop.table(table(HHNettoeinkommenUmfrage)),2)
summary(HHNettoeinkommenUmfragetable)
round(prop.table(table(HHNettoeinkommenUmfrage))*100,2)

hist(UG$F_12)

HHNettoeinkommenUmfrage$HHNettoeinkommen = factor(HHNettoeinkommenUmfrage$HHNettoeinkommen, levels = c("Bis 500 €", "501 € bis 1.000 €", "1.001 € bis 1.500 €", "1.501 € bis 2.000 €", "2.001 € bis 3.000 €", "3.001 € bis 4.000 €", "4.001 € bis 5.000 €", "Mehr als 5.000 €", NA))

ggplot(data=HHNettoeinkommenUmfrage)+
  geom_bar(
    aes(x=HHNettoeinkommen, fill=HHNettoeinkommen)
  )

###### Nettoeinkommen Umfrageteilnehmende Modelldaten ######
names(HHNettoeinkommenModell)[names(HHNettoeinkommenModell)=="F_12"] <- "HHNettoeinkommen"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="1")]="Bis 500 €"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="2")]="501 € bis 1.000 €"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="3")]="1.001 € bis 1.500 €"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="4")]="1.501 € bis 2.000 €"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="5")]="2.001 € bis 3.000 €"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="6")]="3.001 € bis 4.000 €"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="7")]="4.001 € bis 5.000 €"
HHNettoeinkommenModell$HHNettoeinkommen[which(HHNettoeinkommenModell$HHNettoeinkommen=="8")]="Mehr als 5.000 €"

median(UGDFOmit$F_12, na.rm=T)

alterNettoeinkommenModell <- table(HHNettoeinkommenModell$HHNettoeinkommen)
round(prop.table(table(HHNettoeinkommenModell, useNA = "ifany"))*100,2)
round(prop.table(table(HHNettoeinkommenModell)),2)
summary(alterNettoeinkommenModell)
round(prop.table(table(HHNettoeinkommenModell))*100,2)

hist(UGDFOmit$F_12)

HHNettoeinkommenModell$HHNettoeinkommen = factor(HHNettoeinkommenModell$HHNettoeinkommen, levels = c("Bis 500 €", "501 € bis 1.000 €", "1.001 € bis 1.500 €", "1.501 € bis 2.000 €", "2.001 € bis 3.000 €", "3.001 € bis 4.000 €", "4.001 € bis 5.000 €", "Mehr als 5.000 €", NA))

ggplot(data=HHNettoeinkommenModell)+
  geom_bar(
    aes(x=HHNettoeinkommen, fill=HHNettoeinkommen)
  )

table(HHNettoeinkommenUmfrage)
round(prop.table(table(HHNettoeinkommenUmfrage, useNA = "ifany"))*100,2)

##### Bildungsabschluss #####
hochschulabschluss <- subset(UG, select=c("F_10_1", "F_11_1"))
hochschulabschlussModell <- subset(UGDFOmitModelldaten, select=c("F_10_1", "F_11_1"))

###### Bildungsabschluss Umfrageteilnehmende Gesamtdaten ######
unique(hochschulabschluss$F_11_1)
hochschulabschluss$JaNein <- hochschulabschluss$F_11_1
hochschulabschluss$JaNein[which(hochschulabschluss$JaNein =="5"| hochschulabschluss$JaNein =="6" | hochschulabschluss$JaNein =="7")]<- "Ja"
hochschulabschluss$JaNein[which(hochschulabschluss$JaNein =="1"| hochschulabschluss$JaNein =="2" | hochschulabschluss$JaNein =="3"|hochschulabschluss$JaNein =="4")]<- "Nein"
hochschulabschluss$JaNein[which(is.na(hochschulabschluss$JaNein)| hochschulabschluss$JaNein =="88" )]<- NA

round(prop.table(table(hochschulabschluss$JaNein, useNA = "ifany"))*100,2)
round(prop.table(table(hochschulabschluss$JaNein))*100,2)

###### Bildungsabschluss Umfrageteilnehmende Modelldaten ######
unique(hochschulabschlussModell$F_11_1)
hochschulabschlussModell$JaNein <- hochschulabschlussModell$F_11_1
hochschulabschlussModell$JaNein[which(hochschulabschlussModell$JaNein =="5"| hochschulabschlussModell$JaNein =="6" | hochschulabschlussModell$JaNein =="7")]<- "Ja"
hochschulabschlussModell$JaNein[which(hochschulabschlussModell$JaNein =="1"| hochschulabschlussModell$JaNein =="2" | hochschulabschlussModell$JaNein =="3"|hochschulabschlussModell$JaNein =="4")]<- "Nein"
hochschulabschlussModell$JaNein[which(is.na(hochschulabschlussModell$JaNein)| hochschulabschlussModell$JaNein =="88" )]<- NA

round(prop.table(table(hochschulabschlussModell$JaNein, useNA = "ifany"))*100,2)
round(prop.table(table(hochschulabschlussModell$JaNein))*100,2)


#### Gründe für Nicht-Interesse ####
UG$F_4
nrow(UG[!is.na(UG$F_4),])
nrow(na.omit(UG$F_4))

UG$GruendeNichtInteresse_kat <- NA
UG$GruendeNichtInteresse_kat[2]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[4]<- "Konzept nicht ansprechend"
UG$GruendeNichtInteresse_kat[5]<- "nichts"
UG$GruendeNichtInteresse_kat[6]<- "nichts"
UG$GruendeNichtInteresse_kat[8]<- "Lebensumstaende Alter, Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[9]<- "Konzept nicht ansprechend"
UG$GruendeNichtInteresse_kat[10]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[11]<- "kein Interesse"
UG$GruendeNichtInteresse_kat[13]<- "nichts"
UG$GruendeNichtInteresse_kat[14]<- "nichts"
UG$GruendeNichtInteresse_kat[15]<- "nichts"
UG$GruendeNichtInteresse_kat[16]<- "Finanzielles, Entfernung"
UG$GruendeNichtInteresse_kat[18]<- "fehlender Bedarf Andere, Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[19]<- "Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[20]<- "nichts"
UG$GruendeNichtInteresse_kat[21]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[23]<- "Abnahmezwang"
UG$GruendeNichtInteresse_kat[25]<- "Abnahmezwang, fehlende Mengenauswahl, Finanzielles, Preis-Leistung"
UG$GruendeNichtInteresse_kat[27]<- "nichts"
UG$GruendeNichtInteresse_kat[28]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[29]<- "nichts"
UG$GruendeNichtInteresse_kat[30]<- "Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[31]<- "Lebensumstaende Alter, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[32]<- "Lebensumstaende Alter, Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[33]<- "Finanzielles"
UG$GruendeNichtInteresse_kat[34]<- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[35]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[36]<- "Lebensumstaende Andere"
UG$GruendeNichtInteresse_kat[37]<- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[40]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[41]<- "nichts"
UG$GruendeNichtInteresse_kat[42]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[43]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[45]<- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[47]<- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[50]<- "Entfernung, Lebensumstaende Wohnsituation"

UG$GruendeNichtInteresse_kat[51]<- "Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[52]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[53]<- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[54]<- "Lebensumstaende Alter, Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[56]<- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[57]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[58]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[59]<- "keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[60]<- "Finanzielles, Preis-Leistung"
UG$GruendeNichtInteresse_kat[61]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[62]<- "kein Interesse"
UG$GruendeNichtInteresse_kat[63]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[64]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[65]<- "Lebensumstaende Alter, Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[66]<- "nichts"
UG$GruendeNichtInteresse_kat[67]<- "Lebensumstaende Alter, keine Verfuegbarkeit vor Ort, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[68]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[69]<- "nichts"
UG$GruendeNichtInteresse_kat[70]<- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[71]<- "Entfernung"
UG$GruendeNichtInteresse_kat[72]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[75]<- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[76]<- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[77]<- "fehlende Info oder Erfahrung, Lebensumstaende Wohnsituation, Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[78]<- "Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[80]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[81]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[82]<- "nichts"
UG$GruendeNichtInteresse_kat[84]<- "nichts"
UG$GruendeNichtInteresse_kat[85]<- "Zeit"
UG$GruendeNichtInteresse_kat[86]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[87]<- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[88]<- "Lebensumstaende Wohnsituation, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[89]<- "eigener Anbau - fehlender Bedarf, fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[90]<- "Lebensumstaende Alter, Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[91]<- "Lebensumstaende Gesundheit, Finanzielles"
UG$GruendeNichtInteresse_kat[93]<- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[94]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[95]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[96]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[97]<- "nichts"
UG$GruendeNichtInteresse_kat[98]<- "kein Interesse"
UG$GruendeNichtInteresse_kat[99]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[100]<- "fehlender Bedarf Andere"

UG$GruendeNichtInteresse_kat[101]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[102]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[104]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[105]<- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[106]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[107]<- "Zeit"
UG$GruendeNichtInteresse_kat[109]<- "nichts"
UG$GruendeNichtInteresse_kat[110]<- "Lebensumstaende Wohnsituation, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[113]<- "Finanzielles"
UG$GruendeNichtInteresse_kat[115]<- "keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[117]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[118]<- "nichts"
UG$GruendeNichtInteresse_kat[119]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[120]<- "Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[123]<- "keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[126]<- "Risiko"
UG$GruendeNichtInteresse_kat[127]<- "Zeit"
UG$GruendeNichtInteresse_kat[129]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[130]<- "Finanzielles"
UG$GruendeNichtInteresse_kat[131]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[143]<- "Finanzielles, Preis-Leistung"
UG$GruendeNichtInteresse_kat[145]<- "Zeit"
UG$GruendeNichtInteresse_kat[146]<- "nichts"
UG$GruendeNichtInteresse_kat[148]<- "fehlende Info oder Erfahrung, Preis-Leistung"
UG$GruendeNichtInteresse_kat[149]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[152]<- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[154]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[156]<- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[158]<- "Finanzielles"
UG$GruendeNichtInteresse_kat[160]<- "nichts"
UG$GruendeNichtInteresse_kat[161]<- "Konzept nicht ansprechend, Finanzielles"
UG$GruendeNichtInteresse_kat[162]<- "Lebensumstaende Alter, Lebensumstaende Gesundheit, Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[163]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[165]<- "nichts"
UG$GruendeNichtInteresse_kat[167]<- "Abnahmezwang"
UG$GruendeNichtInteresse_kat[171]<- "nichts"
UG$GruendeNichtInteresse_kat[172]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[175]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[177]<- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[178]<- "Zeit"
UG$GruendeNichtInteresse_kat[180]<- "nichts"
UG$GruendeNichtInteresse_kat[181]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[184]<- "nichts"
UG$GruendeNichtInteresse_kat[188]<- "Zeit"
UG$GruendeNichtInteresse_kat[190]<- "nichts"
UG$GruendeNichtInteresse_kat[194]<- "Sonstiges"
UG$GruendeNichtInteresse_kat[197]<- "Zeit"
UG$GruendeNichtInteresse_kat[199]<- "nichts"

UG$GruendeNichtInteresse_kat[201]<- "Abnahmezwang, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[206]<- "kein Interesse"
UG$GruendeNichtInteresse_kat[208]<- "Lebensumstaende Wohnsituation, eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[210]<- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[212]<- "nichts"
UG$GruendeNichtInteresse_kat[214]<- "Finanzielles, Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[217]<- "kein Interesse"
UG$GruendeNichtInteresse_kat[218]<- "nichts"
UG$GruendeNichtInteresse_kat[222]<- "nichts, eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[223]<- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[225]<- "Zeit, Finanzielles"
UG$GruendeNichtInteresse_kat[226]<- "Finanzielles, Verpflichtung oder Beteiligung, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[227]<- "nichts"
UG$GruendeNichtInteresse_kat[228]<- "keine Verfuegbarkeit vor Ort, Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[230]<- "keine Verfuegbarkeit vor Ort, Finanzielles"
UG$GruendeNichtInteresse_kat[231] <- "Zeit"
UG$GruendeNichtInteresse_kat[232] <- "nichts"
UG$GruendeNichtInteresse_kat[235] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[236] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[237] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[239] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[242] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[243] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[245] <- "Zeit"
UG$GruendeNichtInteresse_kat[246] <- "Zeit, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[248] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[250] <- "Zeit"
UG$GruendeNichtInteresse_kat[252] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[254] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[255] <- "nichts"
UG$GruendeNichtInteresse_kat[258] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[260] <- "nichts"
UG$GruendeNichtInteresse_kat[261] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[262] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[264] <- "kein Interesse"
UG$GruendeNichtInteresse_kat[265] <- "Konzept nicht ansprechend"
UG$GruendeNichtInteresse_kat[267] <- "nichts"
UG$GruendeNichtInteresse_kat[269] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[270] <- "Zeit, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[271] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[275] <- "kein Interesse"
UG$GruendeNichtInteresse_kat[278] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[280] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[281] <- "Risiko, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[282] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[286] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[287] <- "Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[289] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[290] <- "nichts"
UG$GruendeNichtInteresse_kat[293] <- "keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[294] <- "Entfernung, Zeit"
UG$GruendeNichtInteresse_kat[295] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[296] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[297] <- "nichts"
UG$GruendeNichtInteresse_kat[298] <- "kein Interesse"

UG$GruendeNichtInteresse_kat[303] <- "Finanzielles, Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[305] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[309] <- "Zeit"
UG$GruendeNichtInteresse_kat[315] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[320] <- "Zeit"
UG$GruendeNichtInteresse_kat[326] <- "nichts"
UG$GruendeNichtInteresse_kat[331] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[332] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[336] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[337] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[338] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[339] <- "nichts"
UG$GruendeNichtInteresse_kat[342] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[343] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[345] <- "nichts"
UG$GruendeNichtInteresse_kat[346] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[355] <- "fehlende Flexibilitaet, Sonstiges"
UG$GruendeNichtInteresse_kat[357] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[358] <- "Finanzielles, Bindung bzw. Mitgliedschaft, fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[361] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[362] <- "nichts"
UG$GruendeNichtInteresse_kat[370] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[373] <- "Zeit"
UG$GruendeNichtInteresse_kat[379] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[382] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[385] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[389] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[391] <- "Zeit"
UG$GruendeNichtInteresse_kat[396] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[397] <- "nichts"
UG$GruendeNichtInteresse_kat[398] <- "Zeit"

UG$GruendeNichtInteresse_kat[403] <- "nichts"
UG$GruendeNichtInteresse_kat[405] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[406] <- "Abnahmezwang, fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[407] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[409] <- "nichts"
UG$GruendeNichtInteresse_kat[410] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[418] <- "nichts"
UG$GruendeNichtInteresse_kat[419] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[420] <- "fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[423] <- "fehlende Flexibilitaet, Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[424] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[425] <- "Zeit"
UG$GruendeNichtInteresse_kat[426] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[428] <- "nichts"
UG$GruendeNichtInteresse_kat[430] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[433] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[434] <- "nichts"
UG$GruendeNichtInteresse_kat[438] <- "Zeit"
UG$GruendeNichtInteresse_kat[440] <- "nichts, Preis-Leistung"
UG$GruendeNichtInteresse_kat[443] <- "nichts"
UG$GruendeNichtInteresse_kat[444] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[445] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[446] <- "Zeit"
UG$GruendeNichtInteresse_kat[448] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[452] <- "nichts"
UG$GruendeNichtInteresse_kat[454] <- "Zeit"
UG$GruendeNichtInteresse_kat[457] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[458] <- "Risiko"
UG$GruendeNichtInteresse_kat[460] <- "fehlender Bedarf Andere, Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[463] <- "nichts"
UG$GruendeNichtInteresse_kat[465] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[467] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[474] <- "nichts"
UG$GruendeNichtInteresse_kat[476] <- "nichts"
UG$GruendeNichtInteresse_kat[479] <- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[480] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[481] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[482] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[483] <- "nichts"
UG$GruendeNichtInteresse_kat[485] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[486] <- "Entfernung"
UG$GruendeNichtInteresse_kat[487] <- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[488] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[490] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[491] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[493] <- "Lebensumstaende Gesundheit, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[496] <- "Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[497] <- "Bindung bzw. Mitgliedschaft, fehlender Bedarf Andere"

UG$GruendeNichtInteresse_kat[500] <- "Entfernung, Verpflichtung oder Beteiligung, Finanzielles, Sonstiges"
UG$GruendeNichtInteresse_kat[503] <- "nichts"
UG$GruendeNichtInteresse_kat[505] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[506] <- "keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[507] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[508] <- "Verpflichtung oder Beteiligung, Finanzielles, Sonstiges"
UG$GruendeNichtInteresse_kat[509] <- "Entfernung, fehlende Produktauswahl, Preis-Leistung"
UG$GruendeNichtInteresse_kat[510] <- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[511] <- "nichts"
UG$GruendeNichtInteresse_kat[512] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[513] <- "nichts"
UG$GruendeNichtInteresse_kat[514] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[515] <- "fehlende Flexibilitaet, fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[516] <- "Zeit"
UG$GruendeNichtInteresse_kat[517] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[518] <- "nichts"
UG$GruendeNichtInteresse_kat[519] <- "eigener Anbau - fehlender Bedarf, Zeit"
UG$GruendeNichtInteresse_kat[520] <- "Zeit"
UG$GruendeNichtInteresse_kat[521] <- "nichts"
UG$GruendeNichtInteresse_kat[522] <- "Entfernung"
UG$GruendeNichtInteresse_kat[523] <- "Preis-Leistung, fehlender Bedarf Andere, Sonstiges"
UG$GruendeNichtInteresse_kat[524] <- "fehlende Info oder Erfahrung, Sonstiges"
UG$GruendeNichtInteresse_kat[525] <- "Entfernung, Preis-Leistung"
UG$GruendeNichtInteresse_kat[526] <- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[527] <- "Verpflichtung oder Beteiligung, Finanzielles"
UG$GruendeNichtInteresse_kat[528] <- "Zeit"
UG$GruendeNichtInteresse_kat[529] <- "nichts"
UG$GruendeNichtInteresse_kat[531] <- "Konzept nicht ansprechend"
UG$GruendeNichtInteresse_kat[532] <- "Zeit, Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[533] <- "nichts, Preis-Leistung"
UG$GruendeNichtInteresse_kat[534] <- "Zeit, Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[535] <- "Abholung, fehlende Info oder Erfahrung, Lebensumstaende Gesundheit"
UG$GruendeNichtInteresse_kat[536] <- "Abnahmezwang, fehlende Produktauswahl, Sonstiges"
UG$GruendeNichtInteresse_kat[537] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[538] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[539] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[540] <- "Verpflichtung oder Beteiligung, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[541] <- "fehlende Info oder Erfahrung, Preis-Leistung"
UG$GruendeNichtInteresse_kat[542] <- "fehlende Produktauswahl, Entfernung"
UG$GruendeNichtInteresse_kat[543] <- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[544] <- "Zeit"
UG$GruendeNichtInteresse_kat[548] <- "kein Interesse, Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[549] <- "fehlende Flexibilitaet, fehlende Produktauswahl, fehlende Mengenauswahl"
UG$GruendeNichtInteresse_kat[550] <- "fehlende Flexibilitaet, Entfernung"
UG$GruendeNichtInteresse_kat[551] <- "fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[555] <- "Risiko"
UG$GruendeNichtInteresse_kat[556] <- "Finanzielles, fehlende Flexibilitaet, fehlende Produktauswahl, Bindung bzw. Mitgliedschaft, Abnahmezwang"
UG$GruendeNichtInteresse_kat[557] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[559] <- "Preis-Leistung, Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[560] <- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[562] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[563] <- "fehlende Mengenauswahl, Abholung, Sonstiges"
UG$GruendeNichtInteresse_kat[564] <- "Abholung, Zeit, Finanzielles"
UG$GruendeNichtInteresse_kat[566] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[567] <- "Entfernung"
UG$GruendeNichtInteresse_kat[568] <- "fehlende Flexibilitaet, Bindung bzw. Mitgliedschaft, Abnahmezwang"
UG$GruendeNichtInteresse_kat[571] <- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[573] <- "nichts"
UG$GruendeNichtInteresse_kat[575] <- "keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[577] <- "Lebensumstaende Wohnsituation, fehlende Flexibilitaet, Abholung"
UG$GruendeNichtInteresse_kat[578] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[580] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[581] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[585] <- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[588] <- "Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[590] <- "Bindung bzw. Mitgliedschaft, Abholung, fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[594] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[595] <- "fehlender Bedarf Andere, Zeit, kein Interesse"
UG$GruendeNichtInteresse_kat[596] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[597] <- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[598] <- "fehlende Produktauswahl"

UG$GruendeNichtInteresse_kat[600] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[601] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[602] <- "fehlende Info oder Erfahrung"
UG$GruendeNichtInteresse_kat[603] <- "fehlende Flexibilitaet, Bindung bzw. Mitgliedschaft, Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[604] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[605] <- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[606] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[607] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[608] <- "Konzept nicht ansprechend, Abnahmezwang, Verpflichtung oder Beteiligung, fehlende Mengenauswahl"
UG$GruendeNichtInteresse_kat[610] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[612] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[613] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[615] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[616] <- "fehlender Bedarf Andere, Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[617] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[618] <- "Zeit, Finanzielles"
UG$GruendeNichtInteresse_kat[622] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[626] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[629] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[631] <- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[633] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[634] <- "Lebensumstaende Alter, Sonstiges"
UG$GruendeNichtInteresse_kat[635] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[636] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[639] <- "fehlende Flexibilitaet, fehlende Produktauswahl, fehlende Mengenauswahl, Entfernung"
UG$GruendeNichtInteresse_kat[640] <- "Lebensumstaende Alter, eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[641] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[644] <- "fehlender Bedarf Andere, Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[645] <- "nichts"
UG$GruendeNichtInteresse_kat[646] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[647] <- "Finanzielles, keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[650] <- "Konzept nicht ansprechend"
UG$GruendeNichtInteresse_kat[651] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[652] <- "Lebensumstaende Alter, fehlende Produktauswahl, Entfernung"
UG$GruendeNichtInteresse_kat[653] <- "Lebensumstaende Andere"
UG$GruendeNichtInteresse_kat[654] <- "Abholung, Risiko, Abnahmezwang"
UG$GruendeNichtInteresse_kat[656] <- "fehlender Bedarf Andere, Lebensumstaende Wohnsituation, eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[660] <- "Lebensumstaende Alter, Lebensumstaende Gesundheit, Sonstiges"
UG$GruendeNichtInteresse_kat[661] <- "eigener Anbau - fehlender Bedarf, Entfernung"
UG$GruendeNichtInteresse_kat[662] <- "fehlende Flexibilitaet, fehlender Bedarf Andere, Lebensumstaende Wohnsituation, Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[663] <- "fehlender Bedarf Andere, Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[665] <- "nichts"
UG$GruendeNichtInteresse_kat[666] <- "fehlende Produktauswahl, eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[667] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[668] <- "nichts"
UG$GruendeNichtInteresse_kat[669] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[671] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[673] <- "Konzept nicht ansprechend, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[674] <- "fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[679] <- "Finanzielles, Lebensumstaende Alter, eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[682] <- "Risiko"
UG$GruendeNichtInteresse_kat[685] <- "Zeit"
UG$GruendeNichtInteresse_kat[686] <- "Finanzielles, fehlende Mengenauswahl, fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[688] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[689] <- "Zeit"
UG$GruendeNichtInteresse_kat[692] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[694] <- "fehlender Bedarf Andere, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[695] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[697] <- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[698] <- "fehlende Produktauswahl"

UG$GruendeNichtInteresse_kat[702] <- "Verpflichtung oder Beteiligung, Bindung bzw. Mitgliedschaft, Lebensumstaende Gesundheit, Abholung"
UG$GruendeNichtInteresse_kat[704] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[706] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[707] <- "Zeit"
UG$GruendeNichtInteresse_kat[708] <- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[709] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[710] <- "Zeit, Finanzielles"
UG$GruendeNichtInteresse_kat[712] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[714] <- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[716] <- "Konzept nicht ansprechend"
UG$GruendeNichtInteresse_kat[717] <- "Lebensumstaende Alter, Lebensumstaende Wohnsituation"
UG$GruendeNichtInteresse_kat[718] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[719] <- "Verpflichtung oder Beteiligung, fehlende Produktauswahl, Risiko"
UG$GruendeNichtInteresse_kat[720] <- "keine Verfuegbarkeit vor Ort"
UG$GruendeNichtInteresse_kat[721] <- "Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[722] <- "nichts"
UG$GruendeNichtInteresse_kat[723] <- "fehlende Produktauswahl, fehlende Mengenauswahl, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[724] <- "fehlende Mengenauswahl, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[725] <- "Sonstiges"
UG$GruendeNichtInteresse_kat[726] <- "Zeit, Lebensumstaende Andere"
UG$GruendeNichtInteresse_kat[727] <- "Verpflichtung oder Beteiligung, Bindung bzw. Mitgliedschaft"
UG$GruendeNichtInteresse_kat[729] <- "Verpflichtung oder Beteiligung, Zeit"
UG$GruendeNichtInteresse_kat[730] <- "Lebensumstaende Wohnsituation, fehlender Bedarf Andere, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[732] <- "Lebensumstaende Alter, Lebensumstaende Wohnsituation, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[733] <- "Bindung bzw. Mitgliedschaft, fehlende Flexibilitaet, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[734] <- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[736] <- "fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[737] <- "Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[738] <- "Lebensumstaende Alter, Lebensumstaende Andere, fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[739] <- "fehlende Produktauswahl, Abholung, Preis-Leistung"
UG$GruendeNichtInteresse_kat[740] <- "Finanzielles"
UG$GruendeNichtInteresse_kat[741] <- "Entfernung, Lebensumstaende Andere, fehlende Flexibilitaet"
UG$GruendeNichtInteresse_kat[742] <- "fehlende Flexibilitaet, fehlende Mengenauswahl"
UG$GruendeNichtInteresse_kat[743] <- "Zeit"
UG$GruendeNichtInteresse_kat[744] <- "Lebensumstaende Wohnsituation, Abholung"
UG$GruendeNichtInteresse_kat[745] <- "fehlende Produktauswahl, fehlende Mengenauswahl, Abholung"
UG$GruendeNichtInteresse_kat[747] <- "Zeit, eigener Anbau - fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[748] <- "Abnahmezwang"
UG$GruendeNichtInteresse_kat[749] <- "eigener Anbau - fehlender Bedarf, Lebensumstaende Alter"
UG$GruendeNichtInteresse_kat[751] <- "fehlende Info oder Erfahrung, Entfernung, fehlende Produktauswahl"
UG$GruendeNichtInteresse_kat[754] <- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[756] <- "fehlender Bedarf Andere"
UG$GruendeNichtInteresse_kat[757] <- "Finanzielles, Lebensumstaende Andere"
UG$GruendeNichtInteresse_kat[758] <- "fehlender Bedarf Andere, fehlende Mengenauswahl"
UG$GruendeNichtInteresse_kat[760] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[761] <- "Verpflichtung oder Beteiligung"
UG$GruendeNichtInteresse_kat[762] <- "eigener Anbau - fehlender Bedarf"
UG$GruendeNichtInteresse_kat[763] <- "Konzept nicht ansprechend"


GruendeNichtInteressedf <- as.data.frame(UG[,"GruendeNichtInteresse_kat"])
names(GruendeNichtInteressedf)[1] <- "Gruende"

library(tidyverse)
library(janitor)

GruendeNichtInteressedfneu <- as.data.frame(str_split_fixed(GruendeNichtInteressedf$Gruende, ", ", 5)) 

Gruende_kat <- as.data.frame(unique(c(GruendeNichtInteressedfneu[,1], GruendeNichtInteressedfneu[,2], GruendeNichtInteressedfneu[,3], GruendeNichtInteressedfneu[,4], GruendeNichtInteressedfneu[,5])))
names(Gruende_kat)[1] <- "Gruende_kat"
write.xlsx(Gruende_kat, file = "Gruende_Kategorien.xlsx")



Abholung <- as.data.frame(UG$F_4[which(grepl("Abholung", UG$GruendeNichtInteresse_kat))])
names(Abholung)[1] <- "Abholung"
summary(Abholung)
Abholungdf <- as.data.frame(summary(Abholung))

# 10 mal genannt

Abnahmezwang <- as.data.frame(UG$F_4[which(grepl("Abnahmezwang", UG$GruendeNichtInteresse_kat))])
names(Abnahmezwang)[1] <- "Abnahmezwang"
summary(Abnahmezwang)
Abnahmezwangdf <- as.data.frame(summary(Abnahmezwang))
# 11 mal genannt

BindungbzwMitgliedschaft<- as.data.frame(UG$F_4[which(grepl("Bindung bzw. Mitgliedschaft", UG$GruendeNichtInteresse_kat))])
names(BindungbzwMitgliedschaft)[1] <- "BindungbzwMitgliedschaft"
summary(BindungbzwMitgliedschaft)
BindungbzwMitgliedschaftdf <- as.data.frame(summary(BindungbzwMitgliedschaft))
# 27 mal genannt

eigenerAnbauFehlenderBedarf  <- as.data.frame(UG$F_4[which(grepl("eigener Anbau - fehlender Bedarf", UG$GruendeNichtInteresse_kat))])
names(eigenerAnbauFehlenderBedarf)[1] <- "eigenerAnbauFehlenderBedarf"
summary(eigenerAnbauFehlenderBedarf)
eigenerAnbauFehlenderBedarfdf <- as.data.frame(summary(eigenerAnbauFehlenderBedarf))
# 58 mal genannt

Entfernung<- as.data.frame(UG$F_4[which(grepl("Entfernung", UG$GruendeNichtInteresse_kat))])
names(Entfernung)[1] <- "Entfernung"
summary(Entfernung)
Entfernungdf <- as.data.frame(summary(Entfernung))
# 17 mal genannt

fehlendeFlexibilitaet  <- as.data.frame(UG$F_4[which(grepl("fehlende Flexibilitaet", UG$GruendeNichtInteresse_kat))])
names(fehlendeFlexibilitaet)[1] <- "fehlendeFlexibilitaet"
summary(fehlendeFlexibilitaet)
fehlendeFlexibilitaetdf <- as.data.frame(summary(fehlendeFlexibilitaet))
# 24 mal genannt

fehlendeMengenauswahl  <- as.data.frame(UG$F_4[which(grepl("fehlende Mengenauswahl", UG$GruendeNichtInteresse_kat))])
names(fehlendeMengenauswahl)[1] <- "fehlendeMengenauswahl"
summary(fehlendeMengenauswahl)
fehlendeMengenauswahldf <- as.data.frame(summary(fehlendeMengenauswahl))
# 11 mal genannt

fehlendeProduktauswahl  <- as.data.frame(UG$F_4[which(grepl("fehlende Produktauswahl", UG$GruendeNichtInteresse_kat))])
names(fehlendeProduktauswahl)[1] <- "fehlendeProduktauswahl"
summary(fehlendeProduktauswahl)
fehlendeProduktauswahldf <- as.data.frame(summary(fehlendeProduktauswahl))
# 28 mal genannt

fehlenderBedarfAndere  <- as.data.frame(UG$F_4[which(grepl("fehlender Bedarf Andere", UG$GruendeNichtInteresse_kat))])
names(fehlenderBedarfAndere)[1] <- "fehlenderBedarfAndere"
summary(fehlenderBedarfAndere)
fehlenderBedarfAnderedf <- as.data.frame(summary(fehlenderBedarfAndere))
# 36 mal genannt

Finanzielles  <- as.data.frame(UG$F_4[which(grepl("Finanzielles", UG$GruendeNichtInteresse_kat))])
names(Finanzielles)[1] <- "Finanzielles"
summary(Finanzielles)
Finanziellesdf <- as.data.frame(summary(Finanzielles))
# 48 mal genannt

keinInteresse  <- as.data.frame(UG$F_4[which(grepl("kein Interesse", UG$GruendeNichtInteresse_kat))])
names(keinInteresse)[1] <- "keinInteresse"
summary(keinInteresse)
keinInteressedf <- as.data.frame(summary(keinInteresse))
# 10 mal genannt

keineVerfuegbarkeitvorOrt  <- as.data.frame(UG$F_4[which(grepl("keine Verfuegbarkeit vor Ort", UG$GruendeNichtInteresse_kat))])
names(keineVerfuegbarkeitvorOrt)[1] <- "keineVerfuegbarkeitvorOrt"
summary(keineVerfuegbarkeitvorOrt)
keineVerfuegbarkeitvorOrtdf <- as.data.frame(summary(keineVerfuegbarkeitvorOrt))
# 11 mal genannt

KonzeptNichtAnsprechend <- as.data.frame(UG$F_4[which(grepl("Konzept nicht ansprechend", UG$GruendeNichtInteresse_kat))])
names(KonzeptNichtAnsprechend)[1] <- "KonzeptNichtAnsprechend"
summary(KonzeptNichtAnsprechend)
KonzeptNichtAnsprechenddf <- as.data.frame(summary(KonzeptNichtAnsprechend))
# 10 mal genannt

LebensumstaendeAlter <- as.data.frame(UG$F_4[which(grepl("Lebensumstaende Alter", UG$GruendeNichtInteresse_kat))])
names(LebensumstaendeAlter)[1] <- "LebensumstaendeAlter"
summary(LebensumstaendeAlter)
LebensumstaendeAlterdf <- as.data.frame(summary(LebensumstaendeAlter))
# 29 mal genannt

LebensumstaendeGesundheit  <- as.data.frame(UG$F_4[which(grepl("Lebensumstaende Gesundheit", UG$GruendeNichtInteresse_kat))])
names(LebensumstaendeGesundheit)[1] <- "LebensumstaendeGesundheit"
summary(LebensumstaendeGesundheit)
LebensumstaendeGesundheitdf <- as.data.frame(summary(LebensumstaendeGesundheit))
# 15 mal genannt

LebensumstaendeWohnsituation <- as.data.frame(UG$F_4[which(grepl("Lebensumstaende Wohnsituation", UG$GruendeNichtInteresse_kat))])
names(LebensumstaendeWohnsituation)[1] <- "LebensumstaendeWohnsituation"
summary(LebensumstaendeWohnsituation)
LebensumstaendeWohnsituationdf <- as.data.frame(summary(LebensumstaendeWohnsituation))
# 22 mal genannt

LebensumstaendeAndere<- as.data.frame(UG$F_4[which(grepl("Lebensumstaende Andere", UG$GruendeNichtInteresse_kat))])
names(LebensumstaendeAndere)[1] <- "LebensumstaendeAndere"
summary(LebensumstaendeAndere)
LebensumstaendeAnderedf <- as.data.frame(summary(LebensumstaendeAndere))
# 6 mal genannt

Nichts <- as.data.frame(UG$F_4[which(grepl("nichts", UG$GruendeNichtInteresse_kat))])
names(Nichts)[1] <- "Nichts"
summary(Nichts)
Nichtsdf <- as.data.frame(summary(Nichts))
# 63 mal genannt

PreisLeistung <- as.data.frame(UG$F_4[which(grepl("Preis-Leistung", UG$GruendeNichtInteresse_kat))])
names(PreisLeistung)[1] <- "PreisLeistung"
summary(PreisLeistung)
PreisLeistungdf <- as.data.frame(summary(PreisLeistung))
# 12 mal genannt

Risiko <- as.data.frame(UG$F_4[which(grepl("Risiko", UG$GruendeNichtInteresse_kat))])
names(Risiko)[1] <- "Risiko"
summary(Risiko)
Risikodf <- as.data.frame(summary(Risiko))
# 7 mal genannt

Sonstiges <- as.data.frame(UG$F_4[which(grepl("Sonstiges", UG$GruendeNichtInteresse_kat))])
names(Sonstiges)[1] <- "Sonstiges"
summary(Sonstiges)
Sonstigesdf <- as.data.frame(summary(Sonstiges))
# 49 mal genannt

VerpflichtungOderBeteiligung<- as.data.frame(UG$F_4[which(grepl("Verpflichtung oder Beteiligung", UG$GruendeNichtInteresse_kat))])
names(VerpflichtungOderBeteiligung)[1] <- "VerpflichtungOderBeteiligung"
summary(VerpflichtungOderBeteiligung)
VerpflichtungOderBeteiligungdf <- as.data.frame(summary(VerpflichtungOderBeteiligung))
# 27 mal genannt

Zeit<- as.data.frame(UG$F_4[which(grepl("Zeit", UG$GruendeNichtInteresse_kat))])
names(Zeit)[1] <- "Zeit"
summary(Zeit)
Zeitdf <- as.data.frame(summary(Zeit))
# 41 mal genannt

dfcountGruende <- rbind(Abholungdf[1,], Abnahmezwangdf[1,], BindungbzwMitgliedschaftdf[1,], eigenerAnbauFehlenderBedarfdf[1,],Entfernungdf[1,], fehlendeFlexibilitaetdf[1,], fehlendeMengenauswahldf[1,], fehlendeProduktauswahldf[1,],  
                        fehlenderBedarfAnderedf[1,], Finanziellesdf[1,], keinInteressedf[1,], keineVerfuegbarkeitvorOrtdf[1,],KonzeptNichtAnsprechenddf[1,],  LebensumstaendeAlterdf[1,], LebensumstaendeGesundheitdf[1,], LebensumstaendeWohnsituationdf[1,],
                        LebensumstaendeAnderedf[1,], Nichtsdf[1,], PreisLeistungdf[1,], Risikodf[1,], Sonstigesdf[1,], VerpflichtungOderBeteiligungdf[1,], Zeitdf[1,])


dfcountGruende$Freq <- gsub("Length:", "", dfcountGruende$Freq)
dfcountGruendeNeu <- dfcountGruende[,!names(dfcountGruende) %in% "Var1"]
write.xlsx(dfcountGruendeNeu, file = "GruendeNichtInteresseNcount.xlsx")


GruendeNichtInteressedfneu$numbering <- 1:nrow(GruendeNichtInteressedfneu)
UG$numbering <- 1:nrow(UG)

GruendeNichtInteressedfneu_merge <- merge(x = GruendeNichtInteressedfneu, y = UG[ ,c("numbering", "F_4","GruendeNichtInteresse_kat")], by = "numbering", all.x=TRUE)
GruendeNichtInteressedfneu_merge <- GruendeNichtInteressedfneu_merge %>% select(numbering, F_4, V1, V2, V3, V4, V5, GruendeNichtInteresse_kat)
names(GruendeNichtInteressedfneu_merge) <- c("numbering", "Gruende_Original", "Kat_1", "Kat_2", "Kat_3", "Kat_4", "Kat_5", "Kat_zusammen")

write.xlsx(GruendeNichtInteressedfneu_merge, file = "GruendeNichtInteresseZuordnung_neu.xlsx")


