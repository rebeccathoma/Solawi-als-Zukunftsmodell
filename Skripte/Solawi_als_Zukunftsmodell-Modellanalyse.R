### Skript Modellauswertung zum Paper: Solidarische Landwirtschaft als Zukunftsmodell? ###
# Autorin: Rebecca Thoma
# Die Analyse erfolgte in Anlehnung an das Arbeitsbuch "Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R" von Hair Jr., Hult, Ringle, Sarstedt, Danks und Ray (2021)

library(openxlsx)
library(seminr)

#### Lade Datensatz ####
UG<-read.xlsx("Data/Umfrage_gesamt_mit_MS_1018.xlsx", na.strings=c("", "99"))
sapply(UG, class)

#### Datenaufbereitung ####
##### Umwandeln der Datentypen #####
cols.num <- c("Region","F_1", "F_2", "F_3","F_5"	,"F_6"	,"F_7_1"	,"F_7_2"	,"F_7_3"	,"F_7_4"	,"F_7_5"	,"F_7_6"	,"F_7_7"
              ,"F_7_8"	,"F_7_9"	,"F_7_10"	,"F_7_11"	,"F_7_12"	,"F_7_13"	,"F_7_14"	,"F_7_15"	,"F_7_16"	,"F_7_17"	,"F_7_18"	,"F_7_19"	,"F_7_20"	,"F_7_21"	,"F_7_22"	
              ,"F_7_23"	,"F_7_24"	,"F_7_25"	,"F_7_26"	,"F_7_27"	,"F_7_28"	,"F_7_29"	,"F_7_30"	,"F_7_31"	,"F_7_32"	,"F_7_33"	,"F_7_34"	,"F_7_35","F_8",	"F_9"
              ,"F_10_1"	,"F_11_1","F_12"	,"F_13_1"	,"F_14_1")
UG[cols.num] <- sapply(UG[cols.num],as.numeric)
sapply(UG, class)
head(UG)

##### Durchnummerierung der Fälle #####
UG$numbering <- 1:nrow(UG)

##### Umgang mit fehlenden Werten #####
dataPLS <- subset(UG, select=c(numbering, F_7_1:F_7_35))
dataPLS[dataPLS == 66] <- NA
nrow(na.omit(dataPLS))
# 259 Fälle, die keine fehlenden Werte enthalten

na_count <-sapply(dataPLS, function(y) sum(is.na(y)))
na_count

# problematische Items (mit mindestens 250 fehlenden Werten):
# F_7_15: Menschen, die mir wichtig sind, halten das Solawi-Konzept für nachhaltig
# F_7_16: Menschen, die mir wichtig sind, sind der Meinung, dass ich Solawi-Mitglied werden sollte
# F_7_31: Die Solawi würde mir ein gutes Preis-Leistungs-Verhältnis bieten

dataPLSohneSozialuPreis <- subset(dataPLS, select=c(numbering, F_7_1:F_7_14, F_7_19:F_7_30, F_7_32:F_7_35))

dataPLSohneSozialuPreisModeratoren <- subset(UG, select=c(numbering, F_7_1:F_7_14, F_7_19:F_7_30, F_7_32:F_7_35, F_8, F_9, F_12, F_11_1))
dataPLSohneSozialuPreisModeratoren[dataPLSohneSozialuPreisModeratoren == 66] <- NA

dataPLSohneSozialuPreiscleanOmitMod <- na.omit(dataPLSohneSozialuPreisModeratoren)

numberingDFOmitMod <- dataPLSohneSozialuPreiscleanOmitMod$numbering


##### Vorbereitung der Items für die PLS-Analyse #####
###### VERHALTENSABSICHT ######
# Ich bin grundsätzlich an einer Mitgliedschaft in einer Solawi interessiert (F_7_1)
# Ich habe konkrete Pläne, zukünftig Mitglied einer Solawi zu werden (F_7_2)
# Ich habe mich bereits über das Solawi-Konzept informiert (F_7_3)

names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_1"] <- "verhaltensabsicht_1"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_2"] <- "verhaltensabsicht_2"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_3"] <- "verhaltensabsicht_3"


###### LEISTUNGSERWARTUNG ######
# Die Mitgliedschaft in einer Solawi würde meinen Lebensstil positiv verändern. (F_7_4)
# Die Mitgliedschaft in einer Solawi würde mein Leben gesünder machen. (F_7_5)
# Durch die Mitgliedschaft in einer Solawi würde ich die Lebensmittelproduktion zum Besseren verändern. (F_7_6)
# Die Mitgliedschaft in einer Solawi würde die Lebensmittelproduktion für mich transparenter machen. (F_7_7)
# Durch die Mitgliedschaft in einer Solawi würde ich einen positiven Beitrag zur Artenvielfalt in meiner Region leisten. (F_7_8)
# Die Mitgliedschaft in einer Solawi wäre eine Gelegenheit, etwas Gutes für die Umwelt zu tun. (F_7_9)

names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_4"] <- "leistungserwartung_1"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_5"] <- "leistungserwartung_2"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_6"] <- "leistungserwartung_3"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_7"] <- "leistungserwartung_4"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_8"] <- "leistungserwartung_5"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_9"] <- "leistungserwartung_6"


###### AUFWANDSERWARTUNG ######
# Die Beschaffung von Lebensmitteln wäre zeitaufwändiger als bisher. (F_7_10)
# Die Beschaffung von Lebensmitteln wäre komplizierter als bisher. (F_7_11)
# Ich wäre in meinen Konsumgewohnheiten stärker eingeschränkt als bisher. (F_7_12)
# Ich würde das Engagement für die Gemeinschaft als Belastung empfinden. (F_7_13)
# Mich würde der langfristige Vertrag stören.(F_7_14)

names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_10"] <- "aufwandserwartung_1"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_11"] <- "aufwandserwartung_2"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_12"] <- "aufwandserwartung_3"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_13"] <- "aufwandserwartung_4"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_14"] <- "aufwandserwartung_5"


###### ERLEICHTERNDE BEDINGUNGEN ######
# Ich habe bereits vom Solawi-Konzept gehört. (F_7_19)
# Es ist mir wichtig, mich über die Herstellung meiner Lebensmittel zu informieren. (F_7_20)
# Ich interessiere mich für das Thema Landwirtschaft (F_7_21)
# Ich setze mich bewusst mit der Herkunft meiner Lebensmittel auseinander. (F_7_22)

names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_19"] <- "erleichterndeBedingungen_1"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_20"] <- "erleichterndeBedingungen_2"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_21"] <- "erleichterndeBedingungen_3"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_22"] <- "erleichterndeBedingungen_4"


###### HEDONISCHE MOTIVATION ######
# Ich möchte mir neues Wissen aneignen (z.B. zur Lebensmittelproduktion). (F_7_23)
# Ich würde mich gut fühlen, wenn ich einen landwirtschaftlichen Betrieb in der Region unterstütze. (F_7_24)
# Ich würde es als positiv empfinden, Mitglied einer Solawi zu sein. (F_7_25)
# Ich würde gerne mit Gleichgesinnten in Kontakt treten. (F_7_26)
# Ich würde gerne mit anderen Menschen auf einem Bauernhof arbeiten. (F_7_27)

names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_23"] <- "hedonischeMotivation_1"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_24"] <- "hedonischeMotivation_2"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_25"] <- "hedonischeMotivation_3"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_26"] <- "hedonischeMotivation_4"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_27"] <- "hedonischeMotivation_5"


###### PREISWERT ######
# Ich würde qualitativ hochwertigere Lebensmittel als zuvor erhalten. (F_7_28)
# Ich würde mich gesünder ernähren als bisher. (F_7_29)
# Ich würde frischere Lebensmittel als bisher erhalten. (F_7_30)
# Die Solawi würde mir ein gutes Preis-Leistungs-Verhältnis bieten. (F_7_31)

names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_28"] <- "Preiswert_1"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_29"] <- "Preiswert_2"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_30"] <- "Preiswert_3"


###### GEWOHNHEIT ######
# Ich kaufe oft Lebensmittel direkt vom Erzeuger/ von der Erzeugerin. (F_7_32)
# Ich bevorzuge es, regionale Produkte zu konsumieren. (F_7_33)
# Ich bevorzuge es, Bio-Produkte zu konsumieren. (F_7_34)
# Ich bereite oft frische Speisen zu. (F_7_35)

names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_32"] <- "Gewohnheit_1"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_33"] <- "Gewohnheit_2"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_34"] <- "Gewohnheit_3"
names(dataPLSohneSozialuPreiscleanOmitMod)[names(dataPLSohneSozialuPreiscleanOmitMod)=="F_7_35"] <- "Gewohnheit_4"


dataPLSohneSozialuPreiscleanOmitMod = as.data.frame(sapply(dataPLSohneSozialuPreiscleanOmitMod, as.numeric))



#### Partial Least Squares Structural Equation Modeling (PLS-SEM) (in Anlehnung an Hair Jr. et al. 2021) ####
##### Modell ohne Moderatoren #####
sapply(dataPLSohneSozialuPreiscleanOmitMod, function(y) sum(is.na(y)))

###### Erstellung Messmodell ######
modeltestPLSmm <- constructs(
  composite("verhaltensabsicht", multi_items("verhaltensabsicht_", 1:3)),
  composite("leistungserwartung", multi_items("leistungserwartung_", 1:6)),
  composite("aufwandserwartung", multi_items("aufwandserwartung_",1:5)),
  composite("erleichterndeBedingungen", multi_items("erleichterndeBedingungen_", 1:4)),
  composite("hedonischeMotivation", multi_items("hedonischeMotivation_",1:5)),
  composite("Preiswert", multi_items("Preiswert_", 1:3)),
  composite("Gewohnheit", multi_items("Gewohnheit_",1:4))
)

###### Erstellung Strukturmodell ######
modeltestPLSsm <- relationships(
  paths(from=c("leistungserwartung", "aufwandserwartung", "erleichterndeBedingungen", "hedonischeMotivation", "Preiswert", "Gewohnheit"), to=c("verhaltensabsicht")))

###### Modellschätzung ######
modeltestestimation <- estimate_pls(data=dataPLSohneSozialuPreiscleanOmitMod,
                                    measurement_model = modeltestPLSmm,
                                    structural_model = modeltestPLSsm,
                                    inner_weights = path_weighting,
                                    missing = mean_replacement,
                                    missing_value = NA)


###### Zusammenfassung Modellergebnisse ###### 
summary_modeltestestimation <- summary(modeltestestimation)

###### Bootstrapping des Modells ######
boot_modeltestestimation <-  bootstrap_model(seminr_model=modeltestestimation,
                                             nboot=10000, 
                                             cores=NULL,
                                             seed=123)

sum_boot_modeltestestimation <- summary(boot_modeltestestimation, alpha=0.05)

sum_boot_modeltestestimation$bootstrapped_paths

sum_boot_modeltestestimation$bootstrapped_loadings


###### Evaluierung des Messmodells ######

# Iterationen
summary_modeltestestimation$iterations
# 10 Iterationen wurden benötigt

####### Indikatorreliabilität - Ladungen #######
summary_modeltestestimation$loadings
# Problem mit erleichterndeBedingungen1 (0.543), verhaltensabsicht3 (0.688), Gewohnheit4 (0.683)
summary_modeltestestimation$loadings^2

####### Konstruktreliabilität (rhoa) und Konvergenzvalidität (DEV)#######
summary_modeltestestimation$reliability
plot(summary_modeltestestimation$reliability)

####### Diskriminanzvalidität (Fornell-Larcker-Kriterion; HTMT-Ratio; Kreuzladungen) #######
summary_modeltestestimation$validity$fl_criteria
summary_modeltestestimation$validity$htmt
summary_modeltestestimation$validity$cross_loadings

sum_boot_modeltestestimation_rep <- summary(boot_modeltestestimation, alpha = 0.10)
sum_boot_modeltestestimation_rep$bootstrapped_HTMT
# Probleme für:
# erleichterndeBedingungen - Gewohnheit 
# erleichterndeBedinungen - hedonischeMotivation 
# hedonischeMotivation - Verhaltensabsicht 


###### Evaluierung des Strukturmodells ######
####### Kollinearität (VIF) #######
summary_modeltestestimation$vif_antecedents

####### Relevanz und Signifikanz der Pfadkoeffizienten #######
sum_boot_modeltestestimation$bootstrapped_paths

####### Erklärkraft (R² und adj. R²) #######
summary_modeltestestimation$paths

####### Effektstärke (f²) #######
summary_modeltestestimation$fSquare


####### Vorhersagekraft #######
predict_modeltestestimation <- predict_pls(
  model= modeltestestimation,
  technique = predict_DA,
  noFolds = 10,
  reps = 10)

sum_predict_modeltestestimation <-  summary(predict_modeltestestimation)
sum_predict_modeltestestimation

######## Verteilung der Vorhersagefehler #######
par(mfrow=c(1,3))
plot(sum_predict_modeltestestimation,
     indicator = "verhaltensabsicht_1")
plot(sum_predict_modeltestestimation,
     indicator = "verhaltensabsicht_2")
plot(sum_predict_modeltestestimation,
     indicator = "verhaltensabsicht_3")
par(mfrow=c(1,1))

####### Plot des Modells #######
plot(boot_modeltestestimation, title="bootstrapped model")


##### Modell mit Moderatoren #####
# Moderatoren: Alter, Geschlecht, Nettoeinkommen, Bildungsabschluss 

###### Vorbereiten der Variablen ######
###### Geschlecht ######
# Dummy-Variable für Männlich = 1 und Weiblich oder Divers = 0 
table(dataPLSohneSozialuPreiscleanOmitMod$F_8)

dataPLSohneSozialuPreiscleanOmitMod$Geschlechtmaennl <- NA
dataPLSohneSozialuPreiscleanOmitMod$Geschlechtmaennl[which(dataPLSohneSozialuPreiscleanOmitMod$F_8==2)]<-1
dataPLSohneSozialuPreiscleanOmitMod$Geschlechtmaennl[which(dataPLSohneSozialuPreiscleanOmitMod$F_8==1 | dataPLSohneSozialuPreiscleanOmitMod$F_8==3)]<-0


####### Alter #######
# Dummy-Variable für unter 50 Jahre (0) und 50 Jahre und älter (1)
table(dataPLSohneSozialuPreiscleanOmitMod$F_9)

dataPLSohneSozialuPreiscleanOmitMod$ue50 <- NA
dataPLSohneSozialuPreiscleanOmitMod$ue50[which(dataPLSohneSozialuPreiscleanOmitMod$F_9==1|dataPLSohneSozialuPreiscleanOmitMod$F_9==2|dataPLSohneSozialuPreiscleanOmitMod$F_9==3|dataPLSohneSozialuPreiscleanOmitMod$F_9==4|dataPLSohneSozialuPreiscleanOmitMod$F_9==5|dataPLSohneSozialuPreiscleanOmitMod$F_9==6|dataPLSohneSozialuPreiscleanOmitMod$F_9==7)]<-0
dataPLSohneSozialuPreiscleanOmitMod$ue50[which(dataPLSohneSozialuPreiscleanOmitMod$F_9==8|dataPLSohneSozialuPreiscleanOmitMod$F_9==9|dataPLSohneSozialuPreiscleanOmitMod$F_9==10|dataPLSohneSozialuPreiscleanOmitMod$F_9==11|dataPLSohneSozialuPreiscleanOmitMod$F_9==12)]<-1

###### Haushaltsnettoeinkommen ######
# Einkommen unter dem Median (2.001 bis 3.000 €) (0) bzw. gleich oder ueber dem Median (1)
table(dataPLSohneSozialuPreiscleanOmitMod$F_12)

dataPLSohneSozialuPreiscleanOmitMod$EinkommenUE5 <- NA
dataPLSohneSozialuPreiscleanOmitMod$EinkommenUE5[which(dataPLSohneSozialuPreiscleanOmitMod$F_12==1|dataPLSohneSozialuPreiscleanOmitMod$F_12==2|dataPLSohneSozialuPreiscleanOmitMod$F_12==3|dataPLSohneSozialuPreiscleanOmitMod$F_12==4)]<-0
dataPLSohneSozialuPreiscleanOmitMod$EinkommenUE5[which(dataPLSohneSozialuPreiscleanOmitMod$F_12==5|dataPLSohneSozialuPreiscleanOmitMod$F_12==6|dataPLSohneSozialuPreiscleanOmitMod$F_12==7|dataPLSohneSozialuPreiscleanOmitMod$F_12==8)]<-1

class(dataPLSohneSozialuPreiscleanOmitMod$EinkommenUE5)


###### Bildungsabschluss #######
# Person hat Hochschulabschluss (1) vs. Personen hat keinen Hochschulabschluss (0)
table(dataPLSohneSozialuPreiscleanOmitMod$F_11_1)

dataPLSohneSozialuPreiscleanOmitMod$HSAbschluss <- NA
dataPLSohneSozialuPreiscleanOmitMod$HSAbschluss[which(dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="5"| dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="6" | dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="7")]<- "1"
dataPLSohneSozialuPreiscleanOmitMod$HSAbschluss[which(dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="1"| dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="2" | dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="3"|dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="4"|dataPLSohneSozialuPreiscleanOmitMod$F_11_1 =="88")]<- "0"

class(dataPLSohneSozialuPreiscleanOmitMod$HSAbschluss)
dataPLSohneSozialuPreiscleanOmitMod$HSAbschluss <- as.numeric(dataPLSohneSozialuPreiscleanOmitMod$HSAbschluss)


##### Zweistufiges Verfahren PLS-Analyse #####
###### Erstellung Messmodell ######
modeltestPLSmmMod1_2stage <- constructs(
  composite("verhaltensabsicht", multi_items("verhaltensabsicht_", 1:3)),
  composite("leistungserwartung", multi_items("leistungserwartung_", 1:6)),
  composite("aufwandserwartung", multi_items("aufwandserwartung_",1:5)),
  composite("erleichterndeBedingungen", multi_items("erleichterndeBedingungen_", 1:4)),
  composite("hedonischeMotivation", multi_items("hedonischeMotivation_",1:5)),
  composite("Preiswert", multi_items("Preiswert_", 1:3)),
  composite("Gewohnheit", multi_items("Gewohnheit_",1:4)),
  composite("Geschlecht", single_item("Geschlechtmaennl")),
  composite("Alter", single_item("ue50")),
  composite("Einkommen", single_item("EinkommenUE5")),
  composite("Abschluss", single_item("HSAbschluss")),
  interaction_term(iv="leistungserwartung", moderator="Geschlecht", method=two_stage),
  interaction_term(iv="aufwandserwartung", moderator="Geschlecht", method=two_stage),
  interaction_term(iv="erleichterndeBedingungen", moderator="Geschlecht", method=two_stage),
  interaction_term(iv="hedonischeMotivation", moderator="Geschlecht", method=two_stage),
  interaction_term(iv="Preiswert", moderator="Geschlecht", method=two_stage),
  interaction_term(iv="Gewohnheit", moderator="Geschlecht", method=two_stage),
  interaction_term(iv="leistungserwartung", moderator="Alter", method=two_stage),
  interaction_term(iv="aufwandserwartung", moderator="Alter", method=two_stage),
  interaction_term(iv="erleichterndeBedingungen", moderator="Alter", method=two_stage),
  interaction_term(iv="hedonischeMotivation", moderator="Alter", method=two_stage),
  interaction_term(iv="Preiswert", moderator="Alter", method=two_stage),
  interaction_term(iv="Gewohnheit", moderator="Alter", method=two_stage),
  interaction_term(iv="leistungserwartung", moderator="Einkommen", method=two_stage),
  interaction_term(iv="aufwandserwartung", moderator="Einkommen", method=two_stage),
  interaction_term(iv="erleichterndeBedingungen", moderator="Einkommen", method=two_stage),
  interaction_term(iv="hedonischeMotivation", moderator="Einkommen", method=two_stage),
  interaction_term(iv="Preiswert", moderator="Einkommen", method=two_stage),
  interaction_term(iv="Gewohnheit", moderator="Einkommen", method=two_stage),
  interaction_term(iv="leistungserwartung", moderator="Abschluss", method=two_stage),
  interaction_term(iv="aufwandserwartung", moderator="Abschluss", method=two_stage),
  interaction_term(iv="erleichterndeBedingungen", moderator="Abschluss", method=two_stage),
  interaction_term(iv="hedonischeMotivation", moderator="Abschluss", method=two_stage),
  interaction_term(iv="Preiswert", moderator="Abschluss", method=two_stage),
  interaction_term(iv="Gewohnheit", moderator="Abschluss", method=two_stage)
)


###### Erstellung Strukturmodell ######
modeltestPLSsmMod1 <- relationships(
  paths(from=c("leistungserwartung", "Geschlecht","Alter", "Einkommen", "Abschluss", "leistungserwartung*Geschlecht", "leistungserwartung*Alter","leistungserwartung*Einkommen", "leistungserwartung*Abschluss",
               "aufwandserwartung","aufwandserwartung*Geschlecht","aufwandserwartung*Alter","aufwandserwartung*Einkommen","aufwandserwartung*Abschluss",
               "erleichterndeBedingungen", "erleichterndeBedingungen*Geschlecht", "erleichterndeBedingungen*Alter","erleichterndeBedingungen*Einkommen","erleichterndeBedingungen*Abschluss",
               "hedonischeMotivation", "hedonischeMotivation*Geschlecht","hedonischeMotivation*Alter","hedonischeMotivation*Einkommen","hedonischeMotivation*Abschluss",
               "Preiswert",  "Preiswert*Geschlecht", "Preiswert*Alter","Preiswert*Einkommen","Preiswert*Abschluss",
               "Gewohnheit", "Gewohnheit*Geschlecht","Gewohnheit*Alter","Gewohnheit*Einkommen","Gewohnheit*Abschluss"), to=c("verhaltensabsicht")))


###### Modellschätzung ######
modeltestestimationMod1_2stage <- estimate_pls(data=dataPLSohneSozialuPreiscleanOmitMod,
                                               measurement_model = modeltestPLSmmMod1_2stage,
                                               structural_model = modeltestPLSsmMod1,
                                               inner_weights = path_weighting,
                                               missing = mean_replacement,
                                               missing_value = NA)

###### Zusammenfassung Modellergebnisse ######
summary_modeltestestimationMod1_2stage <-summary(modeltestestimationMod1_2stage)

###### Bootstrapping des Modells ######
boot_modeltestestimationMod1_2stage <- bootstrap_model(
  seminr_model = modeltestestimationMod1_2stage,
  nboot=10000,
  cores=NULL,
  seed=123)

sum_boot_modeltestestimationMod1_2stage <- summary(boot_modeltestestimationMod1_2stage, alpha=0.05)

sum_boot_modeltestestimationMod1_2stage$bootstrapped_paths
summary_modeltestestimationMod1_2stage$paths


###### Evaluatierung des Messmodells #######

# Iterationen
summary_modeltestestimationMod1_2stage$iterations
# 11 Iterationen wurden benötigt

###### Indikatorreliabilität - Ladungen ######
summary_modeltestestimationMod1_2stage$loadings
summary_modeltestestimationMod1_2stage$loadings^2

###### Konstruktreliabilität (rhoa) und Konvergenzvalidität (DEV) ######
summary_modeltestestimationMod1_2stage$reliability
plot(summary_modeltestestimationMod1_2stage$reliability)

####### Diskriminanzvalidität (Fornell-Larcker-Kriterion, HTMT-Ratio, Kreuzladungen) ######
summary_modeltestestimationMod1_2stage$validity$fl_criteria
summary_modeltestestimationMod1_2stage$validity$htmt
summary_modeltestestimationMod1_2stage$validity$cross_loadings

sum_boot_modeltestestimationMod1_2stage <- summary(boot_modeltestestimationMod1_2stage, alpha = 0.10)
sum_boot_modeltestestimationMod1_2stage$bootstrapped_HTMT

###### Evaluierung des Strukturmodells ######
####### Kollinearität (VIF) #######
sum_boot_modeltestestimationMod1_2stage$vif_antecedents

####### Relevanz und Signifikanz des Strukturmodells #######
sum_boot_modeltestestimationMod1_2stage$bootstrapped_paths

####### Erklärkraft (R² bzw adj. R²) #######
summary_modeltestestimationMod1_2stage$paths

###### Effektstärke (f²) ######
summary_modeltestestimationMod1_2stage$fSquare








