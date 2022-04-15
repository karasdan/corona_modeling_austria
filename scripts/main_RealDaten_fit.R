library(tidyverse)
library(lubridate)
library(plotly)
library(data.table)

#----------------------------------------------------------------
#----------------------Laden_und_einstellen----------------------

# Voreinstellungen
district <- "Melk" # Welcher Bezirk?
district_number <- 315 # Welche Nummer

# Switch je nach Bezirk
switch(district,
       "Melk" = {file_agents <- "districtMelk_Niederösterreich_household_household_workplace_School_kindergarden_2022_02_01.RData"},
       "Gmünd" = {file_agents <- "districtGmünd_Niederösterreich_household_workplace_primarySchool_1infected_2022_01_09.RData"
       file_wsk <- "districtGmünd_0.05.RData"})

# Agentdateneinlesen -> Initialisierung
setwd("./agents_initialisierung")
load(file_agents)
setwd("..")

# Umbenennen, da alles so geschrieben ist
daten_wsk <- wsk_between_centre
rm(wsk_between_centre)

# Functions laden
setwd("./scripts")
source("function_RealDaten_fit.R")
setwd("..")

# Erkennungswsk erstellen
detection_wsk_per_agent <- test_detection_wsk(agents_f = agents)

#----------------------------------------------------------------
#-----------------------Realdaten einlesen-----------------------

setwd("./data")
real_daten <- read_csv2("CovidFaelle_Timeline_GKZ.csv")
setwd("..")

real_daten_Melk <- real_daten %>%
  filter(GKZ == district_number) %>%
  mutate(Time = gsub(" 00:00:00", "", x = Time)) %>%
  mutate(Datum = as.Date(Time, format =  "%d.%m.%Y"), .before = Time) 

ggplot(real_daten_Melk) +
  aes(x = Datum, y = AnzahlFaelle) +
  geom_line()

ggplot(real_daten_Melk) +
  aes(x = Datum, y = AnzahlFaelleSum) +
  geom_line()


# Entscheidung -> versuchen an Realdaten anzupassen, indem wir den Zeitraum von 15.10.2020
# bis 16.11.2020 betrachten (Tag vor 2.Lockdown) (geltende Regeln: Maskenplicht, Lockdown
# light, Beschränkungen bei Events)

# Oder erster Test: nimm Zeit von 1.Lockdown 14.3.2020 - 27.3.2020

#----------------------------------------------------------------
# Erster Versuch 14.3.2020 - 27.3.2020

# Realdaten in bestimmten Zeitpunkt
real_daten_Melk_relevant <- real_daten_Melk %>%
  filter(Datum >= as.Date("2020-03-14") & Datum <= as.Date("2020-03-27")) %>%
  mutate(Tag = 1:n())

ggplot(real_daten_Melk_relevant) +
  aes(x = Datum, y = AnzahlFaelleSum) +
  geom_line()

# TESTCODE
# exp_growth <- real_daten_Melk_relevant %>%
#   select(Datum, AnzahlFaelleSum) %>%
#   mutate(ratio = AnzahlFaelleSum/lag(AnzahlFaelleSum)) %>%
#   mutate(Tag = 1:n())

realDaten_SummeDerFaelle <- real_daten_Melk_relevant$AnzahlFaelleSum

Verlauf_der_betrachteten_Tage <- real_daten_Melk_relevant$Tag

exponetial_fit_for_RealDaten <- 
  nls(realDaten_SummeDerFaelle ~ a0 * exp(r * Verlauf_der_betrachteten_Tage), start = list(a0 = 0.5, r = 0.2))

y_predict <- 
  coef(exponetial_fit_for_RealDaten)[1] * exp(coef(exponetial_fit_for_RealDaten)[2] * Verlauf_der_betrachteten_Tage)

ggplot(real_daten_Melk_relevant) +
  aes(x = Tag, y = AnzahlFaelleSum) +
  geom_point() +
  geom_line(aes(x = Verlauf_der_betrachteten_Tage, y = y_predict), color = "red")

# -> haben Exponetielles Wachstum in diesem Intervall!!!!!
#----------------------------------------------------------------

# # Goverment erstellen
# goverment <- data.frame(Tag = 1:15,
#                         Lockdown = c(rep(FALSE,15)))

tage <- 15
# Goverment erstellen
goverment <- data.frame(Tag = 1:tage,
                        Lockdown = c(rep(FALSE,tage)),
                        leisure_contact_reduction = c(rep(FALSE,tage)),
                        home_office = c(rep(FALSE,tage)),
                        primary_school_closed = c(rep(FALSE,tage)),
                        kindergarden_closed = c(rep(FALSE,tage)),
                        school_closed = c(rep(FALSE,tage)))

# erster Versuch die Daten anzupassen 
# beginnen mit 20 infectioese Agents 

doubling_time_vergleich <- tibble(Anzahl_Agents = numeric(),
                                  Summe_doublingTime_Differences = numeric())
historical_doublingTime <- Inf

for (i in 20:50){
  
  # Goverment erstellen
  goverment <- data.frame(Tag = 1:tage,
                          Lockdown = c(rep(FALSE,tage)),
                          leisure_contact_reduction = c(rep(FALSE,tage)),
                          home_office = c(rep(FALSE,tage)),
                          primary_school_closed = c(rep(FALSE,tage)),
                          kindergarden_closed = c(rep(FALSE,tage)),
                          school_closed = c(rep(FALSE,tage)))
  
  Summe_ErkannteFaelle_DieserDurchgang <- 5
  
  while(Summe_ErkannteFaelle_DieserDurchgang > 3) {
    
    agents_for_realData <- gesundheit_erstellen(agents, i)
    
    Summe_ErkannteFaelle_DieserDurchgang <- 0
    tag <- 1
    
    # Kontaktzusammensetzung
    type_of_contactfunctions <- c("freizeit",  
                                  "haushalt", 
                                  "arbeitsplatz", 
                                  "volksschule", 
                                  "schule", 
                                  "kindergarten")
    
    while(Summe_ErkannteFaelle_DieserDurchgang < 1) {
      
      fitting_test <- corona_model_agent_based_with_quarantine_ForFitting(agents_f = agents_for_realData,
                                                                          tage_f = tag,
                                                                          type_of_contactfunctions_f = type_of_contactfunctions,
                                                                          daten_wsk_f = daten_wsk,
                                                                          goverment_f = goverment,
                                                                          detection_wsk_per_agent_f = detection_wsk_per_agent,
                                                                          plotting = FALSE,
                                                                          division_kontakte_ff = 0.125)
      
      temp <- fitting_test$TR
      
      Summe_ErkannteFaelle_DieserDurchgang <- 
        filter(temp, Tag == tag & type == "Summe_ErkannteFaelle")$Wert
      
      cat(paste0("Tag ", tag, ": ", Summe_ErkannteFaelle_DieserDurchgang))
      
      tag <- tag + 1
      
      }
  }
  
  agents_for_fitting <- fitting_test$AG
  zeiten_for_fitting <- fitting_test$ZE
  tracker_from_fitting <- fitting_test$TR
  
  goverment <- data.frame(Tag = 1:tage,
                          Lockdown = c(FALSE, FALSE, rep(TRUE,tage - 2)),
                          leisure_contact_reduction = c(rep(FALSE,tage)),
                          home_office = c(rep(FALSE,tage)),
                          primary_school_closed = c(rep(FALSE,tage)),
                          kindergarden_closed = c(rep(FALSE,tage)),
                          school_closed = c(rep(FALSE,tage)))
  
  first_try <- 
    monte_carlo_simulation_ForFitting(agents_ff = agents_for_fitting,
                                      tage_ff = 9,
                                      times_f = 5,
                                      type_of_contactfunctions_ff = type_of_contactfunctions,
                                      daten_wsk_ff = daten_wsk,
                                      goverment_ff = goverment,
                                      detection_wsk_per_agent_ff = detection_wsk_per_agent,
                                      quarantine = TRUE,
                                      zeiten_ff = zeiten_for_fitting)
  
  first_try_schnitt <- add_mean_and_ConfInt(first_try)
  
  schnitt_fitting <- first_try_schnitt %>%
    filter(Durchlauf %in% c("Schnitt") & type == "Summe_ErkannteFaelle")
  
  lower_fitting <- first_try_schnitt %>%
    filter(Durchlauf %in% c("lower") & type == "Summe_ErkannteFaelle")
  
  upper_fitting <- first_try_schnitt %>%
    filter(Durchlauf %in% c("upper") & type == "Summe_ErkannteFaelle")
  
  p <- ggplot() +
    aes(x = real_daten_Melk_relevant$Datum[1:10]) +
    geom_line(aes(y = Wert), data = schnitt_fitting) +
    geom_point(aes( y = y_predict[1:10])) +
    geom_line(aes(y = Wert), data = lower_fitting, color = "red") +
    geom_line(aes(y = Wert), data = upper_fitting, color = "red") +
    ggtitle(i)
  
  print(p)
  
  # Test der Verdoppelungsrate 
  
  temp_realDaten <- real_daten_Melk_relevant %>%
    filter(Tag <= 10) %>%
    select(Tag, AnzahlFaelleSum)
  
  temp_predictDaten <- schnitt_fitting %>%
    mutate(Tag = Tag + 1) %>%
    select(Tag, Wert)
  
  actual_doublingTime <- 
    test_doubling_time(Real_Daten_selected_f = temp_realDaten, Predict_Daten_seleceted_f = temp_predictDaten,
                       abTag_f = 4, bisTag_f = 10)
  
  if (actual_doublingTime < historical_doublingTime){
    
    historical_doublingTime <- actual_doublingTime
    
    best_agents <- agents_for_fitting
    best_time <- zeiten_for_fitting
    best_track <- tracker_from_fitting
    best_schnitt <- first_try_schnitt
    
  }
  
  doubling_time_vergleich <- doubling_time_vergleich %>%
    add_row(Anzahl_Agents = i, Summe_doublingTime_Differences = actual_doublingTime)
  
}

schnitt_fitting <- best_schnitt %>%
  filter(Durchlauf %in% c("Schnitt") & type == "Summe_ErkannteFaelle")

lower_fitting <- best_schnitt %>%
  filter(Durchlauf %in% c("lower") & type == "Summe_ErkannteFaelle")

upper_fitting <- best_schnitt %>%
  filter(Durchlauf %in% c("upper") & type == "Summe_ErkannteFaelle")

p <- ggplot() +
  aes(x = real_daten_Melk_relevant$Datum[1:10]) +
  geom_line(aes(y = Wert), data = schnitt_fitting) +
  geom_point(aes( y = y_predict[1:10])) +
  geom_line(aes(y = Wert), data = lower_fitting, color = "red") +
  geom_line(aes(y = Wert), data = upper_fitting, color = "red") +
  ggtitle("Bestergebnis")

print(p)

# save(best_agents, best_schnitt, best_time, best_track,
#      file = "agents_initialisierung_ForFitting.RData")

#----------------------------------------------------------------

#load("agents_initialisierung_ForFitting_29Agents_2022_04_12.RData")

tage <- 15
# Goverment erstellen
goverment <- data.frame(Tag = 1:tage,
                        Lockdown = c(rep(FALSE,tage)),
                        leisure_contact_reduction = (rep(FALSE,tage)),
                        home_office = c(rep(FALSE,tage)),
                        primary_school_closed = c(rep(FALSE,tage)),
                        kindergarden_closed = c(rep(FALSE,tage)),
                        school_closed = c(rep(FALSE,tage)))

# Kontaktzusammensetzung
type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz", 
                              "volksschule", 
                              "schule", 
                              "kindergarten")

vergleich_doublingTime <- tibble(prozent = numeric(),
                                 doubling_time = numeric())

for (i in seq(0.05, 0.1, 0.01)) {
  
  temp <- monte_carlo_simulation_ForFitting(agents_ff = best_agents,
                                            tage_ff = 10,
                                            times_f = 15,
                                            type_of_contactfunctions_ff = type_of_contactfunctions,
                                            daten_wsk_ff = daten_wsk,
                                            goverment_ff = goverment,
                                            detection_wsk_per_agent_ff = detection_wsk_per_agent,
                                            quarantine = TRUE,
                                            zeiten_ff = best_time,
                                            wsk_infection_hh_ff = 0.25,
                                            wsk_infection_other_ff = i
  )
  
  temp <- add_mean_and_ConfInt(temp) 
  
  temp_realDaten <- real_daten_Melk_relevant %>%
    filter(Tag <= 11) %>%
    select(Tag, AnzahlFaelleSum)
  
  temp_predictDaten <- temp %>%
    mutate(Tag = Tag + 1) %>%
    filter(Durchlauf == "Schnitt" & type == "Summe_ErkannteFaelle") %>%
    select(Tag, Wert)
  
  actual_doublingTime <- 
    test_doubling_time(Real_Daten_selected_f = temp_realDaten, Predict_Daten_seleceted_f = temp_predictDaten,
                       abTag_f = 4, bisTag_f = 11)
  
  vergleich <- temp_realDaten %>%
    inner_join(temp_predictDaten, by = "Tag")

  p <- ggplot(vergleich) +
    aes(x = Tag) +
    geom_point(aes(y = AnzahlFaelleSum)) +
    geom_line(aes(y = Wert)) +
    ggtitle(i)
  
  print(paste0("Summe der quadratischen Unterschiede der Verdopplungszeit: ", actual_doublingTime, " Prozent: ", i))
  print(p)
  
  vergleich_doublingTime <- vergleich_doublingTime %>%
    add_row(prozent = i, doubling_time = actual_doublingTime)
  
}


#---------------------------------------------------------------- 

tage <- 30
# Goverment erstellen
goverment <- data.frame(Tag = 1:tage,
                        Lockdown = c(rep(FALSE,tage)),
                        leisure_contact_reduction = (rep(FALSE,tage)),
                        home_office = c(rep(FALSE,tage)),
                        primary_school_closed = c(rep(FALSE,tage)),
                        kindergarden_closed = c(rep(FALSE,tage)),
                        school_closed = c(rep(FALSE,tage)))

prognose_ohne_lockdown <- 
  monte_carlo_simulation_ForFitting(agents_ff = best_agents,
                                    tage_ff = 30,
                                    times_f = 15,
                                    type_of_contactfunctions_ff = type_of_contactfunctions,
                                    daten_wsk_ff = daten_wsk,
                                    goverment_ff = goverment,
                                    detection_wsk_per_agent_ff = detection_wsk_per_agent,
                                    quarantine = TRUE,
                                    zeiten_ff = best_time,
                                    wsk_infection_hh_ff = 0.25,
                                    wsk_infection_other_ff = 0.06
                                    )

prognose_ohne_lockdown <- add_mean_and_ConfInt(prognose_ohne_lockdown)

goverment <- data.frame(Tag = 1:tage,
                        Lockdown = c(FALSE, FALSE, rep(TRUE,tage - 2)),
                        leisure_contact_reduction = (rep(FALSE,tage)),
                        home_office = c(rep(FALSE,tage)),
                        primary_school_closed = c(rep(FALSE,tage)),
                        kindergarden_closed = c(rep(FALSE,tage)),
                        school_closed = c(rep(FALSE,tage)))

prognose_mit_lockdown <- 
  monte_carlo_simulation_ForFitting(agents_ff = best_agents,
                                    tage_ff = 30,
                                    times_f = 15,
                                    type_of_contactfunctions_ff = type_of_contactfunctions,
                                    daten_wsk_ff = daten_wsk,
                                    goverment_ff = goverment,
                                    detection_wsk_per_agent_ff = detection_wsk_per_agent,
                                    quarantine = TRUE,
                                    zeiten_ff = best_time,
                                    wsk_infection_hh_ff = 0.25,
                                    wsk_infection_other_ff = 0.06,
                                    division_kontakte_fff = 0.125
  )

prognose_mit_lockdown <- add_mean_and_ConfInt(prognose_mit_lockdown)

real_daten_Melk_test <- real_daten_Melk %>%
  filter(Datum >= as.Date("2020-03-14") & Datum <= as.Date("2020-03-27"))
  

schnitt <- prognose_ohne_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("Schnitt") & Tag < 14) 

upper <- prognose_ohne_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("upper") & Tag < 14) 

lower <- prognose_ohne_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("lower") & Tag < 14) 

ggplot() +
  aes(x = real_daten_Melk_test$Datum) +
  geom_line(aes(y = schnitt$Wert)) +
  geom_line(aes(y = lower$Wert), color = "red") +
  geom_line(aes(y = upper$Wert), color = "red") +
  geom_point(aes(y = real_daten_Melk_test$AnzahlFaelleSum))
  #facet_wrap(~type)

real_daten_Melk_test <- real_daten_Melk %>%
  filter(Datum >= as.Date("2020-03-14") & Datum <= as.Date("2020-04-01"))


schnitt <- prognose_ohne_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("Schnitt") & Tag < 19) 

upper <- prognose_ohne_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("upper") & Tag < 19) 

lower <- prognose_ohne_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("lower") & Tag < 19) 

ggplot() +
  aes(x = real_daten_Melk_test$Datum) +
  geom_line(aes(y = schnitt$Wert)) +
  geom_line(aes(y = lower$Wert), color = "red") +
  geom_line(aes(y = upper$Wert), color = "red") +
  geom_point(aes(y = real_daten_Melk_test$AnzahlFaelleSum))
  #facet_wrap(~type)

real_daten_Melk_test <- real_daten_Melk %>%
  filter(Datum >= as.Date("2020-03-14") & Datum <= as.Date("2020-03-27"))


schnitt <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("Schnitt") & Tag < 14) 

upper <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("upper") & Tag < 14) 

lower <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("lower") & Tag < 14) 

ggplot() +
  aes(x = real_daten_Melk_test$Datum) +
  geom_line(aes(y = schnitt$Wert)) +
  geom_line(aes(y = lower$Wert), color = "red") +
  geom_line(aes(y = upper$Wert), color = "red") +
  geom_point(aes(y = real_daten_Melk_test$AnzahlFaelleSum))
#facet_wrap(~type)

real_daten_Melk_test <- real_daten_Melk %>%
  filter(Datum >= as.Date("2020-03-14") & Datum <= as.Date("2020-04-01"))


schnitt <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("Schnitt") & Tag < 19) 

upper <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("upper") & Tag < 19) 

lower <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("lower") & Tag < 19) 

ggplot() +
  aes(x = real_daten_Melk_test$Datum) +
  geom_line(aes(y = schnitt$Wert)) +
  geom_line(aes(y = lower$Wert), color = "red") +
  geom_line(aes(y = upper$Wert), color = "red") +
  geom_point(aes(y = real_daten_Melk_test$AnzahlFaelleSum))

real_daten_Melk_test <- real_daten_Melk %>%
  filter(Datum >= as.Date("2020-03-14") & Datum <= as.Date("2020-04-13"))


schnitt <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("Schnitt") & Tag < 31) 

upper <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("upper") & Tag < 31) 

lower <- prognose_mit_lockdown %>%
  filter(type == "Summe_ErkannteFaelle" & Durchlauf %in% c("lower") & Tag < 31) 

ggplot() +
  aes(x = real_daten_Melk_test$Datum) +
  geom_line(aes(y = schnitt$Wert)) +
  geom_line(aes(y = lower$Wert), color = "red") +
  geom_line(aes(y = upper$Wert), color = "red") +
  geom_point(aes(y = real_daten_Melk_test$AnzahlFaelleSum))

#save(prognose_ohne_lockdown, file = "prognose_ohne_lockdown_2022_04_15.RData")
#save(prognose_mit_lockdown, file = "prognose_mit_lockdown_2022_04_15.RData")
