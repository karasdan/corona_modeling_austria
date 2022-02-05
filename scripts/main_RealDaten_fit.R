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


# # Goverment erstellen
# goverment <- data.frame(Tag = 1:tage,
#                         Lockdown = c(rep(FALSE,tage)))

# Lockdown von Tag 25 - 35
# goverment <- goverment %>%
#   mutate(Lockdown = if_else((Tag > 25 & Tag <= 50), TRUE, FALSE))

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

p <- ggplot(real_daten_Melk) +
  aes(x = Datum, y = AnzahlFaelle) +
  geom_line()

ggplotly(p)

p <- ggplot(real_daten_Melk) +
  aes(x = Datum, y = AnzahlFaelleSum) +
  geom_line()

ggplotly(p)

# Entscheidung -> versuchen an Realdaten anzupassen, indem wir den Zeitraum von 15.10.2020
# bis 16.11.2020 betrachten (Tag vor 2.Lockdown) (geltende Regeln: Maskenplicht, Lockdown
# light, Beschränkungen bei Events)

# Oder erster Test: nimm Zeit von 1.Lockdown 14.3.2020 - 27.3.2020

#----------------------------------------------------------------
# Erster Versuch 14.3.2020 - 27.3.2020

# Realdaten in bestimmten Zeitpunkt
real_daten_Melk_relevant <- real_daten_Melk %>%
  filter(Datum >= as.Date("2020-03-14") & Datum <= as.Date("2020-03-27"))

#TESTPLOT
# ggplot(real_daten_Melk_relevant) +
#   aes(x = Datum, y = AnzahlFaelleSum) +
#   geom_line()

# Test if exponetial growth
exp_growth <- real_daten_Melk_relevant %>%
  select(Datum, AnzahlFaelleSum) %>%
  mutate(ratio = AnzahlFaelleSum/lag(AnzahlFaelleSum)) %>%
  mutate(Tag = 1:n())

y_test <- exp_growth$AnzahlFaelleSum

t <- exp_growth$Tag

test <- nls(y_test ~ a * exp(r * t), start = list(a = 0.5, r = 0.2))

y_true <- coef(test)[1] * exp(coef(test)[2] * t)

ggplot(exp_growth) +
  aes(x = Tag, y = AnzahlFaelleSum) +
  geom_point() +
  geom_line(aes(x = t, y = y_true), color = "red")

# -> haben Exponetielles Wachstum in diesem Intervall!!!!!

# Goverment erstellen
goverment <- data.frame(Tag = 1:15,
                        Lockdown = c(rep(FALSE,15)))

# erster Versuch die Daten anzupassen 
# beginnen mit 5 infectioese Agents 
agents_for_realData <- gesundheit_erstellen(agents, 15)

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
                                                                      plotting = FALSE)
  
  temp <- fitting_test$TR
  
  Summe_ErkannteFaelle_DieserDurchgang <- 
    filter(temp, Tag == tag & type == "Summe_ErkannteFaelle")$Wert
  
  cat(paste0("Tag ", tag, ": ", Summe_ErkannteFaelle_DieserDurchgang))
  
  tag <- tag + 1
  
}

agents_for_fitting <- fitting_test$AG
zeiten_for_fitting <- fitting_test$ZE
tracker_from_fitting <- fitting_test$TR

save(agents_for_fitting, zeiten_for_fitting, tracker_from_fitting,
     file = "agents_initialisierung_ForFitting_FirstTry.RData")

first_try <- 
  monte_carlo_simulation_ForFitting(agents_ff = agents_for_fitting,
                                    tage_ff = 14,
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

ggplot() +
  aes(x = exp_growth$Datum[1:15]) +
  geom_line(aes(y = Wert), data = schnitt_fitting) +
  geom_point(aes( y = y_test[1:15])) +
  geom_line(aes(y = Wert), data = lower_fitting, color = "red") +
  geom_line(aes(y = Wert), data = upper_fitting, color = "red")


#----------------------------------------------------------------

load("agents_initialisierung_ForFitting_FirstTry.RData")

goverment <- data.frame(Tag = 1:15,
                        Lockdown = c(rep(FALSE,15)))


for (i in seq(0.05, 0.1, 0.01)) {
  
  temp <- monte_carlo_simulation_ForFitting(agents_ff = agents_for_fitting,
                                            tage_ff = 10,
                                            times_f = 5,
                                            type_of_contactfunctions_ff = type_of_contactfunctions,
                                            daten_wsk_ff = daten_wsk,
                                            goverment_ff = goverment,
                                            detection_wsk_per_agent_ff = detection_wsk_per_agent,
                                            quarantine = TRUE,
                                            zeiten_ff = zeiten_for_fitting,
                                            wsk_infection_hh_ff = 0.25,
                                            wsk_infection_other_ff = i
  )
  
  temp <- add_mean_and_ConfInt(temp) 
  
  prediction <- temp %>%
    filter(Durchlauf == "Schnitt" & Tag != 0 & type == "Summe_ErkannteFaelle") %>%
    slice(1:10)
  
  true_value <- real_daten_Melk_relevant %>%
    mutate(Tag = 0:(n() - 1), .after = "Datum") %>%
    filter(Tag > 0 & Tag < 11)
  
  vergleich <- true_value %>%
    inner_join(prediction, by = "Tag")
  
  vergleich <- vergleich %>%
    mutate(difference = Wert - AnzahlFaelleSum)
  
  print(paste0("Summe der Differenzen: ", sum(vergleich$difference)))
  
}

ggplot(vergleich) +
  aes(x = Tag) +
  geom_point(aes(y = AnzahlFaelleSum)) +
  geom_line(aes(y = Wert))
  



  monte_carlo_simulation_ForFitting(agents_ff = agents_for_fitting,
                                    tage_ff = 5,
                                    times_f = 5,
                                    type_of_contactfunctions_ff = type_of_contactfunctions,
                                    daten_wsk_ff = daten_wsk,
                                    goverment_ff = goverment,
                                    detection_wsk_per_agent_ff = detection_wsk_per_agent,
                                    quarantine = TRUE,
                                    zeiten_ff = zeiten_for_fitting,
                                    wsk_infection_hh_ff = 0.25,
                                    wsk_infection_other_ff = 0.05
                                    )





