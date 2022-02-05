library(tidyverse)
library(microbenchmark)
library(data.table)

#----------------------Laden und einstellen----------------------

# Voreinstellungen
district <- "Melk" # Welcher Bezirk?
district_number <- 315 # Welche Nummer
tage <- 5 # Wie viele Tage?
times <- 5 # Wie viele WH bei Monte-Carlo-Simulation?

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
source("function_modellierung.R")
setwd("..")

# Gesundheitsstatus der Agents waehlen
agents <- gesundheit_erstellen(agents, 100)

# agents for without quarantine
agents_without_quarantine <- agents %>%
  mutate(infected = if_else(infectious == TRUE, TRUE, infected))

# Goverment erstellen
goverment <- data.frame(Tag = 1:tage,
                        Lockdown = c(rep(FALSE,tage)))

# Lockdown von Tag 25 - 35
# goverment <- goverment %>%
#   mutate(Lockdown = if_else((Tag > 25 & Tag <= 50), TRUE, FALSE))

# Erkennungswsk erstellen
detection_wsk_per_agent <- test_detection_wsk(agents_f = agents)

#--------Modell mit allen Kontakten (with and without Q)---------

# Kontaktzusammensetzung
type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz", 
                              "volksschule", 
                              "schule", 
                              "kindergarten")

# Model without Q
Model_All_WithoutQ <- 
  corona_model_agent_based(agents_f = agents_without_quarantine,
                           tage_f = tage,
                           type_of_contactfunctions_f = type_of_contactfunctions,
                           daten_wsk_f = daten_wsk,
                           goverment_f = goverment,
                           plotting = TRUE)

# Model with Q
Model_All_WithQ <- 
  corona_model_agent_based_with_quarantine(agents_f = agents,
                                           tage_f = tage,
                                           type_of_contactfunctions_f = type_of_contactfunctions,
                                           daten_wsk_f = daten_wsk,
                                           goverment_f = goverment,
                                           detection_wsk_per_agent_f = detection_wsk_per_agent,
                                           plotting = TRUE)


#---Modell ohne Kontakte im Kindergarten (with and without Q)----

type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz", 
                              "volksschule", 
                              "schule") 
                              #"kindergarten")

# Model without Q
Model_LeisureHouseholdWorkSchool_WithoutQ <- 
  corona_model_agent_based(agents_f = agents_without_quarantine,
                           tage_f = tage,
                           type_of_contactfunctions_f = type_of_contactfunctions,
                           daten_wsk_f = daten_wsk,
                           goverment_f = goverment,
                           plotting = TRUE)

# Model with Q
Model_LeisureHouseholdWorkSchool_WithQ <-
  corona_model_agent_based_with_quarantine(agents_f = agents,
                                           tage_f = tage,
                                           type_of_contactfunctions_f = type_of_contactfunctions,
                                           daten_wsk_f = daten_wsk,
                                           goverment_f = goverment,
                                           detection_wsk_per_agent_f = detection_wsk_per_agent,
                                           plotting = TRUE)

#----Modell ohne Kindergarten und Schule (with and without Q)----

type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz")
                              #"volksschule", 
                              #"schule", 
                              #"kindergarten")

# Model without Q
Model_LeisureHouseholdWork_WithoutQ <- 
  corona_model_agent_based(agents_f = agents_without_quarantine,
                           tage_f = tage,
                           type_of_contactfunctions_f = type_of_contactfunctions,
                           daten_wsk_f = daten_wsk,
                           goverment_f = goverment,
                           plotting = TRUE)

# Model with Q
Model_LeisureHouseholdWork_WithQ <-
  corona_model_agent_based_with_quarantine(agents_f = agents,
                                           tage_f = tage,
                                           type_of_contactfunctions_f = type_of_contactfunctions,
                                           daten_wsk_f = daten_wsk,
                                           goverment_f = goverment,
                                           detection_wsk_per_agent_f = detection_wsk_per_agent,
                                           plotting = TRUE)

#------Modell mit Freizeit und Haushalt (with and without Q)-----

type_of_contactfunctions <- c("freizeit",  
                              "haushalt")

# Model without Q
Model_LeisureHousehold_WithoutQ <- 
  corona_model_agent_based(agents_f = agents_without_quarantine,
                           tage_f = tage,
                           type_of_contactfunctions_f = type_of_contactfunctions,
                           daten_wsk_f = daten_wsk,
                           goverment_f = goverment,
                           plotting = TRUE)

# Model with Q
Model_LeisureHousehold_WithQ <-
  corona_model_agent_based_with_quarantine(agents_f = agents,
                                           tage_f = tage,
                                           type_of_contactfunctions_f = type_of_contactfunctions,
                                           daten_wsk_f = daten_wsk,
                                           goverment_f = goverment,
                                           detection_wsk_per_agent_f = detection_wsk_per_agent,
                                           plotting = TRUE)

#-------------Modell mit Freizeit (with and without Q)-----------

type_of_contactfunctions <- c("freizeit")

# Model without Q
Model_Leisure_WithoutQ <- 
  corona_model_agent_based(agents_f = agents_without_quarantine,
                           tage_f = tage,
                           type_of_contactfunctions_f = type_of_contactfunctions,
                           daten_wsk_f = daten_wsk,
                           goverment_f = goverment,
                           plotting = TRUE)

# Model with Q
Model_Leisure_WithQ <-
  corona_model_agent_based_with_quarantine(agents_f = agents,
                                           tage_f = tage,
                                           type_of_contactfunctions_f = type_of_contactfunctions,
                                           daten_wsk_f = daten_wsk,
                                           goverment_f = goverment,
                                           detection_wsk_per_agent_f = detection_wsk_per_agent,
                                           plotting = TRUE)

#-----------------------Plots alle Modelle-----------------------

# Vergleich 5 Modelle mit Quarantaene 
plot_model_results_together(quarantine = TRUE, 
                            agent_status = c("I_", "Q", "Neuinfektionen"))

plot_model_results_together(quarantine = TRUE, 
                            agent_status = c("Neuinfektionen"))

# Vergleiche 5 Modelle ohne Quarantaene
plot_model_results_together(quarantine = FALSE, 
                            agent_status = c("Neuinfektionen", "I"))

plot_model_results_together(quarantine = FALSE,
                            agent_status = "R")

#-----------Monte Carlo alle Kontakte (mit Quarantaene)----------

tage <- 5 # Wie viele Tage?
times <- 5

# Kontaktzusammensetzung
type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz", 
                              "volksschule", 
                              "schule", 
                              "kindergarten")


# MonteCarlo ohne Quarantaene
MonteCarlo_All_WithQ <- 
  monte_carlo_simulation(agents_ff = agents,
                         tage_ff = tage,
                         type_of_contactfunctions_ff = type_of_contactfunctions,
                         daten_wsk_ff = daten_wsk,
                         goverment_ff = goverment,
                         times_f = times,
                         detection_wsk_per_agent_ff = detection_wsk_per_agent,
                         quarantine = TRUE)

MonteCarlo_All_WithQ <- add_mean_and_ConfInt(MonteCarlo_All_WithQ)

plot_result_MonteCarlo(MonteCarlo_All_WithQ, agent_status = c("Neuinfektionen","I"))
plot_result_MonteCarlo(MonteCarlo_All_WithQ, agent_status = c("I_", "I", "Q"))

#--------MonteCarlo ohne Kontakte im Kindergarten (with Q)-------

type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz", 
                              "volksschule", 
                              "schule") 
                              #"kindergarten")

MonteCarlo_LeisureHouseholdWorkSchool_WithQ <- 
  monte_carlo_simulation(agents_ff = agents,
                         tage_ff = tage,
                         type_of_contactfunctions_ff = type_of_contactfunctions,
                         daten_wsk_ff = daten_wsk,
                         goverment_ff = goverment,
                         times_f = times,
                         detection_wsk_per_agent_ff = detection_wsk_per_agent,
                         quarantine = TRUE)

MonteCarlo_LeisureHouseholdWorkSchool_WithQ <- 
  add_mean_and_ConfInt(MonteCarlo_LeisureHouseholdWorkSchool_WithQ)

plot_result_MonteCarlo(MonteCarlo_LeisureHouseholdWorkSchool_WithQ, 
                       agent_status = c("Neuinfektionen","I"))
plot_result_MonteCarlo(MonteCarlo_LeisureHouseholdWorkSchool_WithQ, 
                       agent_status = c("I_", "I", "Q"))


#---------MonteCarlo ohne Kindergarten und Schule (with Q)---------

type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz")
                              #"volksschule", 
                              #"schule", 
                              #"kindergarten")

MonteCarlo_LeisureHouseholdWork_WithQ <- 
  monte_carlo_simulation(agents_ff = agents,
                         tage_ff = tage,
                         type_of_contactfunctions_ff = type_of_contactfunctions,
                         daten_wsk_ff = daten_wsk,
                         goverment_ff = goverment,
                         times_f = times,
                         detection_wsk_per_agent_ff = detection_wsk_per_agent,
                         quarantine = TRUE)

MonteCarlo_LeisureHouseholdWork_WithQ <- 
  add_mean_and_ConfInt(MonteCarlo_LeisureHouseholdWork_WithQ)

plot_result_MonteCarlo(MonteCarlo_LeisureHouseholdWork_WithQ, 
                       agent_status = c("Neuinfektionen","I"))
plot_result_MonteCarlo(MonteCarlo_LeisureHouseholdWork_WithQ, 
                       agent_status = c("I_", "I", "Q"))

#---------MonteCarlo mit Freizeit und Haushalt (with Q)----------

type_of_contactfunctions <- c("freizeit",  
                              "haushalt")

MonteCarlo_LeisureHousehold_WithQ <- 
  monte_carlo_simulation(agents_ff = agents,
                         tage_ff = tage,
                         type_of_contactfunctions_ff = type_of_contactfunctions,
                         daten_wsk_ff = daten_wsk,
                         goverment_ff = goverment,
                         times_f = times,
                         detection_wsk_per_agent_ff = detection_wsk_per_agent,
                         quarantine = TRUE)

MonteCarlo_LeisureHousehold_WithQ <- 
  add_mean_and_ConfInt(MonteCarlo_LeisureHousehold_WithQ)

plot_result_MonteCarlo(MonteCarlo_LeisureHousehold_WithQ, 
                       agent_status = c("Neuinfektionen","I"))
plot_result_MonteCarlo(MonteCarlo_LeisureHousehold_WithQ, 
                       agent_status = c("I_", "I", "Q"))

#----------------MonteCarlo mit Freizeit (with Q)----------------

type_of_contactfunctions <- c("freizeit")

MonteCarlo_Leisure_WithQ <- 
  monte_carlo_simulation(agents_ff = agents,
                         tage_ff = tage,
                         type_of_contactfunctions_ff = type_of_contactfunctions,
                         daten_wsk_ff = daten_wsk,
                         goverment_ff = goverment,
                         times_f = times,
                         detection_wsk_per_agent_ff = detection_wsk_per_agent,
                         quarantine = TRUE)

MonteCarlo_Leisure_WithQ <- 
  add_mean_and_ConfInt(MonteCarlo_Leisure_WithQ)

plot_result_MonteCarlo(MonteCarlo_Leisure_WithQ, 
                       agent_status = c("Neuinfektionen","I"))
plot_result_MonteCarlo(MonteCarlo_Leisure_WithQ, 
                       agent_status = c("I_", "I", "Q"))

#--------------Plots alle Monte-Carlo Simulationen---------------

plot_MonteCarlo_results_together(quarantine = TRUE,
                                 agent_status = c("I_", "Neuinfektionen"),
                                 plot_inhalt = c("Schnitt"))

plot_MonteCarlo_results_together(quarantine = TRUE,
                                 agent_status = c("R", "Q", "I"),
                                 plot_inhalt = c("Schnitt"))

plot_MonteCarlo_results_together(quarantine = TRUE,
                                 agent_status = c("S"),
                                 plot_inhalt = c("Schnitt"))

#------------------------Test Monte-Carlo------------------------
# Kontaktzusammensetzung
type_of_contactfunctions <- c("freizeit",  
                              "haushalt", 
                              "arbeitsplatz", 
                              "volksschule", 
                              "schule", 
                              "kindergarten")
sequenz <- seq(5,20,5)

test_monte_carlo <- 
  test_MonteCarlo_Iteration(sequenz_f = sequenz,
                            agents_f = agents,
                            type_of_contactfunctions_f = type_of_contactfunctions,
                            goverment_f = goverment,
                            tage_f = tage,
                            daten_wsk_f = daten_wsk,
                            detection_wsk_per_agent_f = detection_wsk_per_agent,
                            quarantine_f = TRUE)

plot_MonteCarloTest_together(daten_MonteCarloTest = test_monte_carlo, 
                             durchlauf_type = c("Schnitt"),
                             agent_status = c("I_"))

plot_MonteCarloTest_together(daten_MonteCarloTest = test_monte_carlo, 
                             durchlauf_type = c("Schnitt"),
                             agent_status = c("S"))

#----------------------------------------------------------------

  

#-------------------------DEPRECATED-----------------------------
#-----------------------Erstes Modell (SIR)----------------------
# Erstes Modell nur mit Freizeitkontakte

# Zeitstempel fuer Infektion hinterlegen
agents_basic_model <- zeitstempel_hinterlegen(agents, "infected")

tracker_cases_1 <- data.frame(Tag = 0,
                      S = sum(agents_basic_model$susceptible),
                      I = sum(agents_basic_model$infected),
                      R = sum(agents_basic_model$removed),
                      Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  goverment_day <- goverment %>%
    filter(Tag == day)
  
  contacts <- data.frame(Id_agent = c(), 
                         type_of_contact = c(), 
                         Id_contact = c())
  
  # contacts <- kontakte_erstellen_haushalt(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_freizeit(agents_basic_model, 
                                          contacts, 
                                          daten_wsk,
                                          goverment_day)
  # contacts <- kontakte_erstellen_neu(agents_basic_model, household = TRUE, wsk = daten_wsk)

  agents_basic_model <- infected_status_aendern_ohne_quarantine(agents_basic_model, contacts)
  
  neuinfektionen <- agents_basic_model %>%
    filter(infected == TRUE & time_infected == 0) %>%
    nrow()
  
  # Sobald Zeit infected 7 ist -> nicht mehr infected sondern resistant
  agents_basic_model$infected[agents_basic_model$time_infected == 7] <- FALSE
  agents_basic_model$removed[agents_basic_model$time_infected == 7] <- TRUE

  # Time +1 falls man infected ist
  agents_basic_model$time_infected[agents_basic_model$infected == TRUE] <- agents_basic_model$time_infected[agents_basic_model$infected == TRUE] + 1
  
  # Pro Tag Summe merken
  temp <- data.frame(Tag = day,
                     S = sum(agents_basic_model$susceptible),
                     I = sum(agents_basic_model$infected),
                     R = sum(agents_basic_model$removed),
                     Neuinfektionen = neuinfektionen)
  
  tracker_cases_1 <- tracker_cases_1 %>%
    bind_rows(temp)
  
  time_end <- Sys.time()
  difference <- data.frame(time = time_end - time_begin)
  tracker_time <- tracker_time %>%
    bind_rows(difference)
  
  print(paste0("Tag:", day))
}

mean(tracker_time$time)
sum(tracker_time$time)

# # test <- tracker_cases %>%
# #   slice((n() - 7): (n() - 1))
# # 
# # test <- tracker_cases %>%
# #   slice(1:7)
# 
# sum(test$Neuinfektionen) / length(agents_basic_model$Id_agent) * 100000

ggplot(tracker_cases_1) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  #geom_line(aes(y = Neuinfektionen)) +
  ylim(0, length(agents$Id_agent))

# ggplot(tracker_cases) +
#   aes(x = Tag) +
#   geom_line(aes(y = S), color = "green") +
#   xlim(0, 5) +
#   ylim(78100, 78200)

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Zweites Modell (SIR)----------------------
#Zweites Modell mit Haushalt hinzugenommen

# Zeitstempel fuer Infektion hinterlegen
agents_basic_model <- zeitstempel_hinterlegen(agents, "infected")

tracker_cases_2 <- data.frame(Tag = 0,
                            S = sum(agents_basic_model$susceptible),
                            I = sum(agents_basic_model$infected),
                            R = sum(agents_basic_model$removed),
                            Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  goverment_day <- goverment %>%
    filter(Tag == day)
  
  contacts <- data.frame(Id_agent = c(), 
                         type_of_contact = c(), 
                         Id_contact = c())
  
  contacts <- kontakte_erstellen_haushalt(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_freizeit(agents_basic_model, 
                                          contacts, 
                                          daten_wsk,
                                          goverment_day)
  # contacts <- kontakte_erstellen_neu(agents_basic_model, household = TRUE, wsk = daten_wsk)
  
  agents_basic_model <- infected_status_aendern_ohne_quarantine(agents_basic_model, contacts)

  neuinfektionen <- agents_basic_model %>%
    filter(infected == TRUE & time_infected == 0) %>%
    nrow()
  
  # Sobald Zeit infected 7 ist -> nicht mehr infected sondern resistant
  agents_basic_model$infected[agents_basic_model$time_infected == 7] <- FALSE
  agents_basic_model$removed[agents_basic_model$time_infected == 7] <- TRUE
  
  # Time +1 falls man infected ist
  agents_basic_model$time_infected[agents_basic_model$infected == TRUE] <- agents_basic_model$time_infected[agents_basic_model$infected == TRUE] + 1
  
  # Pro Tag Summe merken
  temp <- data.frame(Tag = day,
                     S = sum(agents_basic_model$susceptible),
                     I = sum(agents_basic_model$infected),
                     R = sum(agents_basic_model$removed),
                     Neuinfektionen = neuinfektionen)
  
  tracker_cases_2 <- tracker_cases_2 %>%
    bind_rows(temp)
  
  time_end <- Sys.time()
  difference <- data.frame(time = time_end - time_begin)
  tracker_time <- tracker_time %>%
    bind_rows(difference)
  
  print(paste0("Tag:", day))
}

mean(tracker_time$time)
sum(tracker_time$time)

# test <- tracker_cases %>%
#   slice((n() - 7): (n() - 1))
# 
# test <- tracker_cases %>%
#   slice(1:7)
# 
# sum(test$Neuinfektionen) / length(agents_basic_model$Id_agent) * 100000

ggplot(tracker_cases_2) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  #geom_line(aes(y = Neuinfektionen)) +
  ylim(0, length(agents$Id_agent))

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Drittes Modell (SIR)----------------------
#Drittes Modell mit Haushalt und Arbeitsplatz hinzugenommen

# Zeitstempel fuer Infektion hinterlegen
agents_basic_model <- zeitstempel_hinterlegen(agents, "infected")

tracker_cases_3 <- data.frame(Tag = 0,
                            S = sum(agents_basic_model$susceptible),
                            I = sum(agents_basic_model$infected),
                            R = sum(agents_basic_model$removed),
                            Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  goverment_day <- goverment %>%
    filter(Tag == day)
  
  contacts <- data.frame(Id_agent = c(), 
                         type_of_contact = c(), 
                         Id_contact = c())
  
  contacts <- kontakte_erstellen_freizeit(agents_basic_model, 
                                          contacts, 
                                          daten_wsk,
                                          goverment_day)
  contacts <- kontakte_erstellen_haushalt(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_arbeitsplatz(agents_basic_model, contacts)
  # contacts <- kontakte_erstellen_neu(agents_basic_model, household = TRUE, wsk = daten_wsk)
  
  agents_basic_model <- infected_status_aendern_ohne_quarantine(agents_basic_model, contacts)
  
  neuinfektionen <- agents_basic_model %>%
    filter(infected == TRUE & time_infected == 0) %>%
    nrow()
  
  # Sobald Zeit infected 7 ist -> nicht mehr infected sondern resistant
  agents_basic_model$infected[agents_basic_model$time_infected == 7] <- FALSE
  agents_basic_model$removed[agents_basic_model$time_infected == 7] <- TRUE
  
  # Time +1 falls man infected ist
  agents_basic_model$time_infected[agents_basic_model$infected == TRUE] <- agents_basic_model$time_infected[agents_basic_model$infected == TRUE] + 1
  
  # Pro Tag Summe merken
  temp <- data.frame(Tag = day,
                     S = sum(agents_basic_model$susceptible),
                     I = sum(agents_basic_model$infected),
                     R = sum(agents_basic_model$removed),
                     Neuinfektionen = neuinfektionen)
  
  tracker_cases_3 <- tracker_cases_3 %>%
    bind_rows(temp)
  
  time_end <- Sys.time()
  difference <- data.frame(time = time_end - time_begin)
  tracker_time <- tracker_time %>%
    bind_rows(difference)
  
  print(paste0("Tag:", day))
}

mean(tracker_time$time)
sum(tracker_time$time)

# test <- tracker_cases %>%
#   slice((n() - 7): (n() - 1))
# 
# test <- tracker_cases %>%
#   slice(1:7)
# 
# sum(test$Neuinfektionen) / length(agents_basic_model$Id_agent) * 100000

ggplot(tracker_cases_3) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  #geom_line(aes(y = Neuinfektionen)) +
  ylim(0, length(agents$Id_agent))

ggplot(tracker_cases_1) +
  aes(x = Tag) +
  geom_line(aes(y = I), color = "green") +
  geom_line(aes(y = tracker_cases_2$I), color = "red") +
  geom_line(aes(y = tracker_cases_3$I))

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Viertes Modell (SIR)----------------------
#Viertes Modell mit Haushalt, Arbeitsplatz und Volksschule hinzugenommen

# Zeitstempel fuer Infektion hinterlegen
agents_basic_model <- zeitstempel_hinterlegen(agents, "infected")

tracker_cases_4 <- data.frame(Tag = 0,
                              S = sum(agents_basic_model$susceptible),
                              I = sum(agents_basic_model$infected),
                              R = sum(agents_basic_model$removed),
                              Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  goverment_day <- goverment %>%
    filter(Tag == day)
  
  contacts <- data.frame(Id_agent = c(), 
                         type_of_contact = c(), 
                         Id_contact = c())
  
  contacts <- kontakte_erstellen_freizeit(agents_basic_model, 
                                          contacts, 
                                          daten_wsk,
                                          goverment_day)
  contacts <- kontakte_erstellen_haushalt(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_arbeitsplatz(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_volksschule(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_schule(agents_basic_model, contacts)
  # contacts <- kontakte_erstellen_neu(agents_basic_model, household = TRUE, wsk = daten_wsk)
  
  agents_basic_model <- infected_status_aendern_ohne_quarantine(agents_basic_model, contacts)
  
  neuinfektionen <- agents_basic_model %>%
    filter(infected == TRUE & time_infected == 0) %>%
    nrow()
  
  # Sobald Zeit infected 7 ist -> nicht mehr infected sondern resistant
  agents_basic_model$infected[agents_basic_model$time_infected == 7] <- FALSE
  agents_basic_model$removed[agents_basic_model$time_infected == 7] <- TRUE
  
  # Time +1 falls man infected ist
  agents_basic_model$time_infected[agents_basic_model$infected == TRUE] <- agents_basic_model$time_infected[agents_basic_model$infected == TRUE] + 1
  
  # Pro Tag Summe merken
  temp <- data.frame(Tag = day,
                     S = sum(agents_basic_model$susceptible),
                     I = sum(agents_basic_model$infected),
                     R = sum(agents_basic_model$removed),
                     Neuinfektionen = neuinfektionen)
  
  tracker_cases_4 <- tracker_cases_4 %>%
    bind_rows(temp)
  
  time_end <- Sys.time()
  difference <- data.frame(time = time_end - time_begin)
  tracker_time <- tracker_time %>%
    bind_rows(difference)
  
  print(paste0("Tag:", day))
}

mean(tracker_time$time)
sum(tracker_time$time)

# test <- tracker_cases %>%
#   slice((n() - 7): (n() - 1))
# 
# test <- tracker_cases %>%
#   slice(1:7)
# 
# sum(test$Neuinfektionen) / length(agents_basic_model$Id_agent) * 100000

ggplot(tracker_cases_4) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  #geom_line(aes(y = Neuinfektionen)) +
  ylim(0, length(agents$Id_agent))

ggplot(tracker_cases_1) +
  aes(x = Tag) +
  geom_line(aes(y = I), color = "green") +
  geom_line(aes(y = tracker_cases_2$I), color = "red") +
  geom_line(aes(y = tracker_cases_3$I), color = "blue") +
  geom_line(aes(y = tracker_cases_4$I), color = "grey")

#----------------------------------------------------------------
#----------------------------------------------------------------
#-----------------------Fuenftes Modell (SIR)----------------------
# Fuenftes Modell Kontakt wie viertes plus Infektioese Zeit aendern!

# Zeitstempel fuer Infektion hinterlegen
agents_basic_model <- zeitstempel_hinterlegen(agents, "infected")




tracker_cases_5 <- data.frame(Tag = 0,
                              S = sum(agents_basic_model$susceptible),
                              I = sum(agents_basic_model$infected),
                              R = sum(agents_basic_model$removed),
                              Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  goverment_day <- goverment %>%
    filter(Tag == day)
  
  contacts <- data.frame(Id_agent = c(), 
                         type_of_contact = c(), 
                         Id_contact = c())
  
  contacts <- kontakte_erstellen_freizeit(agents_basic_model, 
                                          contacts, 
                                          daten_wsk,
                                          goverment_day)
  contacts <- kontakte_erstellen_haushalt(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_arbeitsplatz(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_volksschule(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_schule(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_kindergarten(agents_basic_model, contacts)
  # contacts <- kontakte_erstellen_neu(agents_basic_model, household = TRUE, wsk = daten_wsk)
  
  agents_basic_model <- infected_status_aendern_ohne_quarantine(agents_basic_model, contacts)
  
  neuinfektionen <- agents_basic_model %>%
    filter(infected == TRUE & time_infected == 0) %>%
    nrow()
  
  # Sobald Zeit infected 7 ist -> nicht mehr infected sondern resistant
  agents_basic_model$infected[agents_basic_model$time_infected == 7] <- FALSE
  agents_basic_model$removed[agents_basic_model$time_infected == 7] <- TRUE
  
  # Time +1 falls man infected ist
  agents_basic_model$time_infected[agents_basic_model$infected == TRUE] <- agents_basic_model$time_infected[agents_basic_model$infected == TRUE] + 1
  
  # Pro Tag Summe merken
  temp <- data.frame(Tag = day,
                     S = sum(agents_basic_model$susceptible),
                     I = sum(agents_basic_model$infected),
                     R = sum(agents_basic_model$removed),
                     Neuinfektionen = neuinfektionen)
  
  tracker_cases_5 <- tracker_cases_5 %>%
    bind_rows(temp)
  
  time_end <- Sys.time()
  difference <- data.frame(time = time_end - time_begin)
  tracker_time <- tracker_time %>%
    bind_rows(difference)
  
  print(paste0("Tag:", day))
}

mean(tracker_time$time)
sum(tracker_time$time)

# # test <- tracker_cases %>%
# #   slice((n() - 7): (n() - 1))
# # 
# # test <- tracker_cases %>%
# #   slice(1:7)
# 
# sum(test$Neuinfektionen) / length(agents_basic_model$Id_agent) * 100000

ggplot(tracker_cases_5) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  #geom_line(aes(y = Neuinfektionen)) +
  ylim(0, length(agents$Id_agent))

ggplot(tracker_cases_1) +
  aes(x = Tag) +
  geom_line(aes(y = I), color = "green") +
  geom_line(aes(y = tracker_cases_2$I), color = "red") +
  geom_line(aes(y = tracker_cases_3$I), color = "blue") +
  geom_line(aes(y = tracker_cases_4$I), color = "grey") +
  geom_line(aes(y = tracker_cases_5$I), color = "pink")

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------Testcode----------------------------

#Testcode fuer Agentswskmatrix

test <- agents %>%
  slice(1:9000) %>%
  # filter(Id_municipality == 31502) %>%
  select(Id_agent, Id_municipality)

test_kontakte <- data.frame(Id_agent = test$Id_agent, 
                       contacts = round(rgamma(length(test$Id_agent),6.11,1)))

test_agents <- agents %>%
  select(Id_agent, Id_municipality) %>%
  rename(Id_test = Id_municipality,
         Id_contact = Id_agent)

test2 <- test %>%
  left_join(daten_wsk, by = c("Id_municipality" = "centre_i")) %>%
  left_join(test_agents, by = c("centre_j" = "Id_test"))

microbenchmark({
test3 <- test2 %>%
  left_join(test_kontakte, by = "Id_agent") %>%
  group_by(Id_agent) %>%
  sample_n(unique(contacts), replace = TRUE, weight = wsk)
}, times = 1L)

test3 <- test2 %>%
  select(- c(wsk, contacts)) %>%
  left_join(test_agents, by = c("centre_j" = "Id_test"))

mat <- matrix(test, nrow(agents), nrow(agents))



# Keine Ahnung was das ist
# start_time <- Sys.time()
# daten_agent <- agents_basic_model
# 
# # Anzahl an Kontakten fuer alle Agents erstellen
# kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
#                        contacts = round(rgamma(length(daten_agent$Id_agent),6.11,1)))
# 
# temp1 <- agents %>%
#   select(Id_agent, Id_municipality)
# 
# kontakte <- kontakte %>%
#   left_join(temp1, by = "Id_agent")
# 
# test <- kontakte %>%
#   rename(centre_i = Id_municipality) %>%
#   inner_join(daten_wsk, by = "centre_i")
# 
# test2 <- test %>%
#   group_by(Id_agent) %>%
#   summarise(Id_municipality = sample(centre_j, unique(contacts), replace = TRUE, prob = wsk)) %>%
#   ungroup()
# 
# test3 <- daten_agent %>%
#   group_by(Id_municipality) %>%
#   summarise(Liste_an_agents = list(Id_agent))
# 
# test4 <- test2 %>%
#   left_join(test3, by = "Id_municipality")
# 
# test5 <- test4 %>%
#   group_by(Id_municipality) %>%
#   mutate(Id_contact = sample(unlist(Liste_an_agents), n(), replace = TRUE)) %>%
#   ungroup() %>%
#   select(- Liste_an_agents)
# end_time <- Sys.time()
# print(end_time - start_time)
# 
# kontakte_dt <- as.data.table(kontakte)
# daten_wsk_dt <- as.data.table(daten_wsk) 
# daten_agent_dt <- as.data.table(daten_agent)
# 
# start_time <- Sys.time()
# daten_agent <- agents_basic_model
# 
# # Anzahl an Kontakten fuer alle Agents erstellen
# kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
#                        contacts = round(rgamma(length(daten_agent$Id_agent),6.11,1)))
# 
# temp1 <- agents %>%
#   select(Id_agent, Id_municipality)
# 
# kontakte <- kontakte %>%
#   left_join(temp1, by = "Id_agent")
# 
# test <- kontakte %>%
#   rename(centre_i = Id_municipality) %>%
#   inner_join(daten_wsk, by = "centre_i")
# 
# test_dt <- as.data.table(test)
# 
# test2_dt <- test_dt[, .(Id_municipality = sample(centre_j, unique(contacts), replace = TRUE, prob = wsk)), by="Id_agent"]
# 
# test3_dt <- daten_agent_dt[order(Id_municipality), .(Liste_an_agents = list(Id_agent)), by = "Id_municipality"]
# 
# test4_dt <- merge(test2_dt, test3_dt, by = "Id_municipality")
# 
# test5_dt <- test4_dt[, .(Id_agent, Id_contact = sample(unlist(Liste_an_agents), .N, replace = TRUE)), by = "Id_municipality"]
# 
# test5_dt <- test5_dt[order(Id_agent), .(Id_agent, Id_municipality, Id_contact)]
# 
# test5 <- as.data.frame(test5_dt) %>%
#   select(Id_agent, Id_contact) %>%
#   mutate(type_of_contact = "leisure", .after = "Id_agent")
# 
# end_time <- Sys.time() 
# print(end_time - start_time)
# 
# #TESTCODE
# test3 <- test2 %>%
#   group_by(Id_agent, Id_municipality) %>%
#   summarise(Anzahl = n()) %>%
#   ungroup() %>%
#   group_by(Id_agent) %>%
#   filter(Anzahl == max(Anzahl)) %>%
#   ungroup()
# 
# test4 <- test3 %>%
#   left_join(kontakte, by = "Id_agent") 

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Real-Daten-Vergleich----------------------


  
#----------------------------------------------------------------
#----------------------------------------------------------------
#------------------Basic Model mit for-Schleife------------------

#DEPRECATED
for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  # Kontakte fuer jeden Agent pro Tag erstellen
  agents <- kontakte_erstellen(agents)
  
  for (i in agents$Id_agent) {
    
    kontakte <- agents %>%
      filter(Id_agent == i)
    
    kontakte <- unlist(kontakte$contact_agent)
    
    for (j in kontakte) {
      
      # status <- status_filtern(agents, id_agent = i, id_kontakt = j)
      
      status_infected <- agents$infected[agents$Id_agent == j]
      status_removed <- agents$removed[agents$Id_agent == i]
      
      if (status_infected == TRUE &  status_removed == FALSE) {
        
        zufallszahl <- runif(1,0,1)
        
        if (zufallszahl < 0.05) {
          
          agents$infected[agents$Id_agent == i] <- TRUE
          agents$susceptible[agents$Id_agent == i] <- FALSE
          
          
        }
        
      }
      
    }
  }
  
  agents$infected[agents$time_infected == 7] <- FALSE
  agents$removed[agents$time_infected == 7] <- TRUE
  
  agents$time_infected[agents$infected == TRUE] <- agents$time_infected[agents$infected == TRUE] + 1
  
  temp <- data.frame(Tag = day,
                     S = sum(agents$susceptible),
                     I = sum(agents$infected),
                     R = sum(agents$removed))
  
  tracker_1 <- tracker_1 %>%
    bind_rows(temp)
  
  time_end <- Sys.time()
  difference <- time_end - time_begin
  
  print(paste0("Tag:", day))
  print(difference)
}

ggplot(tracker_1) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  ylim(0, length(agents$Id_agent))
               