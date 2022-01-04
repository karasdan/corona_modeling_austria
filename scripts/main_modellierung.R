library(tidyverse)
library(microbenchmark)
library(data.table)

#----------------------------------------------------------------

file_agents <- "districtMelk_Niederösterreich_household_1infected_2022_01_04.RData"
# file_agents <- "districtRust(Stadt)_Burgenland_household_4infected_2021_12_19.RData"

file_wsk <- "districtMelk_0.05.RData"
# file_wsk <- "districtMelk_1.RData"
# file_wsk <- "districtMelk_0.001.RData"

# Agentdateneinlesen -> Initialisierung
setwd("./agents_initialisierung")
load(file_agents)
setwd("..")

setwd("./pendel_wsk")
load(file_wsk)
setwd("..")

setwd("./scripts")
source("function_modellierung.R")
setwd("..")

#----------------------------------------------------------------
#----------------------------------------------------------------
#-----------------------Erstes Modell (SIR)----------------------
# Erstes Modell nur mit Freizeitkontakte

# Zeitstempel fuer Infektion hinterlegen
agents_basic_model <- zeitstempel_hinterlegen(agents, "infected")

tage <- 10

tracker_cases <- data.frame(Tag = 0,
                      S = sum(agents_basic_model$susceptible),
                      I = sum(agents_basic_model$infected),
                      R = sum(agents_basic_model$removed),
                      Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  
  contacts <- data.frame(Id_agent = c(), 
                         type_of_contact = c(), 
                         Id_contact = c())
  
  # contacts <- kontakte_erstellen_haushalt(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_freizeit(agents_basic_model, contacts, daten_wsk)
  # contacts <- kontakte_erstellen_neu(agents_basic_model, household = TRUE, wsk = daten_wsk)

  agents_basic_model <- infected_status_aendern_neu(agents_basic_model, contacts)
  
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
  
  tracker_cases <- tracker_cases %>%
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

ggplot(tracker_cases) +
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

tage <- 5

tracker_cases <- data.frame(Tag = 0,
                            S = sum(agents_basic_model$susceptible),
                            I = sum(agents_basic_model$infected),
                            R = sum(agents_basic_model$removed),
                            Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  contacts <- data.frame(Id_agent = c(), 
                         type_of_contact = c(), 
                         Id_contact = c())
  
  contacts <- kontakte_erstellen_haushalt(agents_basic_model, contacts)
  contacts <- kontakte_erstellen_freizeit(agents_basic_model, contacts, daten_wsk)
  # contacts <- kontakte_erstellen_neu(agents_basic_model, household = TRUE, wsk = daten_wsk)
  
  agents_basic_model <- infected_status_aendern_neu(agents_basic_model, contacts)

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
  
  tracker_cases <- tracker_cases %>%
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

ggplot(tracker_cases) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  #geom_line(aes(y = Neuinfektionen)) +
  ylim(0, length(agents$Id_agent))

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------Testcode----------------------------

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

setwd("./data")
test2 <- read_csv2("Coronadaten_Bezirksebene_echt.csv")
setwd("..")

test3 <- test2 %>%
  filter(GKZ == 315) %>%
  filter(AnzahlFaelleSum > 0) %>%
  mutate(Tag = 1:n(), .after = Time) %>%
  filter(Tag <= tage)

ggplot(test3) +
  aes(x = Tag, y = AnzahlFaelle) +
  geom_line() +
  geom_line(data = tracker_cases, aes(x = Tag, y = Neuinfektionen))
  
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
               