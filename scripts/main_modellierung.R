library(tidyverse)

#----------------------------------------------------------------

file <- "districtMelk_Niederösterreich_household_100infected_2021_12_20.RData"
# file <- "districtRust(Stadt)_Burgenland_household_4infected_2021_12_19.RData"

# Agentdateneinlesen -> Initialisierung
setwd("./agents_initialisierung")
load(file)
setwd("..")

setwd("./scripts")
source("function_modellierung.R")
setwd("..")

#----------------------------------------------------------------
#----------------------------------------------------------------
#-----------------------Erstes Modell (SIR)----------------------

# Erstes Modell mit weniger agents

# Zeitstempel fuer Infektion hinterlegen
agents <- zeitstempel_hinterlegen(agents, "infected")

tage <- 50

tracker_cases <- data.frame(Tag = 0,
                      S = sum(agents$susceptible),
                      I = sum(agents$infected),
                      R = sum(agents$removed))

tracker_time <- data.frame(time = c())

tracker_1 <- data.frame(Tag = 0,
                        S = sum(agents$susceptible),
                        I = sum(agents$infected),
                        R = sum(agents$removed))

for (day in 1:tage) {
  
  time_begin <- Sys.time()
  
  # Kontakte fuer jeden Agent pro Tag erstellen
  agents <- kontakte_erstellen(agents)
  
  # WSK fuer Infektion pro Kontakt
  agents <- infektion(agents)
  agents <- infected_status_aendern(agents)
  
  # Sobald Zeit infected 7 ist -> nicht mehr infected sondern resistant
  agents$infected[agents$time_infected == 7] <- FALSE
  agents$removed[agents$time_infected == 7] <- TRUE

  # Time +1 falls man infected ist
  agents$time_infected[agents$infected == TRUE] <- agents$time_infected[agents$infected == TRUE] + 1
  
  # Pro Tag Summe merken
  temp <- data.frame(Tag = day,
                     S = sum(agents$susceptible),
                     I = sum(agents$infected),
                     R = sum(agents$removed))
  
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

ggplot(tracker_cases) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  ylim(0, length(agents$Id_agent))

#----------------------------------------------------------------
#----------------------------------------------------------------
#------------------Basic Model mit for-Schleife------------------

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
               