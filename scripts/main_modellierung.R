library(tidyverse)
library(microbenchmark)

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
agents_basic_model <- zeitstempel_hinterlegen(agents, "infected")

tage <- 100

tracker_cases <- data.frame(Tag = 0,
                      S = sum(agents_basic_model$susceptible),
                      I = sum(agents_basic_model$infected),
                      R = sum(agents_basic_model$removed),
                      Neuinfektionen = 0)

tracker_time <- data.frame(time = c())

for (day in 1:tage) {
  
  time_begin <- Sys.time()

  kontakte <- kontakte_erstellen_neu(agents_basic_model)
  agents_basic_model <- infected_status_aendern_neu(agents_basic_model, kontakte)
  
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

test <- tracker_cases %>%
  slice((n() - 7): (n() - 1))

test <- tracker_cases %>%
  slice(1:7)

sum(test$Neuinfektionen) / length(agents_basic_model$Id_agent) * 100000

ggplot(tracker_cases) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  geom_line(aes(y = I), color = "red") +
  geom_line(aes(y = R)) +
  #geom_line(aes(y = Neuinfektionen)) +
  ylim(0, length(agents$Id_agent))

ggplot(tracker_cases) +
  aes(x = Tag) +
  geom_line(aes(y = S), color = "green") +
  xlim(0, 5) +
  ylim(78100, 78200)

#----------------------------------------------------------------
#----------------------------------------------------------------

setwd("./data")
test2 <- read_csv2("test.csv")
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
  

# Kontakte fuer jeden Agent pro Tag erstellen
# microbenchmark(kontakte_erstellen(agents), times = 1L)
# microbenchmark(kontakte_erstellen_neu(agents), times = 1L)

#agents <- kontakte_erstellen(agents)

# WSK fuer Infektion pro Kontakt
# microbenchmark(infektion(agents), times = 1L)
# agents <- infektion(agents)
# microbenchmark(infected_status_aendern(agents), times = 1L)
# agents <- infected_status_aendern(agents)

#microbenchmark(infected_status_aendern_neu(agents, kontakte), times = 1L)

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
               