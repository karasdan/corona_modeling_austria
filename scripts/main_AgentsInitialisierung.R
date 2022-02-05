library(tidyverse)
library(jsonlite)
library(readxl)
library(osmdata)
library(sf)

source("./scripts/function_initialisierung.R")

#----------------------Geodaten + Pendelwsk----------------------

# # Haushalt ueber Gemeinden zuordnen
# file_municipality <- "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/GeoDaten/geodaten_Austria_Gemeindeebene.json" 
# 
# # Daten einlesen
# geo_raw_data_for_municipality <- fromJSON(file_municipality) 

# bezirksdaten_sehen() # Um Id auszuwaehlen
district_name <- "Melk"
district_number <- "315"

distance_between_centre <- 
  distance_aus_OSM_daten_berechnen(district_name_f = district_name,
                                   district_number_f = district_number)

infos_mumicipality <- tibble(Id_municipality = as.numeric(unique(distance_between_centre$Id_from)))
infos_mumicipality <- 
  bevoelkerungs_anzahl_name_hinzufuegen(info_daten = infos_mumicipality,
                                        district_number_f = district_number)

wsk_between_centre <- pendelwsk_berechnen(infos_mumicipality, 
                                          distance_between_centre, 
                                          bezirkswahl = district_number)

# # Bezirk Melk beginnt immer mit 315 -> "^315"
# district <- "315" # Id fuer Melk

# # Kooridnaten von Gemeiden waehlen
# coordinates_mumicipality <- koordinaten_auswaehlen_gemeinde(geo_raw_data_for_municipality, district)
# infos_mumicipality <- mittelpunkt_berechnen(coordinates_mumicipality)
#FLAECHE VON POLYGON BERECHNEN -> plyarea VOM pracma PACKAGE

#TESTPLOT
# ggplot(coordinates_mumicipality) +
#   geom_polygon(aes(X,Y, fill = name), show.legend = FALSE) +
#   geom_point(data = infos_mumicipality, aes(X,Y))

# distance_between_centre <- abstand_mittelpunkte_berechnen(infos_mumicipality)

#TESTPLOT
# distance_matrix <- distance_between_centre %>%
#   pivot_wider(names_from = centre_2, values_from = distance)
# 
# ggplot(coordinates_mumicipality) +
#   geom_polygon(aes(X,Y, fill = name), show.legend = FALSE) +
#   geom_point(data = centre_municipality, aes(X,Y, color = distance_matrix$`31524`))

# #TESTEN
# 
# test <- distance_between_municipality_long %>% 
#   filter(distance != 0) %>% 
#   group_by(Id_from) %>% 
#   filter(distance == min(distance)) %>%
#   ungroup()
# 
# name_to <- infos_mumicipality %>%
#   select(Id_municipality, name) %>%
#   rename(name_to = name) %>%
#   mutate(Id_municipality = as.character(Id_municipality))
# 
# name_from <- name_to %>%
#   rename(name_from = name_to)
# 
# ergebnis_osm <- test %>%
#   inner_join(name_from, by = c("Id_from" = "Id_municipality")) %>%
#   inner_join(name_to, by = c("Id_to" = "Id_municipality"))
# 
# test <- distance_between_centre %>% 
#   filter(distance != 0) %>% 
#   group_by(centre_1) %>% 
#   filter(distance == min(distance)) %>%
#   ungroup() %>%
#   mutate(centre_1 = as.character(centre_1),
#          centre_2 = as.character(centre_2))
# 
# temp <- test %>%
#   inner_join(name_from, by = c("centre_1" = "Id_municipality")) %>%
#   inner_join(name_to, by = c("centre_2" = "Id_municipality")) %>%
#   select(name_from, name_to) %>%
#   rename(name_to_alt = name_to)
# 
# ergebnis_osm <- ergebnis_osm %>%
#   inner_join(temp, by = "name_from")
#   
# nrow(ergebnis_osm %>% filter(name_to != name_to_alt))
# 
# #TESTENENDE

#------------------------Agents erstellen------------------------

dateiname <<- ""
bevoelkerungszahlen <- 
  einlesen_und_bearbeite_bevoelkerlungszahlen("/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_altersgruppen_geschlecht_und_bezirken_bzw._nuts_3-r.xlsx",
                                              "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_alter_in_einzeljahren_geschlecht_und_bundesland.xlsx",
                                              district_number)
bevoelkerungs_zahlen_district <- bevoelkerungszahlen$altersklasse
bevoelkerungs_zahlen_einzeljahre <- bevoelkerungszahlen$einzeljahre
anzahl_agents <- sum(infos_mumicipality$Anzahl)
agents <- data.frame(Id_agent = seq(1, anzahl_agents, 1))
agents <- geschlecht_erstellen(agents, bevoelkerungs_zahlen_district, anzahl_agents)
agents <- alter_erstellen(agents, bevoelkerungszahlen, district_number)
agents <- gemeinde_waehlen(agents, infos_mumicipality, anzahl_agents)

#TESTPLOT
# test <- agents %>%
#   group_by(Id_municipality) %>%
#   summarise(anzahl = n())
# 
# test2 <- infos_mumicipality %>%
#   right_join(test, by = "Id_municipality")
# 
# ggplot(coordinates_mumicipality) +
#   geom_polygon(aes(X,Y, fill = name), show.legend = FALSE) +
#   geom_point(data = test2, aes(X,Y, color = anzahl))

haushalts_gesamt_daten <- einlesen_und_bearbeite_haushaltszahlen("data/Haushalte_pro_Bundesland/privathaushalte_nach_haushaltsgroesse_bundeslaendern_und_alter_der_haushal.xlsx")
infos_mumicipality <- haushaltsanzahl_erstellen(infos_mumicipality,
                                                haushalts_gesamt_daten$haushalts_daten_bundesland,
                                                agents,
                                                district_number_f = district_number)
agents <- haushalte_auf_gemeindeebene_erstellen(agents, infos_mumicipality)

arbeitsplatz_daten <- einlesen_und_bearbeite_arbeitsplatzdaten("data/Arbeitsstaetten/gemeindeergebnisse_der_abgestimmten_erwerbsstatistik_und_arbeitsstaettenza.xlsx")
infos_mumicipality <- arbeitsplatzdaten_hinzufuegen(infos_mumicipality, arbeitsplatz_daten)
arbeitsplaetze <- arbeitsplaetze_erstellen(infos_mumicipality)
agents <- arbeit_zuweisen(agents, infos_mumicipality)
agents <- arbeitsplaetze_zuweisen(agents, infos_mumicipality, arbeitsplaetze, wsk_between_centre)

infos_mumicipality <- schuldaten_hinzufuegen(infos_mumicipality)
volksschulplaetze <- volksschulplaetze_erstellen(infos_mumicipality)
agents <- volksschulen_zuweisen(agents, infos_mumicipality, volksschulplaetze, wsk_between_centre)

schulplaetze <- schulplaetze_erstellen(infos_mumicipality)
agents <- schulen_zuweisen(agents, infos_mumicipality, schulplaetze, wsk_between_centre)

infos_mumicipality <- kindergartendaten_hinzufuegen(infos_mumicipality)
kindergartenplaetze <- kindergartenplaetze_erstellen(infos_mumicipality)
agents <- kindergarten_zuweisen(agents, infos_mumicipality, kindergartenplaetze, wsk_between_centre)

#----------------------Speichern der Agents----------------------

# Datensatz immer speichern und neu laden
dateiname <- paste0(dateiname, "_", format(lubridate::date(Sys.time()), "%Y_%m_%d"))
setwd("./agents_initialisierung")
save(agents, wsk_between_centre, 
     file = paste0(dateiname, ".RData"))
setwd("..")


#---------------------------DEPRECATED---------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
#TESTCODE FUER SCHULEN ERSTELLEN
#TESTCODE
daten_agent <- agents
daten_info <- infos_mumicipality
daten_schulplaetze <- schulplaetze
daten_wsk <- wsk_between_centre

daten_agent_schule <- daten_agent %>%
  filter(type_of_work == "school")

anzahl_plaetze <- nrow(daten_schulplaetze)
anzahl_schueler <- nrow(daten_agent_schule)

if (anzahl_schueler <= anzahl_plaetze) {
  anzahl <- anzahl_schueler
} else {
  anzahl <- anzahl_plaetze
}

for (j in 1:anzahl) {
  
  temp2 <- daten_wsk %>%
    filter(centre_j %in% unique(daten_schulplaetze$Id_municipality))
  
  ein_schueler <- daten_agent_schule %>%
    sample_n(1) %>%
    select(Id_agent, Id_municipality) %>%
    left_join(temp2, by = c("Id_municipality" = "centre_i")) 
  
  ein_schueler <- ein_schueler %>%
    left_join(daten_schulplaetze, by = c("centre_j" = "Id_municipality")) %>%
    sample_n(1, weight = wsk)
  
  daten_schulplaetze <- daten_schulplaetze %>%
    anti_join(ein_schueler, by = "Id_schulplatz")
  
  daten_agent_schule <- daten_agent_schule %>%
    anti_join(ein_schueler, by = "Id_agent")
  
  ein_schueler <- ein_schueler %>%
    select(Id_agent, Id_schule, centre_j) %>%
    rename(Id_workplace_new = Id_schule,
           Id_municipality_new = centre_j)
  
  daten_agent <- daten_agent %>%
    left_join(ein_schueler, by = "Id_agent") %>%
    mutate(Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new)),
           Id_municipality_work = if_else(is.na(Id_municipality_new), Id_municipality_work, as.double(Id_municipality_new))) %>%
    select(- c(Id_municipality_new, Id_workplace_new))
  
  if ((j %% 100) == 0 | j == anzahl) {
    
    cat(paste0(j, " von ", anzahl, " Agents haben einen freien Schulplatz! \n"))
    
  }
}

daten_agent <- daten_agent %>%
  mutate(Id_municipality_work = if_else(type_of_work == "school" & Id_workplace == 0, 99999, Id_municipality_work))



#TESTCODE FUER ARBEITSPLATZ ERSTELLUNG

daten_info <- infos_mumicipality
daten_agent <- agents

wo_arbeiten <- data.frame(Id_agent = c(),
                          Id_municipality = c(),
                          arbeiten = c())

bundesland <- as.numeric(substr(daten_agent$Id_municipality[1], 1, 1))

# Percentage liste von Niki Popper Paper -> letzten beiden zusammengefasst!!!
percentage <- switch(bundesland,
                     c(0.26, 0.21, 0.53), #Burgenland
                     c(0.46, 0.16, 0.38), #Kaernten
                     c(0.29, 0.2, 0.51),  #Niederoesterreich
                     c(0.35, 0.24, 0.41), #Oberoesterreich
                     c(0.46, 0.21, 0.33), #Salzburg
                     c(0.42, 0.23, 0.35), #Steiermark
                     c(0.41, 0.3, 0.29),  #Tirol
                     c(0.37, 0.35, 0.28), #Vorarlberg
                     c(0.01, 0.04, 0.95)  #Wien
                     )

prob2 <- c(percentage[2] / sum(percentage[2:3]), percentage[3] / sum(percentage[2:3]))

for (id in daten_info$Id_municipality) {
  
  daten_agent_municipaltiy <- daten_agent %>%
    filter(Id_municipality == id & type_of_work == "work")
  
  daten_info_municipality <- daten_info %>%
    filter(Id_municipality == id)
  
  number_of_working_people <- nrow(daten_agent_municipaltiy)
  
  prob <- c(daten_info_municipality$`AnteilderAuspendler/-innenandenaktivErwerbs-tätigenamWohnort`,
            100 - daten_info_municipality$`AnteilderAuspendler/-innenandenaktivErwerbs-tätigenamWohnort`)
  
  arbeiten <- sample(c("o", "m"), number_of_working_people, prob = prob, replace = TRUE)
  
  temp3 <- data.frame(Id_agent = daten_agent_municipaltiy$Id_agent,
                      Id_municipality = daten_agent_municipaltiy$Id_municipality, 
                      arbeiten = arbeiten)
  
  temp3 <- temp3 %>%
    group_by(arbeiten) %>%
    mutate(arbeiten = if_else(arbeiten != "m", 
                              sample(c("d", "o"), n(), prob = prob2, replace = TRUE),
                              arbeiten)) %>%
    ungroup()
  
  wo_arbeiten <- wo_arbeiten %>%
    bind_rows(temp3)
  
}
 #TEST
# test <- wo_arbeiten %>%
#   group_by(Id_municipality, arbeiten) %>%
#   summarise(Anzahl = n()) %>%
#   filter(arbeiten == "m")
# 
# sum(test$Anzahl < daten_info$BeschäftigteindenArbeitsstätten)
# median(test$Anzahl - daten_info$BeschäftigteindenArbeitsstätten)

wo_arbeiten <- wo_arbeiten %>%
  mutate(Id_municipality_work = if_else(arbeiten == "m", Id_municipality, 99999))

wo_arbeiten_municipality <- wo_arbeiten %>%
  filter(arbeiten == "m")

wo_arbeiten_district <- wo_arbeiten %>%
  filter(arbeiten == "d")

wo_arbeiten_other <- wo_arbeiten %>%
  filter(arbeiten == "o") %>%
  mutate(Id_workplace = 0)

# beginnen mit aufteilen der Arbeitsplaetze auf jene die in der Gemeinde bleiben
wo_arbeiten_municipality <- wo_arbeiten_municipality %>%
  mutate(Id_workplace = 0)

moegliche_arbeitsplaetze_1 <- moegliche_arbeitsplaetze

for (id in unique(wo_arbeiten_municipality$Id_municipality_work)) {
  
  temp1 <- wo_arbeiten_municipality %>%
    filter(Id_municipality_work == id)
  
  moegliche_arbeitsplaetze_municipality <- moegliche_arbeitsplaetze_1 %>%
    filter(Id_municipality == id)
  
  workplace_id <- moegliche_arbeitsplaetze_municipality %>%
    sample_n(nrow(temp1), replace = FALSE)
  
  moegliche_arbeitsplaetze_1 <- moegliche_arbeitsplaetze_1 %>%
    anti_join(workplace_id, by = "Id_arbeitsplatz")
  
  temp1$Id_workplace <- workplace_id$Arbeitsstaette
  
  temp1 <- temp1 %>%
    select(Id_agent, Id_workplace) %>%
    rename(Id_workplace_new = Id_workplace)
  
  wo_arbeiten_municipality <- wo_arbeiten_municipality %>%
    left_join(temp1, by = "Id_agent") %>%
    mutate(Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new))) %>%
    select(- Id_workplace_new)
  
}

# beginnen mit aufteilen der Arbeitsplaetze auf jene die in die in andere Gemeinden gehen 
wo_arbeiten_district <- wo_arbeiten_district %>%
  mutate(Id_workplace = 0)

temp4 <- wo_arbeiten_district

for (i in 1:nrow(wo_arbeiten_district)) {
  
  ein_agent <- temp4 %>%
    sample_n(1)
  
  wsk_zu_gemeinde <- wsk_between_centre %>%
    filter(centre_j %in% unique(moegliche_arbeitsplaetze_1$Id_municipality))
  
  ein_agent <- ein_agent %>%
    left_join(wsk_zu_gemeinde, by = c("Id_municipality" = "centre_i")) %>%
    filter(Id_municipality != centre_j) %>%
    sample_n(1, weight = wsk)
  
  ein_agent <- ein_agent %>%
    mutate(Id_municipality_work = centre_j) %>%
    select(- c(centre_j, wsk))
  
  moegliche_arbeitsplaetze_municipality <- moegliche_arbeitsplaetze_1 %>%
    filter(Id_municipality == ein_agent$Id_municipality_work)
  
  workplace_id <- moegliche_arbeitsplaetze_municipality %>%
    sample_n(1, replace = FALSE)
  
  moegliche_arbeitsplaetze_1 <- moegliche_arbeitsplaetze_1 %>%
    anti_join(workplace_id, by = "Id_arbeitsplatz")
  
  ein_agent$Id_workplace <- workplace_id$Arbeitsstaette
  
  # ein_agent <- ein_agent %>%
  #   select(Id_agent, Id_municipality_work, Id_workplace)
  
  # test <- wo_arbeiten_district %>%
  #   left_join(ein_agent, by = "Id_agent") %>%
  #   mutate(Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new))) %>%
  #   select(- Id_workplace_new)
  
  wo_arbeiten_district <- wo_arbeiten_district %>%
    mutate(Id_municipality_work = if_else(Id_agent == ein_agent$Id_agent, ein_agent$Id_municipality_work, Id_municipality_work)) %>%
    mutate(Id_workplace = if_else(Id_agent == ein_agent$Id_agent, as.double(ein_agent$Id_workplace), Id_workplace))
  
  temp4 <- temp4 %>%
    anti_join(ein_agent, by = "Id_agent")
  
}

working_agents <- wo_arbeiten_municipality %>%
  bind_rows(wo_arbeiten_district) %>% 
  bind_rows(wo_arbeiten_other) %>%
  arrange(Id_agent)

working_agents <- working_agents %>%
  select(Id_agent, Id_municipality_work, Id_workplace) %>%
  rename(Id_municipality_work_new = Id_municipality_work,
         Id_workplace_new = Id_workplace)

daten_agent <- daten_agent %>% 
  left_join(working_agents, by = "Id_agent") 

daten_agent <- daten_agent %>%
  mutate(Id_municipality_work = if_else(is.na(Id_municipality_work_new), Id_municipality_work, Id_municipality_work_new)) %>%
  mutate(Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, Id_workplace_new)) %>%
  select(- c(Id_workplace_new, Id_municipality_work_new))

#TEST
# test <-  working_agents %>%
#   group_by(Id_municipality_work) %>%
#   summarise(Anzahl = n())
# 
# test$Anzahl <= daten_info$BeschäftigteindenArbeitsstätten
# sum(daten_info$BeschäftigteindenArbeitsstätten - test$Anzahl)


#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
#ERSTERVERSUCH AGENTSINITIALISIERUNG

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Geodaten fuer Modell----------------------

file <- "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/GeoDaten/geodaten_Austria_Bezirksebene.json" # JSON File mit groberer Aufloesung
# federal_state <- 3 # Waehle von 1-9 (3 == NOE)
# district <- "Melk" # GEHEN WIR UEBER ISO??

# Daten einlesen
geo_raw_data_for_districts <- fromJSON(file)

#----------------------------------------------------------------

dateiname <<- ""
coordinates_districts <- koordinaten_auswaehlen("district",
                                                "Melk",
                                                geo_raw_data_for_districts)

triangles <- triangulierung(coordinates_districts)
triangles <- koordinaten_zu_dreickpunkten_hinzufuegen(triangles,
                                                      coordinates_districts)
triangles <- flaeche_berechnen(triangles)

triangles_plot <- triangles %>%
  pivot_longer(- c(triangles_number, area), names_to = c(".value", "point"), names_pattern = c("(.)_(.+)")) %>%
  arrange(desc(area))

#TESTPLOT
# ggplot(coordinates_districts) +
#   aes(x = X, y = Y) +
#   geom_point() +
#   geom_polygon(data = filter(triangles_plot, triangles_number == 59), aes(X,Y, fill = triangles_number))
#
# zahl <- floor(runif(1,1,length(triangles_plot$triangles_number)/3))
#
# temp <- triangles_plot %>%
#   filter(triangles_number == zahl)
#
# ggplot(coordinates_districts) +
#   aes(x = X, y = Y) +
#   geom_polygon() +
#   geom_polygon(data = temp, aes(X,Y, fill = area))

#----------------------------------------------------------------
#----------------------------------------------------------------
#------------------------Agents erstellen------------------------

dateiname <<- ""
bevoelkerungszahlen <- 
  einlesen_und_bearbeite_bevoelkerlungszahlen("/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_altersgruppen_geschlecht_und_bezirken_bzw._nuts_3-r.xlsx",
                                              "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_alter_in_einzeljahren_geschlecht_und_bundesland.xlsx",
                                              "Melk")
bevoelkerungs_zahlen_district <- bevoelkerungszahlen$altersklasse
bevoelkerungs_zahlen_einzeljahre <- bevoelkerungszahlen$einzeljahre
anzahl_agents <- bevoelkerungs_zahlen_district$gesamt_einwohner
agents <- data.frame(Id_agent = seq(1, anzahl_agents, 1))
agents <- geschlecht_erstellen(agents, bevoelkerungs_zahlen_district, anzahl_agents)
agents <- alter_erstellen(agents, bevoelkerungszahlen, "Niederösterreich")
# agents <- haushalt_erstellen(agents, triangles, anzahl_agents) # DEPRECATED
agents <- gesundheit_erstellen(agents, 100)

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Speichern der Agents----------------------

# Datensatz immer speichern und neu laden
dateiname <- paste0(dateiname, "_", format(lubridate::date(Sys.time()), "%Y_%m_%d"))
setwd("./agents_initialisierung")
save(agents, 
     file = paste0(dateiname, ".RData"))
setwd("..")








