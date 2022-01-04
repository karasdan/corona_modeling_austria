library(tidyverse)
library(jsonlite)
library(readxl)

source("./scripts/function_initialisierung.R")

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Geodaten fuer Modell----------------------

# Haushalt ueber Gemeinden zuordnen
file_municipality <- "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/GeoDaten/geodaten_Austria_Gemeindeebene.json" 

# Daten einlesen
geo_raw_data_for_municipality <- fromJSON(file_municipality) 

# Bezirk Melk beginnt immer mit 315 -> "^315"
# bezirksdaten_sehen() # Um Id auszuwaehlen
district <- "315" # Id fuer Melk

# Kooridnaten von Gemeiden waehlen
coordinates_mumicipality <- koordinaten_auswaehlen_gemeinde(geo_raw_data_for_municipality, district)
infos_mumicipality <- mittelpunkt_berechnen(coordinates_mumicipality)
#FLAECHE VON POLYGON BERECHNEN -> plyarea VOM pracma PACKAGE

#TESTPLOT
# ggplot(coordinates_mumicipality) +
#   geom_polygon(aes(X,Y, fill = name), show.legend = FALSE) +
#   geom_point(data = infos_mumicipality, aes(X,Y))

distance_between_centre <- abstand_mittelpunkte_berechnen(infos_mumicipality)

#TESTPLOT
# distance_matrix <- distance_between_centre %>%
#   pivot_wider(names_from = centre_2, values_from = distance)
# 
# ggplot(coordinates_mumicipality) +
#   geom_polygon(aes(X,Y, fill = name), show.legend = FALSE) +
#   geom_point(data = centre_municipality, aes(X,Y, color = distance_matrix$`31524`))

infos_mumicipality <- bevoelkerungs_anzahl_name_hinzufuegen(infos_mumicipality, district)
wsk_between_centre <- pendelwsk_berechnen(infos_mumicipality, 
                                          distance_between_centre, 
                                          speichern = TRUE,
                                          bezirkswahl = district,
                                          distanz_wahl = 0.001)

#----------------------------------------------------------------
#----------------------------------------------------------------
#------------------------Agents erstellen------------------------

dateiname <<- ""
bevoelkerungszahlen <- 
  einlesen_und_bearbeite_bevoelkerlungszahlen("/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_altersgruppen_geschlecht_und_bezirken_bzw._nuts_3-r.xlsx",
                                              "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_alter_in_einzeljahren_geschlecht_und_bundesland.xlsx",
                                              district)
bevoelkerungs_zahlen_district <- bevoelkerungszahlen$altersklasse
bevoelkerungs_zahlen_einzeljahre <- bevoelkerungszahlen$einzeljahre
anzahl_agents <- sum(infos_mumicipality$Anzahl)
agents <- data.frame(Id_agent = seq(1, anzahl_agents, 1))
agents <- geschlecht_erstellen(agents, bevoelkerungs_zahlen_district, anzahl_agents)
agents <- alter_erstellen(agents, bevoelkerungszahlen, district)
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
                                                agents)
agents <- haushalte_auf_gemeindeebene_erstellen(agents, infos_mumicipality)
agents <- gesundheit_erstellen(agents, 1)

#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Speichern der Agents----------------------

# Datensatz immer speichern und neu laden
dateiname <- paste0(dateiname, "_", format(lubridate::date(Sys.time()), "%Y_%m_%d"))
setwd("./agents_initialisierung")
save(agents, 
     file = paste0(dateiname, ".RData"))
setwd("..")


#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
#TESTCODE FUER GEMEINDE ZUGEHOERIGKEIT 

#TESTCODE WAHRSCHEINLICHKEIT VON GEMEINDE ZU GEMEINDE


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








