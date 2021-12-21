library(tidyverse)
library(jsonlite)
library(readxl)

source("./scripts/function_initialisierung.R")

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
agents <- haushalt_erstellen(agents, triangles, anzahl_agents)
agents <- gesundheit_erstellen(agents, 100)

# Datensatz immer speichern und neu laden
dateiname <- paste0(dateiname, "_", format(lubridate::date(Sys.time()), "%Y_%m_%d"))
setwd("./agents_initialisierung")
save(agents, 
     file = paste0(dateiname, ".RData"))
setwd("..")





