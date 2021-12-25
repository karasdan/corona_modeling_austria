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
district <- "319" # Id fuer Melk
indices <- which(str_detect(geo_raw_data_for_municipality$features$properties$iso, paste0("^", district)))
iso <- as.numeric(geo_raw_data_for_municipality$features$properties$iso[indices])
name <- geo_raw_data_for_municipality$features$properties$name[indices]

for (j in 1:length(indices)) {
  
  coordinates <- geo_raw_data_for_municipality$features$geometry$coordinates[[indices[j]]]
  
  laenge <- (length(coordinates)/2)
  
  X <- coordinates[1: laenge]
  Y <- coordinates[(laenge + 1): (laenge * 2)]
  
  if (exists("coordinates_gesamt") == FALSE) {
    
    coordinates_gesamt <- data.frame(name = rep(name[j], laenge),
                                     Id_district = rep(iso[j], laenge),
                                     Id_point = seq(1, laenge),
                                     X = X, 
                                     Y = Y)
    
  } else {
    
    temp1 <- data.frame(name = rep(name[j], laenge),
                        Id_district = rep(iso[j], laenge),
                        Id_point = seq(1, laenge),
                        X = X, 
                        Y = Y)
    
    coordinates_gesamt <- coordinates_gesamt %>%
      bind_rows(temp1)
    
  }
}

#FLAECHE VON POLYGON BERECHNEN -> plyarea VOM pracma PACKAGE

for (j in unique(coordinates_gesamt$Id_district)) {
  
  if (exists("centre_gesamt") == FALSE) {
    
    temp1 <- coordinates_gesamt %>%
      filter(Id_district == j) %>%
      select(X,Y)
    
    centre_gesamt <- data.frame(Id_district = j,
                                X = geosphere::centroid(temp1)[1],
                                Y = geosphere::centroid(temp1)[2])
    
  } else {
    
    temp1 <- coordinates_gesamt %>%
      filter(Id_district == j) %>%
      select(X,Y)
    
    temp2 <- data.frame(Id_district = j,
                        X = geosphere::centroid(temp1)[1],
                        Y = geosphere::centroid(temp1)[2])
    
    centre_gesamt <- centre_gesamt %>%
      bind_rows(temp2)
  }
}

#TESTPLOT
# ggplot(coordinates_gesamt) +
#   geom_polygon(aes(X,Y, fill = name), show.legend = FALSE) +
#   geom_point(data = centre_gesamt, aes(X,Y))

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


#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
#TESTCODE 



#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
#DEPRECATED

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








