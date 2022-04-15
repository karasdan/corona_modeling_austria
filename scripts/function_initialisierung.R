
#----------------------Geodaten + Pendelwsk----------------------

#Bezirksdaten zum nachlesen
bezirksdaten_sehen <- function(speichern = FALSE){
  
  file_district <- "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/GeoDaten/geodaten_Austria_Bezirksebene.json"
  
  geo_raw_data_for_district <- fromJSON(file_district)
  
  # Bezirksnamen mit Bundesland und Iso
  bezirksdate_nachlesen <- data.frame(name = geo_raw_data_for_district$features$properties$name,
                                      Id_district = as.numeric(geo_raw_data_for_district$features$properties$iso)) %>%
    arrange(by = Id_district)
  
  Bundesland <- data.frame(Id_bundesland = c(1,2,3,4,5,6,7,8,9),
                           bundesland = c("Burgenland",
                                          "Kärnten",
                                          "Niederösterreich",
                                          "Oberösterreich",
                                          "Salzburg",
                                          "Steiermark",
                                          "Tirol",
                                          "Vorarlberg",
                                          "Wien"))
  
  bezirksdate_nachlesen <- bezirksdate_nachlesen %>%
    mutate(Id_bundesland = as.numeric(substr(Id_district, 1, 1))) 
  
  bezirksdate_nachlesen <- bezirksdate_nachlesen %>%
    right_join(Bundesland, by = "Id_bundesland") %>%
    select(bundesland, name, Id_district)
  
  if (speichern == FALSE) {
    
    view(bezirksdate_nachlesen)
    
  } else {
    
    return(bezirksdate_nachlesen)
    
  }
}

# OSM Data verwenden fuer Distanz berechnen
distance_aus_OSM_daten_berechnen <- function(district_name_f, district_number_f, save_plot = FALSE) {
  
  #TESTCODE
  # district_name_f <- district_name
  # district_number_f <- district_number
  # save_plot <- TRUE
  
  # Bezirksname definieren
  district_name_f <- paste0("Bezirk ", district_name_f)
  
  # Bounding box waehlen
  melk_bbox <- getbb(district_name_f, format_out = "polygon")
  
  # Grenzen der einzelenen Gemeinden aus OSM Daten laden -> Polygone
  melk_boundary <- opq(melk_bbox) %>% 
    add_osm_feature(key = "admin_level", value = "8") %>% 
    osmdata_sf()
  
  # Nur Polygondaten auswaehlen
  melk_municipality_boundary <- melk_boundary$osm_multipolygons
  
  # Alle mit richtigem Beginn waehlen
  melk_municipality_boundary <- melk_municipality_boundary %>%
    filter(grepl(district_number_f, substr(melk_municipality_boundary$ref.at.gkz, 1, 3)) == TRUE)
  
  # Relevante spalten waehlen
  melk_municipality_boundary <- melk_municipality_boundary %>%
    select(osm_id, name, ref.at.gkz, type, geometry)
  
  # Flaeche hinzufuegen
  melk_municipality_boundary$poly_area <- st_area(melk_municipality_boundary)
  
  # Zentren berechnen
  centre <- st_centroid(melk_municipality_boundary)
  
  # id hinzufuegen
  centre$Id <- 1:max(nrow(centre))
  
  # Straßen extrahieren
  melk_road <- opq(melk_bbox) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()
  
  # Nur die Linien herausnehmen
  melk_road_data <- melk_road$osm_lines
  
  melk_road_data <- melk_road_data %>%
    select(osm_id, highway, geometry)
  
  if (save_plot == TRUE){
    
    p1 <- ggplot() +
      geom_sf(data = melk_municipality_boundary, 
              fill = "white",
              color = "black") +
      geom_sf(data = centre, color = "blue", size = 1) +
      theme_void()
    
    p2 <- ggplot() +
      geom_sf(data = melk_municipality_boundary, 
              color = "black", 
              show.legend = FALSE, 
              fill = "white",
              size = 1) +
      geom_sf(data = melk_road_data, color = "lightgrey") +
      theme_void()
    
    picture <- arrangeGrob(p1, p2, nrow = 1)
    ggsave(filename = "/Users/danielkaras/Desktop/Masterarbeit/Latex_Template/Plots_und_Logos/Bezirksgrenzen.png", picture)
    
  }
  
  # Gewichteten Graphen aus Straßennetz machen -> Mit Auto
  g <- dodgr::weight_streetnet(melk_road_data, wt_profile = "motorcar")
  
  # Hilfstibble
  distance_between_municipality <- tibble()
  
  for (i in 1:length(centre$Id)){
    
    # waehle immer nur eine Gemeine aus wohin man reist und berechne es von jeder zu dieser
    from <- st_coordinates(slice(centre, i))
    to <- st_coordinates(centre)
    
    # Distanz berechnen -> shortest = FALSE damit ich schnellsten habe
    temp <- dodgr::dodgr_dists(graph = g, from = from, to = to, shortest = FALSE)
    
    # Als Data.frame
    temp_df <- as.data.frame(temp)
    
    # Spaltennamen hinzufuegen
    colnames(temp_df) <- centre$ref.at.gkz

    # Id der Gemeinde
    id <- centre$ref.at.gkz[i]
    
    # Waehle das dazugehoerige Center aus
    centre_one <- centre %>% filter(ref.at.gkz == id)

    # Die Grenzen dieser Gemeinde 
    boundary_one <- melk_municipality_boundary %>% filter(ref.at.gkz == id)

    to <- st_coordinates(boundary_one)
    from <- st_coordinates(centre_one)

    temp <- dodgr::dodgr_dists(graph = g, from = from, to = to, shortest = FALSE)

    # Rechne mir die durchschnittliche Zeit zur Grenze vom Center aus
    average_rand_distance <- mean(temp, na.rm = TRUE)
    
    # Shifte um diesen Beitrag die Werte
    temp_df <- temp_df + average_rand_distance
    
    # Fuege die Id_from hinzu -> aktuelle Gemeinde
    temp_df <- temp_df %>%
      mutate(Id_from = id, .before = 1)
    
    # Distanz zusammen in einem Tibble
    distance_between_municipality <- distance_between_municipality %>%
      bind_rows(temp_df)
    
    cat(paste0("Gemeinde: ", i, "/", centre$Id, "\n"))
  }
  
  # Laenger machen den Data.frame
  distance_between_municipality_long <- distance_between_municipality %>%
    pivot_longer(cols = - Id_from, names_to = "Id_to", values_to = "distance")
  
  # test <- distance_between_municipality_long %>%
  #   group_by(Id_from) %>%
  #   filter(distance == min(distance))
  
  return(distance_between_municipality_long)
  
}

# Bevoelkerung pro Gemeinde 2019 relative und absolut hinzufuegen
bevoelkerungs_anzahl_name_hinzufuegen <- function(info_daten, district_number_f) {
  
  #TESTCODE
  # info_daten <- infos_mumicipality
  # district_number_f <- district_number
  
  district_number_f <- as.numeric(district_number_f)
  
  raw_data_gemeinde_bevoelkerung <- read_excel("data/Bevoelkerung_Gemeinde_und_Haushatsgroeßen/endgueltige_bevoelkerungszahl_fuer_das_finanzjahr_2021_je_gemeinde.xlsx",
                                               skip = 6)
  
  bevoelkerung_gemeinde <- raw_data_gemeinde_bevoelkerung %>%
    select(1:3)
  
  colnames(bevoelkerung_gemeinde) <- c("Id_municipality",
                                       "name",
                                       "Anzahl") 
  
  bevoelkerung_gemeinde <- bevoelkerung_gemeinde %>%
    filter(! is.na(Anzahl)) %>% 
    filter(! is.na(name)) %>%
    mutate(Id_district = as.numeric(substr(Id_municipality, 1, 3), .before = "Id_municipality"))
  
  bevoelkerung_gemeinde <- bevoelkerung_gemeinde %>%
    group_by(Id_district) %>%
    mutate(Anzahl_rel = Anzahl/sum(Anzahl)) %>%
    ungroup() %>%
    mutate(Id_municipality = as.numeric(Id_municipality))
  
  temp1 <- bevoelkerung_gemeinde %>%
    filter(Id_district == district_number_f)
  
  temp1 <- temp1 %>%
    select(Id_municipality, name, Anzahl, Anzahl_rel)
  
  info_daten <- temp1 %>%
    right_join(info_daten, by = "Id_municipality")
  
  return(info_daten)
  
}

# Pendelwsk fuer Gemeinden
pendelwsk_berechnen <- function(daten_info, 
                                daten_distanzen,
                                bezirkswahl) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  # daten_distanzen <- distance_between_centre
  
  # Alles numerisch machen
  daten_distanzen$Id_from <- as.numeric(daten_distanzen$Id_from)
  daten_distanzen$Id_to <- as.numeric(daten_distanzen$Id_to)
  
  # Waehle relevanten Daten von Infos aus
  temp1 <- daten_info %>%
    select(Id_municipality, Anzahl_rel) 
  
  # temp2 <- daten_distanzen %>%
  #   mutate(distance_new = distance + distanz_wahl)
  
  # Umbenennen der Id-Spalten
  temp2 <-  daten_distanzen %>%
    rename(centre_i = Id_from, # centre_1 = Id_from
           centre_j = Id_to) # centre_2 = Id_to
  
  # Zusammen fuegen, dass wir relativen Bevoelkerung pro centre_j = Id_from haben
  daten_distanzen <- temp2 %>%
    left_join(temp1, by = c("centre_j" = "Id_municipality"))
  
  # Division berechnen
  daten_distanzen <- daten_distanzen %>%
    mutate(division = Anzahl_rel/distance)
  
  # Fuege nochmal alles hinzu
  temp3 <- temp2 %>%
    left_join(temp1, by = c("centre_j" = "Id_municipality"))
  
  # Jetzt Faktor c_i berechnen
  temp3 <- temp3 %>%
    group_by(centre_i) %>%
    summarise(c_i = sum(Anzahl_rel/distance)) 
  
  # Wieder joinen nur jetzt ueber centre_i
  daten_distanzen <- daten_distanzen %>%
    left_join(temp3, by = "centre_i")
  
  # WSK berechnen in Email von Prof. beschrieben
  daten_distanzen <- daten_distanzen %>%
    mutate(wsk = division/c_i)
  
  # Wichtige sachen auswaehlen
  daten_wsk <- daten_distanzen %>%
    select(centre_i, centre_j, wsk)
  
  return(daten_wsk)
  
}

#------------------------Agents erstellen------------------------

# Bevoelkerungszahlen altersklassen und Einzeljahre 
einlesen_und_bearbeite_bevoelkerlungszahlen <- function(path_altersklasse, 
                                                        path_einzeljahre,
                                                        auswahl){
  #TESTCODE
  # path_altersklasse <- "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_altersgruppen_geschlecht_und_bezirken_bzw._nuts_3-r.xlsx"
  # path_einzeljahre <- "/Users/danielkaras/Desktop/Masterarbeit/corona_modeling_austria/data/Bevölkerung_nach_Alter_und_Geschlecht/bevoelkerung_2019_nach_alter_in_einzeljahren_geschlecht_und_bundesland.xlsx"
  # auswahl <- district
  
  auswahl <- as.numeric(auswahl)
  
  daten_altersklassen <- 
    read_excel(path_altersklasse,
               skip = 3)
  
  daten_einzeljahre <- 
    read_excel(path_einzeljahre,
               skip = 2,
               col_types = c("text", rep("numeric", times = 10)))
  
  colnames(daten_altersklassen) <- 
    c("iso",
      "name",
      "zusammen_M",
      "unter_15_in_per_M",
      "15_bis_64_in_per_M",
      "64_und_aelter_M",
      "durchschnitts_Alter_M",
      "zusammen_W",
      "unter_15_in_per_W",
      "15_bis_64_in_per_W",
      "64_und_aelter_W",
      "durchschnitts_Alter_W")
  
  daten_altersklassen <- daten_altersklassen %>%
    filter(! is.na(name))
  
  colnames(daten_einzeljahre) <- gsub("\r\n", "",  colnames(daten_einzeljahre))
  colnames(daten_einzeljahre) <- gsub("-", "",  colnames(daten_einzeljahre))
  
  daten_altersklassen <- daten_altersklassen %>%
    filter(iso == auswahl) %>%
    mutate(gesamt_einwohner = sum(zusammen_M, zusammen_W), .after = "name")
  
  return(list(altersklasse = daten_altersklassen,
              einzeljahre = daten_einzeljahre))
}

# Maennlich, weibliche Agents generieren
geschlecht_erstellen <- function(daten_agent, daten_district, anzahl) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_district <- bevoelkerungs_zahlen_district 
  # anzahl <- anzahl_agents
  
  temp <- data.frame(sex = c("M", "W"),
                     Gewichtung = c(daten_district$zusammen_M, 
                                    daten_district$zusammen_W)) %>%
    sample_n(anzahl,
             weight = Gewichtung,
             replace = TRUE) %>%
    select(- Gewichtung)
  
  daten_agent <- daten_agent %>%
    bind_cols(temp) 
  
  return(daten_agent)
}

# Alter hinzufuegen
alter_erstellen <- function(daten_agent, daten_bevoelkerung, auswahl) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_bevoelkerung <- bevoelkerungszahlen
  # auswahl <- district
  
  temp <- bezirksdaten_sehen(speichern = TRUE)
  
  temp <- temp %>%
    filter(Id_district == as.numeric(auswahl))
  
  bundesland <- temp$bundesland
  
  dateiname <<- paste0(dateiname, "district", gsub(" ", "", temp$name), "_", bundesland)
  
  zahlen_district <- daten_bevoelkerung$altersklasse
  zahlen_einzeljahre <- daten_bevoelkerung$einzeljahre
  alter <- data.frame(alter = c(),
                      Id_agent = c())
  
  for (geschlecht in unique(daten_agent$sex)) {
    
    agents_geschlecht <- daten_agent %>%
      filter(sex == geschlecht)
    
    anzahl_geschlecht <- length(agents_geschlecht$Id_agent)
    
    if (geschlecht == "M") {
      
      altersklasse <- data.frame(altersklasse = c("<15", "15-64", "+64"),
                                 Gewichtung = c(zahlen_district$unter_15_in_per_M,
                                                zahlen_district$`15_bis_64_in_per_M`,
                                                zahlen_district$`64_und_aelter_M`)) %>%
        sample_n(anzahl_geschlecht,
                 weight = Gewichtung,
                 replace = TRUE)
      
    } else {
      
      altersklasse <- data.frame(altersklasse = c("<15", "15-64", "+64"),
                                 Gewichtung = c(zahlen_district$unter_15_in_per_W,
                                                zahlen_district$`15_bis_64_in_per_W`,
                                                zahlen_district$`64_und_aelter_W`)) %>%
        sample_n(anzahl_geschlecht,
                 weight = Gewichtung,
                 replace = TRUE)
      
    }
    
    altersklasse$Id_agent <- sample(agents_geschlecht$Id_agent, 
                                    size = anzahl_geschlecht)
    
    
    anzahl_alltersklassen <- altersklasse %>%
      group_by(altersklasse) %>%
      summarise(Anzahl = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = altersklasse, values_from = Anzahl)
    
    # Innerhlab den Altersgruppen Jahren nach Gewichtung des Alters auf Bundeslandebene
    if (geschlecht == "M") {
      
      zahlen_einzeljahre_bundesland <- zahlen_einzeljahre %>%
        select(c("Geschlecht, Alter inEinzeljahren", bundesland)) %>%
        slice((match("Männer", zahlen_einzeljahre$`Geschlecht, Alter inEinzeljahren`) + 1) : (match("Männer", zahlen_einzeljahre$`Geschlecht, Alter inEinzeljahren`) + 101)) %>%
        rename(federal_state = !! sym(bundesland))
      
    } else {
      
      zahlen_einzeljahre_bundesland <- zahlen_einzeljahre %>%
        select(c("Geschlecht, Alter inEinzeljahren", bundesland)) %>%
        slice((match("Frauen", zahlen_einzeljahre$`Geschlecht, Alter inEinzeljahren`) + 1) : (match("Frauen", zahlen_einzeljahre$`Geschlecht, Alter inEinzeljahren`) + 101)) %>%
        rename(federal_state = !! sym(bundesland))
      
    }
    
    unter15 <- data.frame(alter = 0:14,
                          Gewichtung = zahlen_einzeljahre_bundesland$federal_state[1:15]) %>%
      sample_n(anzahl_alltersklassen$`<15`,
               weight = Gewichtung,
               replace = TRUE)
    
    unter15$Id_agent <- sample(filter(altersklasse, altersklasse == "<15")$Id_agent,
                               anzahl_alltersklassen$`<15`)
    
    zwischen15und64 <- data.frame(alter = 15:64,
                                  Gewichtung = zahlen_einzeljahre_bundesland$federal_state[16:65]) %>%
      sample_n(anzahl_alltersklassen$`15-64`,
               weight = Gewichtung,
               replace = TRUE)
    
    zwischen15und64$Id_agent <- sample(filter(altersklasse, altersklasse == "15-64")$Id_agent,
                                       anzahl_alltersklassen$`15-64`)
    
    ueber64 <- data.frame(alter = 65:100,
                          Gewichtung = zahlen_einzeljahre_bundesland$federal_state[66:101]) %>%
      sample_n(anzahl_alltersklassen$`+64`,
               weight = Gewichtung,
               replace = TRUE)
    
    ueber64$Id_agent <- sample(filter(altersklasse, altersklasse == "+64")$Id_agent,
                               anzahl_alltersklassen$`+64`)
    
    alter_geschlect <- unter15 %>%
      bind_rows(zwischen15und64) %>%
      bind_rows(ueber64) %>%
      select(- Gewichtung)
    
    alter <- alter %>%
      bind_rows(alter_geschlect)
    
  }
  
  daten_agent <- daten_agent %>%
    full_join(alter, by = "Id_agent")
  
  return(daten_agent)
  
}

# Zugehoerige Gemeinde waehlen
gemeinde_waehlen <- function(daten_agent, daten_info, anzahl) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  # daten_agent <- agents
  # anzahl <- anzahl_agents
  
  
  temp <- daten_info %>%
    select(Id_municipality, Anzahl) %>%
    sample_n(anzahl,
             weight = Anzahl,
             replace = TRUE) %>%
    select(- Anzahl)
  
  daten_agent <- daten_agent %>%
    bind_cols(temp) 
  
  return(daten_agent)
  
}

# Haushaltszahlen einlesen
einlesen_und_bearbeite_haushaltszahlen <- function(path_haushalt){
 
  #TESTCODE
  # path_haushalt <- "data/Haushalte_pro_Bundesland/privathaushalte_nach_haushaltsgroesse_bundeslaendern_und_alter_der_haushal.xlsx"
  
  haushalts_daten <- read_excel(path_haushalt,
                                skip = 7)
  
  colnames(haushalts_daten) <- c("Bundesland",
                                 "gesamt",
                                 "gesamt_1",
                                 "M_1",
                                 "W_1",
                                 "gesamt_mehr",
                                 "2",
                                 "3",
                                 "4",
                                 ">4",
                                 "durchschnitt")
  
  haushalts_daten <- haushalts_daten %>%
    filter(! is.na(gesamt))
  
  haushalts_daten_bundesland <- haushalts_daten %>%
    slice(1:9)
  
  haushalts_daten_jahre <- haushalts_daten %>%
    slice(10:24) %>%
    rename(Jahre = Bundesland)
  
  return(list(haushalts_daten_bundesland = haushalts_daten_bundesland, 
              haushalts_daten_jahre = haushalts_daten_jahre))
   
}

# Haushaltsanzahl pro Gemeinde erstellen 
haushaltsanzahl_erstellen <- function(daten_info, daten_haushalt_bundesland, daten_agents,
                                      district_number_f) {
  
  #TESTCODE 
  # daten_info <- infos_mumicipality
  # daten_haushalt_bundesland <- haushalts_gesamt_daten$haushalts_daten_bundesland
  # daten_agents <- agents
  # district_number_f <- district_number
  
  temp1 <- bezirksdaten_sehen(speichern = TRUE)
  
  temp1 <- temp1 %>%
    filter(Id_district == as.numeric(district_number_f))
  
  bundesland <- temp1$bundesland
  
  haushalte_ein_bundesland <- daten_haushalt_bundesland %>%
    filter(Bundesland == bundesland) %>%
    select(c("M_1",
             "W_1",
             "2",
             "3",
             "4",
             ">4"))
  
  haushalte_gesamt <- data.frame(Id_municipality = c(),
                                 M_1 = c(),
                                 W_1 = c(),
                                 "2" = c(),
                                 "3" = c(),
                                 "4" = c(),
                                 ">4" = c())
  
  one_mumicipality <- daten_agents %>%
    group_by(Id_municipality) %>%
    summarise(Anzahl = n()) %>%
    ungroup()
  
  for (id in daten_info$Id_municipality) {
    
    # Test fuer eine Gemeinde
    anzahl_personen <- one_mumicipality %>%
      filter(Id_municipality == id) 
    
    anzahl_personen <- anzahl_personen$Anzahl
    
    haushalte <- data.frame(Haushalts_art = c("M_1",
                                              "W_1",
                                              "2",
                                              "3",
                                              "4",
                                              ">4"),
                            Anzahl = c(0, 0, 0, 0, 0, 0))
    
    while(sum(haushalte$Anzahl * c(1,1,2,3,4,5)) < anzahl_personen) {
      
      temp2 <- sample(haushalte$Haushalts_art, 1, prob = haushalte_ein_bundesland)
      
      haushalte <- haushalte %>%
        mutate(Anzahl = if_else(Haushalts_art == temp2, Anzahl + 1, Anzahl))
      
    }
    
    haushalte <- haushalte %>%
      pivot_wider(names_from = Haushalts_art, values_from = Anzahl) %>%
      mutate(Id_municipality = id, .before = M_1)
    
    haushalte_gesamt <- haushalte_gesamt %>%
      bind_rows(haushalte)
    
    cat(paste0("Gemeinde mit folgender ID ist fertig: ", id, "\n"))
    
  }
  
  daten_info <- daten_info %>%
    full_join(haushalte_gesamt, by = "Id_municipality")
  
  return(daten_info)
  
}

# Haushalte erstellen bezueglich Gemeinden
haushalte_auf_gemeindeebene_erstellen <- function(daten_agent, daten_info) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_info <- infos_mumicipality
  
  dateiname <<- paste0(dateiname, "_household")
  
  infos_haushalte_longer <- daten_info %>%
    select(- c(name, Anzahl, Anzahl_rel)) %>%
    pivot_longer(! Id_municipality, names_to = "Haushaltsart", values_to = "Anzahl")
  
  infos_haushalte_longer <- infos_haushalte_longer %>%
    group_by(Id_municipality) %>%
    mutate(kum_Summe = cumsum(Anzahl)) %>%
    ungroup()
  
  id_haushalte <- data.frame(Id_municipality = c(),
                             Haushaltsart = c(),
                             Id_household = c())
  
  for (id in daten_info$Id_municipality) {
    
    temp1 <- infos_haushalte_longer %>%
      filter(Id_municipality == id)
    
    temp2 <- data.frame(Id_municipality = id,
                        Haushaltsart = rep(c("M_1", "W_1", "2", "3", "4", "5"), 
                                           temp1$Anzahl * c(1,1,2,3,4,5)),
                        Id_household = rep(c(1:temp1$kum_Summe[1],
                                             (temp1$kum_Summe[1] + 1) : temp1$kum_Summe[2],
                                             (temp1$kum_Summe[2] + 1) : temp1$kum_Summe[3],
                                             (temp1$kum_Summe[3] + 1) : temp1$kum_Summe[4],
                                             (temp1$kum_Summe[4] + 1) : temp1$kum_Summe[5],
                                             (temp1$kum_Summe[5] + 1) : temp1$kum_Summe[6]),
                                           rep(c(1,1,2,3,4,5),
                                               temp1$Anzahl)))
    
    id_haushalte <- id_haushalte %>%
      bind_rows(temp2)
    
  }
  
  id_haushalte_zu_agents <- data.frame(Id_agent = c(),
                                       Id_household = c())
  
  for (id in daten_info$Id_municipality) {
    
    id_haushalte_eine_municipalty <- id_haushalte %>%
      filter(Id_municipality == id)
    
    infos_mumicipality_one_municipality <- daten_info %>%
      filter(Id_municipality == id)
    
    agents_one_municipality <- daten_agent %>%
      filter(Id_municipality == id)
    
    temp1 <- agents_one_municipality %>%
      filter(alter > 18 & sex == "M")
    
    temp2 <- data.frame(Id_agent = sample(temp1$Id_agent, infos_mumicipality_one_municipality$M_1),
                        Id_household = filter(id_haushalte_eine_municipalty, Haushaltsart == "M_1")$Id_household)
    
    id_haushalte_zu_agents <- id_haushalte_zu_agents %>%
      bind_rows(temp2)
    
    temp1 <- agents_one_municipality %>%
      filter(alter > 18 & sex == "W")
    
    temp2 <- data.frame(Id_agent = sample(temp1$Id_agent, infos_mumicipality_one_municipality$W_1),
                        Id_household = filter(id_haushalte_eine_municipalty, Haushaltsart == "W_1")$Id_household)
    
    id_haushalte_zu_agents <- id_haushalte_zu_agents %>%
      bind_rows(temp2)
    
    agents_one_municipality <- agents_one_municipality %>%
      filter(! (Id_agent %in% id_haushalte_zu_agents$Id_agent))
    
    id_haushalte_eine_municipalty <- id_haushalte_eine_municipalty %>%
      filter(! (Haushaltsart %in% c("M_1", "W_1")))
    
    temp1 <- agents_one_municipality %>%
      filter(alter > 18)
    
    temp2 <- data.frame(Id_agent = sample(temp1$Id_agent, nrow(distinct(id_haushalte_eine_municipalty))),
                        Id_household = unique(id_haushalte_eine_municipalty$Id_household))
    
    id_haushalte_zu_agents <- id_haushalte_zu_agents %>%
      bind_rows(temp2)
    
    agents_one_municipality <- agents_one_municipality %>%
      filter(! (Id_agent %in% id_haushalte_zu_agents$Id_agent))
    
    id_haushalte_eine_municipalty <- id_haushalte_eine_municipalty %>%
      group_by(Id_municipality, Haushaltsart, Id_household) %>%
      slice(2:n()) %>%
      ungroup()
    
    temp2 <- data.frame(Id_agent = sample(agents_one_municipality$Id_agent, nrow(agents_one_municipality)),
                        Id_household = sample(id_haushalte_eine_municipalty$Id_household, nrow(agents_one_municipality)))
    
    id_haushalte_zu_agents <- id_haushalte_zu_agents %>%
      bind_rows(temp2) %>%
      arrange(Id_agent)
    
  }
  
  daten_agent <- daten_agent %>%
    right_join(id_haushalte_zu_agents, by = "Id_agent")
  
  return(daten_agent)
  
}

# Einlesen und bearbeiten Arbeitsplatzdaten
einlesen_und_bearbeite_arbeitsplatzdaten <- function(path_arbeitsplatz) {
  
  #TESTCODE
  # path_arbeitsplatz <- "data/Arbeitsstaetten/gemeindeergebnisse_der_abgestimmten_erwerbsstatistik_und_arbeitsstaettenza.xlsx"
  
  daten_workplace_raw <- read_excel(path_arbeitsplatz)
  
  colnames(daten_workplace_raw) <- daten_workplace_raw[3,]
  colnames(daten_workplace_raw)[1:2] <- c("iso", "name")
  colnames(daten_workplace_raw) <- gsub(" ", "", colnames(daten_workplace_raw))
  colnames(daten_workplace_raw) <- gsub("\n", "", colnames(daten_workplace_raw))
  colnames(daten_workplace_raw) <- gsub("\r", "", colnames(daten_workplace_raw))
  
  daten_workplace_raw <- daten_workplace_raw %>%
    filter(! is.na(name)) %>%
    filter(! is.na(AnteilderPersonenunter15Jahren)) %>%
    mutate(iso = as.numeric(iso))
  
  daten_workplace_raw <- daten_workplace_raw %>%
    filter(nchar(iso) > 1)
  
  temp1 <- daten_workplace_raw %>%
    filter(nchar(iso) == 5) %>%
    mutate(iso_district = substr(iso, 1, 3))
  
  temp2 <- daten_workplace_raw %>%
    filter(nchar(iso) == 3) %>%
    filter(! (iso %in% unique(temp1$iso_district))) %>%
    mutate(iso = iso * 100 + 1)
  
  temp1 <- temp1 %>%
    select(- iso_district)
  
  daten_workplace <- temp1 %>%
    bind_rows(temp2) %>%
    arrange(iso)
  
  return(daten_workplace)
  
}

# Hinzufuegen der wihtigen Daten pro Bezirk
arbeitsplatzdaten_hinzufuegen <- function(daten_info, daten_arbeitsplatz) {
  
  #TESTCODE 
  # daten_info <- infos_mumicipality
  # daten_arbeitsplatz <- arbeitsplatz_daten
  
  daten_arbeitsplatz <- daten_arbeitsplatz %>%
    select(iso, `AnteilderAuspendler/-innenandenaktivErwerbs-tätigenamWohnort`, `Arbeitslosenquote(15Jahreundälter)`, Arbeitsstätten, BeschäftigteindenArbeitsstätten) %>%
    rename(Id_municipality = iso) %>%
    mutate_all(as.numeric)
  
  daten_info <- daten_info %>%
    left_join(daten_arbeitsplatz, by = "Id_municipality")
  
  return(daten_info)
  
}

# Moegliche Arbeitsplaetze erstellen
arbeitsplaetze_erstellen <- function(daten_info) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  
  # moegliche Arbeitsplaetze erstellen
  moegliche_arbeitsplaetze <- data.frame(Id_municipality = c(),
                                         Arbeitsstaette = c())
  
  for (id in daten_info$Id_municipality) {
    
    temp1 <- daten_info %>%
      filter(Id_municipality == id) %>%
      select(Id_municipality, Arbeitsstätten, BeschäftigteindenArbeitsstätten) 
    
    temp2 <- data.frame(Id_municipality = id,
                        Arbeitsstaette = sample(1:temp1$Arbeitsstätten, temp1$BeschäftigteindenArbeitsstätten, replace = TRUE))
    
    moegliche_arbeitsplaetze <- moegliche_arbeitsplaetze %>%
      bind_rows(temp2)
    
  }
  
  moegliche_arbeitsplaetze$Id_arbeitsplatz <- 1:nrow(moegliche_arbeitsplaetze)
  
  return(moegliche_arbeitsplaetze)
  
}

# Arbeit pro Agent erstellen
arbeit_zuweisen <- function(daten_agent, daten_info) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_info <- infos_municipitality
  
  daten_agent <- daten_agent %>%
    mutate(type_of_work = "work",
           Id_municipality_work = Id_municipality,
           Id_workplace = 0)
  
  temp1 <- daten_info %>%
    select(Id_municipality, `Arbeitslosenquote(15Jahreundälter)`) %>%
    rename(Arbeitslosenquote = `Arbeitslosenquote(15Jahreundälter)`)
  
  daten_agent <- daten_agent %>%
    left_join(temp1, by = "Id_municipality") %>%
    mutate(prob_unemployed = runif(n()) * 100)
  
  daten_agent <- daten_agent %>%
    mutate(type_of_work = if_else(prob_unemployed <= Arbeitslosenquote & alter > 18 & alter < 65, 
                                  "unemployed", 
                                  type_of_work)) %>%
    select(- c(prob_unemployed, Arbeitslosenquote))
  
  # Agents in Schule, Pension usw schicken
  daten_agent <- daten_agent %>%
    mutate(type_of_work = if_else(alter >= 65, "pension", type_of_work)) %>%
    mutate(type_of_work = if_else(alter > 6 & alter <= 18, "school", type_of_work)) %>%
    mutate(type_of_work = if_else(alter >= 4 & alter <= 6, "kindergarden", type_of_work)) %>%
    mutate(type_of_work = if_else(alter < 4, "child", type_of_work))
  
  return(daten_agent)
  
}

# Arbeitsplaetze zuweisen 
arbeitsplaetze_zuweisen <- function(daten_agent, daten_info, moegliche_arbeitsplaetze, daten_wsk) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  # daten_agent <- agents
  # moegliche_arbeitsplaetze <- arbeitsplaetze
  # daten_wsk <- wsk_between_centre
  
  dateiname <<- paste0(dateiname, "_workplace")
  
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
    
    wsk_zu_gemeinde <- daten_wsk %>%
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
    
    if ((i %% 100) == 0 | i == nrow(wo_arbeiten_district)) {
      
      cat(paste0(i, " von ", nrow(wo_arbeiten_district), " Agents haben einen freien Arbeitsplatz! \n"))
      
    }
    
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
  
  return(daten_agent)
  
}

# Schuldaten hinzufuegen
schuldaten_hinzufuegen <- function(daten_info) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  
  file_pupils <- "schuelerinnen_und_schueler_202021_nach_gemeinden_und_schultypen.xlsx"
  file_schools <- "schulen_202021_nach_gemeinden_und_schultypen.xlsx"
  
  setwd("./data/Schulen_und_Kindergarten")
  
  pupils_raw <- read_excel(file_pupils, skip = 3)
  schools_raw <- read_excel(file_schools, skip = 3)
  
  setwd("..")
  setwd("..")
  
  colnames(schools_raw)[1:4] <- c("iso", "name", "gesamt", "Volksschule")
  colnames(pupils_raw)[1:4] <- c("iso", "name", "gesamt", "Volksschule_Schueler")
  
  pupils_raw <- pupils_raw %>%
    filter(iso != "alle") %>%
    filter(! is.na(name))
  
  schools_raw <- schools_raw %>%
    filter(iso != "alle") %>%
    filter(! is.na(name))
  
  pupils_raw <- pupils_raw %>%
    mutate(Unter_Oberstufe_Schueler = rowSums(across(5:18))) %>%
    select(c("iso", "Volksschule_Schueler", "Unter_Oberstufe_Schueler"))
  
  schools_raw <- schools_raw %>%
    mutate(Unter_Oberstufe = rowSums(across(5:18))) %>%
    select(c("iso", "Volksschule", "Unter_Oberstufe"))
  
  schools_raw <- schools_raw %>%
    mutate_all(as.numeric)
  
  pupils_raw <- pupils_raw %>%
    mutate_all(as.numeric)
  
  daten_info <- daten_info %>%
    left_join(schools_raw, by = c("Id_municipality" = "iso")) %>%
    left_join(pupils_raw, by = c("Id_municipality" = "iso"))
  
  return(daten_info)
  
}

# Volksschulplaetze erstellen
volksschulplaetze_erstellen <- function(daten_info) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  
  moegliche_volksschulplaetze <- data.frame(Id_municipality = c(),
                                            Id_volksschule = c())
  
  for (id in daten_info$Id_municipality) {
    
    temp1 <- daten_info %>%
      filter(Id_municipality == id) %>%
      select(Id_municipality, Volksschule, Volksschule_Schueler) 
    
    if (temp1$Volksschule != 0) {
      
      temp2 <- data.frame(Id_municipality = id,
                          Id_volksschule = sample(1:temp1$Volksschule, temp1$Volksschule_Schueler, replace = TRUE))
      
      moegliche_volksschulplaetze <- moegliche_volksschulplaetze %>%
        bind_rows(temp2)
      
    }
    
  }
  
  moegliche_volksschulplaetze$Id_schulplatz <- 1:nrow(moegliche_volksschulplaetze)
  
  return(moegliche_volksschulplaetze)
  
}

# Volksschulen zuweisen
volksschulen_zuweisen <- function(daten_agent, daten_info, daten_volkschulplaetze, daten_wsk) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_info <- infos_mumicipality
  # daten_volkschulplaetze <- volksschulplaetze
  # daten_wsk <- wsk_between_centre
  
  dateiname <<- paste0(dateiname, "_primarySchool")
  
  daten_agent_volksschule <- daten_agent %>%
    filter(alter >= 7 & alter <= 10) %>% 
    mutate(type_of_work = "primary_school")
  
  for (id in daten_info$Id_municipality) {
    
    schueler <- daten_agent_volksschule %>%
      filter(Id_municipality == id)
    
    moegliche_plaetze <- daten_volkschulplaetze %>%
      filter(Id_municipality == id)
    
    if (nrow(schueler) <= nrow(moegliche_plaetze)) {
      
      temp1 <- moegliche_plaetze %>%
        sample_n(nrow(schueler))
      
      schueler$Id_workplace <- temp1$Id_volksschule
      
    } else {
      
      temp1 <- moegliche_plaetze %>%
        sample_n(nrow(moegliche_plaetze))
      
      schueler$Id_workplace <- c(temp1$Id_volksschule, rep(0, nrow(schueler) - nrow(moegliche_plaetze)))
      
    }
    
    daten_volkschulplaetze <- daten_volkschulplaetze %>%
      anti_join(temp1, by = "Id_schulplatz")
    
    schueler <- schueler %>%
      select(Id_agent, type_of_work,  Id_workplace) %>%
      rename(Id_workplace_new = Id_workplace,
             type_of_work_new = type_of_work)
    
    daten_agent <- daten_agent %>%
      left_join(schueler, by = "Id_agent") %>%
      mutate(type_of_work = if_else(is.na(type_of_work_new), type_of_work, type_of_work_new),
             Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new))) %>%
      select(- c(type_of_work_new, Id_workplace_new))
    
  }
  
  daten_agent_volksschule_district <- daten_agent %>%
    filter(type_of_work == "primary_school" & Id_workplace == 0) 
  
  anzahl_plaetze <- nrow(daten_volkschulplaetze)
  anzahl_schueler <- nrow(daten_agent_volksschule_district)
  
  if (anzahl_schueler <= anzahl_plaetze) {
    anzahl <- anzahl_schueler
  } else {
    anzahl <- anzahl_plaetze
  }
  
  for (j in 1:anzahl) {
    
    temp2 <- daten_wsk %>%
      filter(centre_j %in% unique(daten_volkschulplaetze$Id_municipality))
    
    ein_schueler <- daten_agent_volksschule_district %>%
      sample_n(1) %>%
      select(Id_agent, Id_municipality) %>%
      left_join(temp2, by = c("Id_municipality" = "centre_i")) 
    
    ein_schueler <- ein_schueler %>%
      left_join(daten_volkschulplaetze, by = c("centre_j" = "Id_municipality")) %>%
      sample_n(1, weight = wsk)
    
    daten_volkschulplaetze <- daten_volkschulplaetze %>%
      anti_join(ein_schueler, by = "Id_schulplatz")
    
    daten_agent_volksschule_district <- daten_agent_volksschule_district %>%
      anti_join(ein_schueler, by = "Id_agent")
    
    ein_schueler <- ein_schueler %>%
      select(Id_agent, Id_volksschule, centre_j) %>%
      rename(Id_workplace_new = Id_volksschule,
             Id_municipality_new = centre_j)
    
    daten_agent <- daten_agent %>%
      left_join(ein_schueler, by = "Id_agent") %>%
      mutate(Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new)),
             Id_municipality_work = if_else(is.na(Id_municipality_new), Id_municipality_work, as.double(Id_municipality_new))) %>%
      select(- c(Id_municipality_new, Id_workplace_new))
    
    if ((j %% 100) == 0 | j == anzahl) {
      
      cat(paste0(j, " von ", anzahl, " Agents haben einen freien Volksschulplatz! \n"))
      
    }
  }
  
  daten_agent <- daten_agent %>%
    mutate(Id_municipality_work = if_else(type_of_work == "primary_school" & Id_workplace == 0, 99999, Id_municipality_work))
  
  return(daten_agent)
  
}

# Schulplaetze erstellen
schulplaetze_erstellen <- function(daten_info) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  
  moegliche_schulplaetze <- data.frame(Id_municipality = c(),
                                       Id_schule = c())
  
  for (id in daten_info$Id_municipality) {
    
    temp1 <- daten_info %>%
      filter(Id_municipality == id) %>%
      select(Id_municipality, Unter_Oberstufe, Unter_Oberstufe_Schueler) 
    
    if (temp1$Unter_Oberstufe != 0) {
      
      temp2 <- data.frame(Id_municipality = id,
                          Id_schule = sample(1:temp1$Unter_Oberstufe, temp1$Unter_Oberstufe_Schueler, replace = TRUE))
      
      moegliche_schulplaetze <- moegliche_schulplaetze %>%
        bind_rows(temp2)
      
    }
    
  }
  
  moegliche_schulplaetze$Id_schulplatz <- 1:nrow(moegliche_schulplaetze)
  
  return(moegliche_schulplaetze)
  
}

# Schulen zuweisen
schulen_zuweisen <- function(daten_agent, daten_info, daten_schulplaetze, daten_wsk) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_info <- infos_mumicipality
  # daten_schulplaetze <- schulplaetze
  # daten_wsk <- wsk_between_centre
  
  if (str_detect(dateiname, "primarySchool")) {
    
    dateiname <<- str_replace(dateiname, "primarySchool", "School")
    
  } else {
    
    dateiname <<- paste0(dateiname, "_highSchool")
    
  }
  
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
  
  return(daten_agent)
  
}

# Kindergartendaten hinzufuegen
kindergartendaten_hinzufuegen <- function(daten_info) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  
  file_kindergarden <- "kindertagesheime_gruppen_und_kinder_nach_gemeinden_2020.xlsx"
  
  setwd("./data/Schulen_und_Kindergarten")
  
  kindergarden_raw <- read_excel(file_kindergarden, skip = 3)
  
  setwd("..")
  setwd("..")
  
  colnames(kindergarden_raw)[1:2] <- c("iso", "name")
  
  kindergarden_raw <- kindergarden_raw %>%
    filter(! is.na(iso)) %>%
    filter(! is.na(name)) %>%
    select(1:6, 15:18)
  
  kindergarden_raw <- kindergarden_raw %>%
    mutate(Kindergarten = rowSums(across(3:6), na.rm = TRUE)) %>%
    mutate(Kindergarten_Kinder = rowSums(across(7:10), na.rm = TRUE)) %>%
    select(- c(2:10)) 
  
  kindergarden_raw <- kindergarden_raw %>%
    mutate_all(as.numeric)
  
  daten_info <- daten_info %>%
    left_join(kindergarden_raw, by = c("Id_municipality" = "iso")) %>%
    mutate(Kindergarten = if_else(is.na(Kindergarten), 0, Kindergarten)) %>%
    mutate(Kindergarten_Kinder = if_else(is.na(Kindergarten_Kinder), 0, Kindergarten_Kinder))
  
  return(daten_info)
  
}

# Kindergartenplaetze erstellen
kindergartenplaetze_erstellen <- function(daten_info) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  
  moegliche_kindergartenplaetze <- data.frame(Id_municipality = c(),
                                              Id_kindergarten = c())
  
  for (id in daten_info$Id_municipality) {
    
    temp1 <- daten_info %>%
      filter(Id_municipality == id) %>%
      select(Id_municipality, Kindergarten, Kindergarten_Kinder) 
    
    if (temp1$Kindergarten != 0) {
      
      temp2 <- data.frame(Id_municipality = id,
                          Id_kindergarten = sample(1:temp1$Kindergarten, temp1$Kindergarten_Kinder, replace = TRUE))
      
      moegliche_kindergartenplaetze <- moegliche_kindergartenplaetze %>%
        bind_rows(temp2)
      
    }
    
  }
  
  moegliche_kindergartenplaetze$Id_kindergartenplatz <- 1:nrow(moegliche_kindergartenplaetze)
  
  return(moegliche_kindergartenplaetze)
  
}

# Kindergarten zuweisen
kindergarten_zuweisen <- function(daten_agent, daten_info, daten_kindergartenplaetze, daten_wsk) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_info <- infos_mumicipality
  # daten_kindergartenplaetze <- kindergartenplaetze
  # daten_wsk <- wsk_between_centre
  
  dateiname <<- paste0(dateiname, "_kindergarden")
  
  daten_agent_kindergarden <- daten_agent %>%
    filter(alter >= 4 & alter <= 6) %>% 
    mutate(type_of_work = "kindergarden")
  
  for (id in daten_info$Id_municipality) {
    
    kinder <- daten_agent_kindergarden %>%
      filter(Id_municipality == id)
    
    moegliche_plaetze <- daten_kindergartenplaetze %>%
      filter(Id_municipality == id)
    
    if (nrow(kinder) <= nrow(moegliche_plaetze)) {
      
      temp1 <- moegliche_plaetze %>%
        sample_n(nrow(kinder))
      
      kinder$Id_workplace <- temp1$Id_kindergarten
      
    } else {
      
      temp1 <- moegliche_plaetze %>%
        sample_n(nrow(moegliche_plaetze))
      
      kinder$Id_workplace <- c(temp1$Id_kindergarten, rep(0, nrow(kinder) - nrow(moegliche_plaetze)))
      
    }
    
    daten_kindergartenplaetze <- daten_kindergartenplaetze %>%
      anti_join(temp1, by = "Id_kindergartenplatz")
    
    kinder <- kinder %>%
      select(Id_agent, type_of_work,  Id_workplace) %>%
      rename(Id_workplace_new = Id_workplace,
             type_of_work_new = type_of_work)
    
    daten_agent <- daten_agent %>%
      left_join(kinder, by = "Id_agent") %>%
      mutate(type_of_work = if_else(is.na(type_of_work_new), type_of_work, type_of_work_new),
             Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new))) %>%
      select(- c(type_of_work_new, Id_workplace_new))
    
  }
  
  daten_agent_kindergarten_district <- daten_agent %>%
    filter(type_of_work == "kindergarden" & Id_workplace == 0) 
  
  anzahl_plaetze <- nrow(daten_kindergartenplaetze)
  anzahl_kinder <- nrow(daten_agent_kindergarten_district)
  
  if (anzahl_kinder <= anzahl_plaetze) {
    anzahl <- anzahl_kinder
  } else {
    anzahl <- anzahl_plaetze
  }
  
  for (j in 1:anzahl) {
    
    temp2 <- daten_wsk %>%
      filter(centre_j %in% unique(daten_kindergartenplaetze$Id_municipality))
    
    ein_kind <- daten_agent_kindergarten_district %>%
      sample_n(1) %>%
      select(Id_agent, Id_municipality) %>%
      left_join(temp2, by = c("Id_municipality" = "centre_i")) 
    
    ein_kind <- ein_kind %>%
      left_join(daten_kindergartenplaetze, by = c("centre_j" = "Id_municipality")) %>%
      sample_n(1, weight = wsk)
    
    daten_kindergartenplaetze <- daten_kindergartenplaetze %>%
      anti_join(ein_kind, by = "Id_kindergartenplatz")
    
    daten_agent_kindergarten_district <- daten_agent_kindergarten_district %>%
      anti_join(ein_kind, by = "Id_agent")
    
    ein_kind <- ein_kind %>%
      select(Id_agent, Id_kindergarten, centre_j) %>%
      rename(Id_workplace_new = Id_kindergarten,
             Id_municipality_new = centre_j)
    
    daten_agent <- daten_agent %>%
      left_join(ein_kind, by = "Id_agent") %>%
      mutate(Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new)),
             Id_municipality_work = if_else(is.na(Id_municipality_new), Id_municipality_work, as.double(Id_municipality_new))) %>%
      select(- c(Id_municipality_new, Id_workplace_new))
    
    if ((j %% 100) == 0 | j == anzahl) {
      
      cat(paste0(j, " von ", anzahl, " Agents haben einen freien Kindergartenplatz! \n"))
      
    }
  }
  
  daten_agent_kindergarden_jung <- daten_agent %>%
    filter(alter %in% c(2,3))
  
  anzahl <- nrow(daten_kindergartenplaetze)
  
  for (j in 1:anzahl) {
    
    temp2 <- daten_wsk %>%
      filter(centre_j %in% unique(daten_kindergartenplaetze$Id_municipality))
    
    ein_kind <- daten_agent_kindergarden_jung %>%
      sample_n(1) %>%
      select(Id_agent, Id_municipality) %>%
      left_join(temp2, by = c("Id_municipality" = "centre_i")) 
    
    ein_kind <- ein_kind %>%
      left_join(daten_kindergartenplaetze, by = c("centre_j" = "Id_municipality")) %>%
      sample_n(1, weight = wsk)
    
    daten_kindergartenplaetze <- daten_kindergartenplaetze %>%
      anti_join(ein_kind, by = "Id_kindergartenplatz")
    
    daten_agent_kindergarden_jung <- daten_agent_kindergarden_jung %>%
      anti_join(ein_kind, by = "Id_agent")
    
    ein_kind <- ein_kind %>%
      select(Id_agent, Id_kindergarten, centre_j) %>%
      rename(Id_workplace_new = Id_kindergarten,
             Id_municipality_new = centre_j) %>%
      mutate(type_of_work_new = "kindergarden")
    
    daten_agent <- daten_agent %>%
      left_join(ein_kind, by = "Id_agent") %>%
      mutate(Id_workplace = if_else(is.na(Id_workplace_new), Id_workplace, as.double(Id_workplace_new)),
             Id_municipality_work = if_else(is.na(Id_municipality_new), Id_municipality_work, as.double(Id_municipality_new)),
             type_of_work = if_else(is.na(type_of_work_new), type_of_work, type_of_work_new)) %>%
      select(- c(Id_municipality_new, Id_workplace_new, type_of_work_new))
    
    if ((j %% 100) == 0 | j == anzahl) {
      
      cat(paste0(j, " von ", anzahl, " Agents haben einen freien Kindergartenplatz! \n"))
      
    }
  }
  
  return(daten_agent)
  
}

#----------------------------------------------------------------
#----------------------------------------------------------------
#------------------Triangulierung (DEPRECATED)-------------------
# Koordinaten aus Rohdaten auswaehlen auf district Ebene
koordinaten_auswaehlen <- function(genauigkeit, auswahl, daten) {
  
  #TESTCODE
  # genauigkeit <- "district"
  # auswahl <- "Rust" # bei federal state = Zahl, district = name
  # daten <- geo_raw_data_for_districts
  
  dateiname <<- paste0(dateiname, genauigkeit, auswahl)
  
  switch(genauigkeit,
         "federal state" = {
           coordinates <- c()
         },
         "district" = {
           
           # ERWEITERUNG FUER DISTRICTS DIE ZWEI TEILE HABEN
           
           indices <- which(daten$features$properties$name == auswahl)
           
           iso <- as.numeric(daten$features$properties$iso[indices])
           
           coordinates <- daten$features$geometry$coordinates[[indices]]
           
           laenge <- (length(coordinates)/2)
           
           X <- coordinates[1: laenge]
           Y <- coordinates[(laenge + 1): (laenge * 2)]
           
           coordinates <- data.frame(name = rep(auswahl, laenge),
                                     Id_district = rep(iso, laenge),
                                     Id_point = seq(1, laenge),
                                     X = X, 
                                     Y = Y)
           
           #TESTPLOT
           # ggplot(coordinates) +
           #   geom_polygon(aes(X,Y))
           
         }
         )
  
  return(coordinates)
  
  #ERWEITERUNG AUF FEDERAL STATE -> BERUECKSICHTIGEN DER VERSCHIEDENEN BEZIRKE (LOECHER USW)
  # "federal state" = {
  #   
  #   indices <- 
  #     which(as.numeric(substr(daten$features$properties$iso,1,1)) == auswahl)
  #   
  #   coordinates <- c()
  #   
  #   for (i in indices) {
  #     
  #     temp <- daten$features$geometry$coordinates[[i]]
  #     
  #     coordinates <- cbind(coordinates, temp)
  #     
  #   }
}

# Triangulierung durchfuehren -> Daten muessen immer X und Y Koordinaten enthalten
triangulierung <- function(daten) {
  
  #TESTCODE
  # daten <- coordinates_districts
  
  daten <- daten %>%
    select(c(X,Y))
  
  triangulierung <- decido::earcut(daten)
  
  Punkte_1 <- triangulierung[seq(1,length(triangulierung), 3)]
  Punkte_2 <- triangulierung[seq(2,length(triangulierung), 3)]
  Punkte_3 <- triangulierung[seq(3,length(triangulierung), 3)]

  # Dreicke herausfiltern -> Punkte zu ordnen
  dreicke <- data.frame(P1 = Punkte_1, P2 = Punkte_2, P3 = Punkte_3)
  
  dreicke <- dreicke %>%
    mutate(triangles_number = seq(1,length(dreicke$P1)), .before = P1)
  
  return(dreicke)
  
}

# Koordinaten zu Dreieckspunkten hinzufuegen
koordinaten_zu_dreickpunkten_hinzufuegen <- function(daten_dreieck,
                                                     daten_koordinaten) {
  
  #TESTCODE
  # daten_dreieck <- triangles
  # daten_koordinaten <- coordinates_districts
  
  daten_dreieck <- daten_dreieck %>%
    pivot_longer(! triangles_number, names_to = "point", values_to = "Id_point") 
  
  temp <- daten_koordinaten %>%
    select(c(Id_point, X, Y))
  
  daten_dreieck <- daten_dreieck %>%
    left_join(temp, by = "Id_point") %>%
    pivot_wider(names_from = "point", values_from = c("X", "Y", "Id_point"))
  
  return(daten_dreieck)
  
}

# Flaeche der Dreiecke berechnen nach Heron
flaeche_berechnen <- function(daten_dreieck) {
  
  #TESTCODE
  # daten_dreieck <- triangles
  
  daten_dreieck <- daten_dreieck %>%
    mutate(l1 = sqrt((X_P1 - X_P2)**2 + (Y_P1 - Y_P2)**2),
           l2 = sqrt((X_P2 - X_P3)**2 + (Y_P2 - Y_P3)**2),
           l3 = sqrt((X_P1 - X_P3)**2 + (Y_P1 - Y_P3)**2),
           p = (l1 + l2 + l3)/2)  
  
  daten_dreieck <- daten_dreieck %>%
    mutate(area = sqrt(p * (p - l1) * (p - l2) * (p - l3)), .after = "triangles_number") %>%
    select(- c(l1, l2, l3, p)) %>%
    select(- c(Id_point_P1, Id_point_P2, Id_point_P3))
  
  return(daten_dreieck)
  
}



# Haushalt fuer Agents erstellen -> NOCH MIT SETTLEMENT MERGEN
haushalt_erstellen <- function(daten_agent, daten_triangles, anzahl) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_triangles <- triangles
  # anzahl <- anzahl_agents
  
  dateiname <<- paste0(dateiname, "_household")
  
  temp <- daten_triangles %>%
    sample_n(anzahl,
             weight = area,
             replace = TRUE)
  
  temp <- temp %>%
    mutate(r_1 = runif(anzahl_agents),
           r_2 = runif(anzahl_agents))
  
  temp <- temp %>%
    mutate(X_household = (X_P1 * (1 - sqrt(r_1)) + (X_P2 * (1 - r_2)* sqrt(r_1)) + (X_P3 * r_2 * sqrt(r_1))),
           Y_household = (Y_P1 * (1 - sqrt(r_1)) + (Y_P2 * (1 - r_2)* sqrt(r_1)) + (Y_P3 * r_2 * sqrt(r_1)))) %>%
    select(c(X_household, Y_household))
  
  daten_agent <- daten_agent %>%
    bind_cols(temp)
  
  return(daten_agent)
}

#----------------------------------------------------------------
# Startwerte bezueglich Infektion waehlen!
gesundheit_erstellen <- function(daten_agent, infiziert) {
  
  #TESTCODE
  # daten_agent <- agents
  # infiziert <- 100
  
  dateiname <<- paste0(dateiname, "_", infiziert, "infected")
  
  if (infiziert == 0) {
    
    daten_agent <- daten_agent %>%
      mutate(susceptible = TRUE,
             infected = FALSE,
             removed = FALSE)
    
  } else {
    
    daten_agent <- daten_agent %>%
      mutate(susceptible = TRUE,
             infected = FALSE,
             removed = FALSE)
    
    indices_infiziert <- sample(1:length(daten_agent$Id_agent), infiziert)
    
    daten_agent$infected[indices_infiziert] <- TRUE
    daten_agent$susceptible[indices_infiziert] <- FALSE
    
  }
  
  # daten_agent <- daten_agent %>%
  #   order(Id_agent)
  
  return(daten_agent)
  
}

#----------------------------------------------------------------
# Waehle Koordinaten von Gemeinden von bestimmten District
koordinaten_auswaehlen_gemeinde <- function(raw_daten, district_wahl) {
  
  #TESTCODE
  # raw_daten <- raw_data_gemeinde_bevoelkerung
  # district_wahl <- district
  
  indices <- which(str_detect(raw_daten$features$properties$iso, paste0("^", district_wahl)))
  iso <- as.numeric(raw_daten$features$properties$iso[indices])
  name <- raw_daten$features$properties$name[indices]
  
  for (j in 1:length(indices)) {
    
    coordinates <- raw_daten$features$geometry$coordinates[[indices[j]]]
    
    laenge <- (length(coordinates)/2)
    
    X <- coordinates[1: laenge]
    Y <- coordinates[(laenge + 1): (laenge * 2)]
    
    if (exists("coordinates_gesamt") == FALSE) {
      
      coordinates_gesamt <- data.frame(name = rep(name[j], laenge),
                                       Id_municipality = rep(iso[j], laenge),
                                       Id_point = seq(1, laenge),
                                       X = X, 
                                       Y = Y)
      
    } else {
      
      temp1 <- data.frame(name = rep(name[j], laenge),
                          Id_municipality = rep(iso[j], laenge),
                          Id_point = seq(1, laenge),
                          X = X, 
                          Y = Y)
      
      coordinates_gesamt <- coordinates_gesamt %>%
        bind_rows(temp1)
      
    }
  }
  
  return(coordinates_gesamt)
  
}

#----------------------------------------------------------------
# Mittelpunkt berechnen
mittelpunkt_berechnen <- function(koordinaten_daten) {
  
  #TESTCODE
  # koordinaten_daten <- coordinates_mumicipality
  
  for (j in unique(koordinaten_daten$Id_municipality)) {
    
    if (exists("centre_gesamt") == FALSE) {
      
      temp1 <- koordinaten_daten %>%
        filter(Id_municipality == j) %>%
        select(X,Y)
      
      centre_gesamt <- data.frame(Id_municipality = j,
                                  X = geosphere::centroid(temp1)[1],
                                  Y = geosphere::centroid(temp1)[2])
      
    } else {
      
      temp1 <- koordinaten_daten %>%
        filter(Id_municipality == j) %>%
        select(X,Y)
      
      temp2 <- data.frame(Id_municipality = j,
                          X = geosphere::centroid(temp1)[1],
                          Y = geosphere::centroid(temp1)[2])
      
      centre_gesamt <- centre_gesamt %>%
        bind_rows(temp2)
    }
  }
  
  return(centre_gesamt)
  
}

#----------------------------------------------------------------
# Abstaende der einzelnen Mittelpunkte ausrechnen
abstand_mittelpunkte_berechnen <- function(centre_daten) {
  
  #TESTCODE
  # centre_daten <- centre_municipality
  
  combinations_centre <- expand.grid(centre_daten$Id_municipality, centre_daten$Id_municipality) %>%
    rename(centre_1 = Var1,
           centre_2 = Var2)
  
  combinations_centre <- combinations_centre %>%
    rename(Id_municipality = centre_1) %>%
    right_join(centre_daten, by = "Id_municipality") %>%
    rename(centre_1 = Id_municipality,
           X_1 = X,
           Y_1 = Y)
  
  combinations_centre <- combinations_centre %>%
    rename(Id_municipality = centre_2) %>%
    right_join(centre_daten, by = "Id_municipality") %>%
    rename(centre_2 = Id_municipality,
           X_2 = X,
           Y_2 = Y)
  
  combinations_centre <- combinations_centre %>%
    mutate(difference_X = X_1 - X_2,
           difference_Y = Y_1 - Y_2) %>%
    mutate(difference_X = difference_X^2,
           difference_Y = difference_Y^2) %>%
    mutate(summe = difference_X + difference_Y) %>%
    mutate(distance = sqrt(summe)) %>%
    select(- c(difference_X, difference_Y, summe, X_1, Y_1, X_2, Y_2))
  
  return(combinations_centre)
  
}



