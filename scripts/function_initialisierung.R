
#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------Agentsinitialisierung---------------------
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

#----------------------------------------------------------------
# Bevoelkerung pro Gemeinde 2019 relative und absolut hinzufuegen
bevoelkerungs_anzahl_name_hinzufuegen <- function(info_daten, district_wahl) {
  
  #TESTCODE
  # info_daten <- infos_mumicipality
  # district_wahl <- district
  
  district_wahl <- as.numeric(district_wahl)
  
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
    filter(Id_district == district_wahl)
  
  temp1 <- temp1 %>%
    select(Id_municipality, name, Anzahl, Anzahl_rel)
  
  info_daten <- temp1 %>%
    right_join(info_daten, by = "Id_municipality")
  
  return(info_daten)
  
}

#----------------------------------------------------------------
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

#----------------------------------------------------------------
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

#----------------------------------------------------------------
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

#----------------------------------------------------------------
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

#----------------------------------------------------------------
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
haushaltsanzahl_erstellen <- function(daten_info, daten_haushalt_bundesland, daten_agents) {
  
  #TESTCODE 
  # daten_info <- infos_mumicipality
  # daten_haushalt_bundesland <- haushalts_gesamt_daten$haushalts_daten_bundesland
  # daten_agents <- agents
  
  temp1 <- bezirksdaten_sehen(speichern = TRUE)
  
  temp1 <- temp1 %>%
    filter(Id_district == as.numeric(district))
  
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
    
    print(paste0("Bezirk mit folgender ID ist fertig: ", id))
    
  }
  
  daten_info <- daten_info %>%
    full_join(haushalte_gesamt, by = "Id_municipality")
  
  return(daten_info)
  
}

#----------------------------------------------------------------
# Haushalte erstellen bezueglich Gemeinden
haushalte_auf_gemeindeebene_erstellen <- function(daten_agent, daten_info) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_info <- infos_mumicipality
  
  dateiname <<- paste0(dateiname, "_household")
  
  infos_haushalte_longer <- daten_info %>%
    select(- c(name, Anzahl, Anzahl_rel, X, Y)) %>%
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

#----------------------------------------------------------------
# Pendelwsk fuer Gemeinden
pendelwsk_berechnen <- function(daten_info, 
                                daten_distanzen, 
                                distanz_wahl = 0.05,
                                speichern) {
  
  #TESTCODE
  # daten_info <- infos_mumicipality
  # daten_distanzen <- distance_between_centre
  # distanz_wahl <- 0.05
  
  temp1 <- daten_info %>%
    select(Id_municipality, Anzahl_rel) 
  
  temp2 <- daten_distanzen %>%
    mutate(distance_new = distance + distanz_wahl)
  
  temp2 <- temp2 %>%
    rename(centre_i = centre_1,
           centre_j = centre_2)
  
  daten_distanzen <- temp2 %>%
    rename(Id_municipality = centre_j) %>%
    left_join(temp1, by = "Id_municipality") %>%
    rename(centre_j = Id_municipality)
  
  daten_distanzen <- daten_distanzen %>%
    mutate(division = Anzahl_rel/distance_new)
  
  temp3 <- temp2 %>%
    rename(Id_municipality = centre_j) %>%
    left_join(temp1, by = "Id_municipality") %>%
    rename(centre_j = Id_municipality)
  
  temp3 <- temp3 %>%
    group_by(centre_i) %>%
    summarise(c_i = sum(Anzahl_rel/distance_new)) 
  
  daten_distanzen <- daten_distanzen %>%
    left_join(temp3, by = "centre_i")
  
  daten_distanzen <- daten_distanzen %>%
    mutate(wsk = division/c_i)
  
  daten_wsk <- daten_distanzen %>%
    select(centre_i, centre_j, wsk)
  
  if (speichern == TRUE) {
    
    setwd("./agents_initialisierung")
    save(daten_wsk, 
         file = paste0("pendelwsk_", distanz_wahl, ".RData"))
    setwd("..")
    
  }
  
  return(daten_wsk)
  
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






