
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

# Bevoelkerungszahlen altersklassen und Einzeljahre 
einlesen_und_bearbeite_bevoelkerlungszahlen <- function(path_altersklasse, 
                                                        path_einzeljahre,
                                                        auswahl){
  
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
    filter(name == auswahl) %>%
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
alter_erstellen <- function(daten_agent, daten_bevoelkerung, bundesland) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_bevoelkerung <- bevoelkerungszahlen
  # bundesland <- "Niederösterreich"
  
  dateiname <<- paste0(dateiname, "_", bundesland)
  
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
    
    # Gleichverteilung in den einzelnen Gruppen -> SPAETER MITTELS EINZELJAHRE GEWICHTUNG AUF 
    # JEDES EINZELNE JAHR
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





