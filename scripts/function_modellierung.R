
#-------------------------Vorbereitung---------------------------
# Detection probability
test_detection_wsk <- function(agents_f) {
  
  #TESTCODE
  # agents_f <- agents
  
  # Altersklassen bilden
  temp <- agents_f %>%
    select(Id_agent, alter) %>%
    mutate(altersklasse = if_else(alter <= 10, 1,
                                  if_else(alter > 10 & alter <= 20, 2,
                                          if_else(alter > 20 & alter <= 30, 3,
                                                  if_else(alter > 30 & alter <= 40, 4, 
                                                          if_else(alter > 40 & alter <= 50, 5, 
                                                                  if_else(alter > 50 & alter <= 60, 6, 
                                                                          if_else(alter > 60 & alter <= 70, 7, 
                                                                                  if_else(alter > 70 & alter <= 80, 8, 
                                                                                          if_else(alter > 80 & alter <= 90, 9, 10))))))))))
  # Percentage kommt von Niki Popper -> pro altersklasse Prozent
  percentage_test <- tibble(altersklasse = 1:10,
                            wsk = c(3.0, 9.0, 22.0, 20.0, 24.0, 28.0, 20.0, 21.0, 33.0, 58.0))
  
  # Zusammenfuegen
  temp <- temp %>%
    left_join(percentage_test, by = "altersklasse") %>%
    select(Id_agent, wsk)
  
  return(temp)
  
}

# Startwerte bezueglich Infektion waehlen!
gesundheit_erstellen <- function(daten_agent, infectious) {
  
  #TESTCODE
  # daten_agent <- agents
  # infectious <- 100
  
  # Unterscheide zwischen 0 und mehr infektioese
  if (infectious == 0) {
    
    # Setze alles auf False bis die susceptible
    daten_agent <- daten_agent %>%
      mutate(susceptible = TRUE,
             infected = FALSE,
             infectious = FALSE,
             tested = FALSE,
             quarantine = FALSE,
             removed = FALSE)
    
  } else {
    
    # dasselbe wie vorher
    daten_agent <- daten_agent %>%
      mutate(susceptible = TRUE,
             infected = FALSE,
             infectious = FALSE,
             tested = FALSE,
             quarantine = FALSE,
             removed = FALSE)
    
    # waehle bestimmte Indices, die infektioese werden aus
    indices_infectious <- sample(1:length(daten_agent$Id_agent), infectious)
    
    # Setze sie auf infectious = TRUE und susceptible = FALSE
    daten_agent$infectious[indices_infectious] <- TRUE
    daten_agent$susceptible[indices_infectious] <- FALSE
    
  }
  
  return(daten_agent)
  
}

#------------------------Model Functions-------------------------
# Zeitstempel fuer State hinterlegen -> nur ohne Quaratäne
zeitstempel_hinterlegen <- function(daten_agent, state) {
  
  #TESTCODE
  # daten_agent <- agents
  # state <- "infected"
  
  # Zeitname -> entweder infected, quarantined, ...
  time_name <- paste0("time_", state)
  
  # Zeit auf 0 setzen zu Beginn
  daten_agent <- daten_agent %>%
    mutate({{time_name}} := 0) 
  
  # Falls schon infiziert auf 1 setzen (ob sinnvoll?) -> vielleicht weglassen
  daten_agent <- daten_agent %>%
    mutate({{time_name}} := if_else(!!sym(state) == TRUE, 1, !!sym(time_name)))
  
  return(daten_agent)
}

# Zeitstempel fuer State hinterlegen; Model mit Quarantäne
zeit_erstellen_infiziert_infektioes <- function(daten_agent, daten_zeit) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_zeit <- zeiten
  
  # Zeitname -> entweder infected, quarantined, ...
  states <- c("time_infected", 
              "time_infectious",
              "time_quarantine",
              "expected_incubation", 
              "expected_latency", 
              "expected_unconfirmed_recovery",
              "expected_reaction_time",
              "expected_quarantine")
  
  # Setze alle Zeiten auf 0
  daten_zeit[states] <- 0
  
  # Infektioese Agents filtern
  temp1 <- daten_agent %>%
    filter(infectious == TRUE)
  
  # Zeiten fuer Inkubation, Latenz, Genesung, Reaktion und Quarantaene schon im vorhinein berechnen
  # Verteilungen kommen von Niki Poper Paper
  incubation <- round((rbeta(nrow(daten_zeit), 3.1, 8.9)*(14 - 2) + 2))
  latency <- incubation - 2
  unconfirmed_recovery <- round(triangle::rtriangle(nrow(daten_zeit), 1, 7, 5))
  reaction_time <- round(rweibull(nrow(daten_zeit), 4.29,1.65))
  quarantine <- round(triangle::rtriangle(nrow(daten_zeit), 11, 16, 14))
  
  # Die erwarteten Zeiten hinzufuegen
  daten_zeit <- daten_zeit %>%
    mutate(expected_incubation = incubation,
           expected_latency = latency,
           expected_unconfirmed_recovery = unconfirmed_recovery,
           expected_reaction_time = reaction_time,
           expected_quarantine = quarantine)
  
  # Schon infektioese auf 1 setzen
  daten_zeit <- daten_zeit %>%
    mutate(time_infected = if_else(Id_agent %in% temp1$Id_agent, expected_latency, time_infected),
           time_infectious = if_else(Id_agent %in% temp1$Id_agent, 1, time_infectious))
  
  return(daten_zeit)
}

# Haushaltskontakte ersellen
kontakte_erstellen_haushalt <- function(daten_agent, daten_kontakte) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # daten_kontakte <- contacts
  
  # Haushalte + darinlebende Agents
  household_with_agent <- daten_agent %>%
    select(Id_municipality, Id_household, Id_agent) %>%
    arrange(Id_municipality, Id_household)
  
  # Umbenennen von Id_agent zu Id_contact -> damit ich nachher joinen kann
  household_with_agent <- household_with_agent %>%
    rename(Id_contact = Id_agent) 
  
  # Fuege zu jedem Id_agent jedes Haushaltsmitglied hinzu 
  # -> Liste: Kombination von jedem Agent mit jedem Haushaltsmitglied
  kontakt_household <- daten_agent %>%
    right_join(household_with_agent, by = c("Id_municipality", "Id_household")) 
  
  # Jede Agent hat einmal einen Kontakt zu jedem Haushaltsmitglied pro Tag
  kontakt_household <- kontakt_household %>%
    mutate(type_of_contact = "household") %>%
    select(Id_agent, type_of_contact, Id_contact)
  
  # Zu Kontaktdaten hinzufuegen und treffen mit sich selbst loeschen
  daten_kontakte <- daten_kontakte %>%
    bind_rows(kontakt_household) %>%
    filter(! Id_agent == Id_contact)
  
  return(daten_kontakte)
  
}

# Freizeitkontakte erstellen
kontakte_erstellen_freizeit <- function(daten_agent, 
                                        daten_kontakte, 
                                        wsk,
                                        daten_goverment) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # daten_kontakte <- contacts
  # wsk <- daten_wsk
  # daten_goverment <- goverment_day
  
  if (daten_goverment$Lockdown == FALSE) {
    
    # Anzahl an Kontakten fuer alle Agents erstellen
    kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
                           contacts = round(rgamma(length(daten_agent$Id_agent),6.11,1)))
    
  } else {
    
    # Anzahl an Kontakten fuer alle Agents erstellen 
    # -> hier einfach mal 1 genommen ohne irgendeiner Referenz
    kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
                           contacts = round(rgamma(length(daten_agent$Id_agent),1,1)))
    
  }
  
  # Data.Table generieren
  daten_wsk_dt <- as.data.table(wsk) 
  daten_agent_dt <- as.data.table(daten_agent)
  
  # Hilfstemp fuer Agent und Heimatgemeinde
  temp1 <- agents %>%
    select(Id_agent, Id_municipality)
  
  # Dataframe mit Id_agent und zugehoerige Anzahl an Kontakten und Heimatgemeinde
  kontakte <- kontakte %>%
    left_join(temp1, by = "Id_agent")
  
  # Pro Agent alle moeglichen Kombinationen mit anderen Gemeinden und Pendelwsk
  kontakte <- kontakte %>%
    rename(centre_i = Id_municipality) %>%
    inner_join(daten_wsk, by = "centre_i")
  
  # Data.Table daraus machen
  kontakte_dt <- as.data.table(kontakte)
  
  # Sample aus moeglichen Kombination alle Gemeinden zu denen der Agent an diesem Tag Kontakt hat
  kontakte_dt <- kontakte_dt[, .(Id_municipality = sample(centre_j, unique(contacts), replace = TRUE, prob = wsk)), by="Id_agent"]
  
  # Generiere Liste an Agents pro Gemeinde
  temp1_dt <- daten_agent_dt[order(Id_municipality), .(Liste_an_agents = list(Id_agent)), by = "Id_municipality"]
  
  # Fuege die Liste den Kontaktgemeinden hinzu
  kontakt_leisure_dt <- merge(kontakte_dt, temp1_dt, by = "Id_municipality")
  
  # Sample aus Liste alle Kontakte
  kontakt_leisure_dt <- kontakt_leisure_dt[, .(Id_agent, Id_contact = sample(unlist(Liste_an_agents), .N, replace = TRUE)), by = "Id_municipality"]
  
  # Ordnen und die wichtigsten waehlen
  kontakt_leisure_dt <- kontakt_leisure_dt[order(Id_agent), .(Id_agent, Id_municipality, Id_contact)]
  
  # Wieder in Data.Frame umwandeln und auswaehlen der Richtigen und type hinzufuegen
  kontakt_leisure <- as.data.frame(kontakt_leisure_dt) %>%
    select(Id_agent, Id_contact) %>%
    mutate(type_of_contact = "leisure", .after = "Id_agent")
  
  # Zu Kontaktdaten hinzufuegen
  daten_kontakte <- daten_kontakte %>%
    bind_rows(kontakt_leisure) %>%
    filter(! Id_agent == Id_contact)
    
  return(daten_kontakte)
  
}

# Arbeitsplatzkontakte erstellen
kontakte_erstellen_arbeitsplatz <- function(daten_agent, daten_kontakte) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # daten_kontakte <- contacts
  
  working_agents <- daten_agent %>%
    filter(type_of_work == "work")
  
  working_agents_in_district <- working_agents %>%
    filter(Id_municipality_work != 99999)
  
  # Anzahl an Kontakten fuer alle Agents erstellen
  kontakte <- data.frame(Id_agent = working_agents_in_district$Id_agent, 
                         contacts = round(rgamma(length(working_agents_in_district$Id_agent),5.28,1)))
  
  # Arbeit + darin arbeitende Agents
  work_with_agent <- working_agents_in_district %>%
    select(Id_municipality_work, Id_workplace, Id_agent) %>%
    arrange(Id_municipality_work, Id_workplace)
  
  # Umbenennen von Id_agent zu Id_contact -> damit ich nachher joinen kann
  work_with_agent <- work_with_agent %>%
    rename(Id_contact = Id_agent) 
  
  # Fuege zu jedem Id_agent jedes Haushaltsmitglied hinzu 
  # -> Liste: Kombination von jedem Agent mit jedem Haushaltsmitglied
  kontakt_workplace <- daten_agent %>%
    filter(type_of_work == "work") %>%
    right_join(work_with_agent, by = c("Id_municipality_work", "Id_workplace")) 
  
  kontakt_workplace <- kontakt_workplace %>%
    right_join(kontakte, by = "Id_agent")
  
  kontakt_workplace <- kontakt_workplace %>%
    filter(Id_agent != Id_contact)
  
  kontakt_workplace <- kontakt_workplace %>%
    group_by(Id_agent) %>%
    sample_n(unique(contacts), replace = TRUE) %>%
    ungroup()
  
  kontakt_workplace <- kontakt_workplace %>%
    mutate(type_of_contact = "workplace") %>%
    select(Id_agent, type_of_contact, Id_contact)
  
  # Zu Kontaktdaten hinzufuegen und treffen mit sich selbst loeschen
  daten_kontakte <- daten_kontakte %>%
    bind_rows(kontakt_workplace) %>%
    filter(! Id_agent == Id_contact)
  
  return(daten_kontakte)
  
}

# Kontakte fuer Volksschule erstellen 
kontakte_erstellen_volksschule <- function(daten_agent, daten_kontakte) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # daten_kontakte <- contacts
  
  primary_school_agents <- daten_agent %>%
    filter(type_of_work == "primary_school")
  
  primary_school_agents_in_district <- primary_school_agents %>%
    filter(Id_municipality_work != 99999)
  
  # Anzahl an Kontakten fuer alle Agents erstellen
  kontakte <- data.frame(Id_agent = primary_school_agents_in_district$Id_agent, 
                         contacts = round(rgamma(length(primary_school_agents_in_district$Id_agent),4.64,1)))
  
  # Arbeit + darin arbeitende Agents
  school_with_agent <- primary_school_agents_in_district %>%
    select(Id_municipality_work, Id_workplace, Id_agent) %>%
    arrange(Id_municipality_work, Id_workplace)
  
  # Umbenennen von Id_agent zu Id_contact -> damit ich nachher joinen kann
  school_with_agent <- school_with_agent %>%
    rename(Id_contact = Id_agent) 
  
  # Fuege zu jedem Id_agent jedes Haushaltsmitglied hinzu 
  # -> Liste: Kombination von jedem Agent mit jedem Haushaltsmitglied
  kontakt_school <- daten_agent %>%
    filter(type_of_work == "primary_school") %>%
    right_join(school_with_agent, by = c("Id_municipality_work", "Id_workplace")) 
  
  kontakt_school <- kontakt_school %>%
    right_join(kontakte, by = "Id_agent")
  
  kontakt_school <- kontakt_school %>%
    filter(Id_agent != Id_contact)
  
  kontakt_school <- kontakt_school %>%
    group_by(Id_agent) %>%
    sample_n(unique(contacts), replace = TRUE) %>%
    ungroup()
  
  kontakt_school <- kontakt_school %>%
    mutate(type_of_contact = "primary_school") %>%
    select(Id_agent, type_of_contact, Id_contact)
  
  # Zu Kontaktdaten hinzufuegen und treffen mit sich selbst loeschen
  daten_kontakte <- daten_kontakte %>%
    bind_rows(kontakt_school) %>%
    filter(! Id_agent == Id_contact)
  
  return(daten_kontakte)
  
}

# Kontakte fuer restliche Schule erstellen 
kontakte_erstellen_schule <- function(daten_agent, daten_kontakte) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # daten_kontakte <- contacts
  
  high_school_agents <- daten_agent %>%
    filter(type_of_work == "school")
  
  high_school_agents_in_district <- high_school_agents %>%
    filter(Id_municipality_work != 99999)
  
  # Anzahl an Kontakten fuer alle Agents erstellen
  kontakte <- data.frame(Id_agent = high_school_agents_in_district$Id_agent, 
                         contacts = round(rgamma(length(high_school_agents_in_district$Id_agent),4.64,1)))
  
  # Arbeit + darin arbeitende Agents
  school_with_agent <- high_school_agents_in_district %>%
    select(Id_municipality_work, Id_workplace, Id_agent) %>%
    arrange(Id_municipality_work, Id_workplace)
  
  # Umbenennen von Id_agent zu Id_contact -> damit ich nachher joinen kann
  school_with_agent <- school_with_agent %>%
    rename(Id_contact = Id_agent) 
  
  # Fuege zu jedem Id_agent jedes Haushaltsmitglied hinzu 
  # -> Liste: Kombination von jedem Agent mit jedem Haushaltsmitglied
  kontakt_school <- daten_agent %>%
    filter(type_of_work == "school") %>%
    right_join(school_with_agent, by = c("Id_municipality_work", "Id_workplace")) 
  
  kontakt_school <- kontakt_school %>%
    right_join(kontakte, by = "Id_agent")
  
  kontakt_school <- kontakt_school %>%
    filter(Id_agent != Id_contact)
  
  kontakt_school <- kontakt_school %>%
    group_by(Id_agent) %>%
    sample_n(unique(contacts), replace = TRUE) %>%
    ungroup()
  
  kontakt_school <- kontakt_school %>%
    mutate(type_of_contact = "high_school") %>%
    select(Id_agent, type_of_contact, Id_contact)
  
  # Zu Kontaktdaten hinzufuegen und treffen mit sich selbst loeschen
  daten_kontakte <- daten_kontakte %>%
    bind_rows(kontakt_school) %>%
    filter(! Id_agent == Id_contact)
  
  return(daten_kontakte)
  
}

# Kontakte fuer Kindergarten erstellen 
kontakte_erstellen_kindergarten <- function(daten_agent, daten_kontakte) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # daten_kontakte <- contacts
  
  kindergarden_agents <- daten_agent %>%
    filter(type_of_work == "kindergarden")
  
  kindergarden_agents_in_district <- kindergarden_agents %>%
    filter(Id_municipality_work != 99999)
  
  # Anzahl an Kontakten fuer alle Agents erstellen; 4.64 muss man noch hinterfragen -> vl anders
  kontakte <- data.frame(Id_agent = kindergarden_agents_in_district$Id_agent, 
                         contacts = round(rgamma(length(kindergarden_agents_in_district$Id_agent),4.64,1)))
  
  # Arbeit + darin arbeitende Agents
  kindergarden_with_agent <- kindergarden_agents_in_district %>%
    select(Id_municipality_work, Id_workplace, Id_agent) %>%
    arrange(Id_municipality_work, Id_workplace)
  
  # Umbenennen von Id_agent zu Id_contact -> damit ich nachher joinen kann
  kindergarden_with_agent <- kindergarden_with_agent %>%
    rename(Id_contact = Id_agent) 
  
  # Fuege zu jedem Id_agent jedes Haushaltsmitglied hinzu 
  # -> Liste: Kombination von jedem Agent mit jedem Haushaltsmitglied
  kontakt_kindergarden <- daten_agent %>%
    filter(type_of_work == "kindergarden") %>%
    right_join(kindergarden_with_agent, by = c("Id_municipality_work", "Id_workplace")) 
  
  kontakt_kindergarden <- kontakt_kindergarden %>%
    right_join(kontakte, by = "Id_agent")
  
  kontakt_kindergarden <- kontakt_kindergarden %>%
    filter(Id_agent != Id_contact)
  
  kontakt_kindergarden <- kontakt_kindergarden %>%
    group_by(Id_agent) %>%
    sample_n(unique(contacts), replace = TRUE) %>%
    ungroup()
  
  kontakt_kindergarden <- kontakt_kindergarden %>%
    mutate(type_of_contact = "kindergarden") %>%
    select(Id_agent, type_of_contact, Id_contact)
  
  # Zu Kontaktdaten hinzufuegen und treffen mit sich selbst loeschen
  daten_kontakte <- daten_kontakte %>%
    bind_rows(kontakt_kindergarden) %>%
    filter(! Id_agent == Id_contact)
  
  return(daten_kontakte)
  
}

# Infektionsstatus aendern
infected_status_aendern_ohne_quarantine <- function(daten_agent, daten_kontakte) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # daten_kontakte <- contacts
  
  # Infektionsstatus fuer jeden Agents waehlen
  temp1 <- daten_agent %>%
    select(Id_agent, infected) %>%
    rename(Id_contact = Id_agent) 
  
  temp2 <- daten_agent %>%
    select(Id_agent, removed) %>%
    rename(removed_agent = removed)
  
  # Jene Agents filtern die mit mindestens einem infizieten Agent Kontakt hatten 
  anzahl_infektioeser_kontakte <- daten_kontakte %>%
    left_join(temp1, by = "Id_contact") %>%
    left_join(temp2, by = "Id_agent") %>%
    filter(removed_agent == FALSE) %>%
    group_by(Id_agent, type_of_contact) %>%
    summarise(Anzahl = sum(infected)) %>%
    ungroup() %>%
    filter(Anzahl > 0)
  
  # Infektionswsk fuer Kontakte berechnen
  wsk_infektion <- data.frame(Id_agent = rep(anzahl_infektioeser_kontakte$Id_agent, anzahl_infektioeser_kontakte$Anzahl),
                              type_of_contact = rep(anzahl_infektioeser_kontakte$type_of_contact, anzahl_infektioeser_kontakte$Anzahl),
                              wsk = runif(sum(anzahl_infektioeser_kontakte$Anzahl), 0, 1))
  
  # Alle Agents-Id behalten
  temp2 <- daten_agent %>%
    select(Id_agent)
  
  # Anzahl an Infektionen pro Agent
  anzahl_infektion_pro_agent <- wsk_infektion %>%
    group_by(Id_agent, type_of_contact) %>%
    mutate(infection = if_else(wsk < 0.25 & type_of_contact == "household", TRUE, 
                               if_else(wsk < 0.05, TRUE, FALSE))) %>%
    ungroup() 
  
  anzahl_infektion_pro_agent <- anzahl_infektion_pro_agent %>%
    group_by(Id_agent) %>%
    summarise(Anzahl = sum(infection)) %>%
    ungroup()
  
  # Status ob infeziert oder nicht
  status_infected <- anzahl_infektion_pro_agent %>%
    mutate(infected_new = if_else(Anzahl > 0, TRUE, FALSE)) %>%
    right_join(temp2, by = "Id_agent") %>% #damit ich alle Agents habe -> auch die die keinen Kontakt haben 
    select(- Anzahl) 
  
  # NAs in False umwandeln
  status_infected <- status_infected %>%
    mutate(infected_new = if_else(is.na(infected_new), FALSE, infected_new)) %>%
    arrange(Id_agent)
  
  # Ordnen nach Agent-Id
  daten_agent <- daten_agent %>%
    arrange(Id_agent)
  
  # zusammenfuegen mit den Agentsdaten -> und infected-Status aktualisieren
  daten_agent <- daten_agent %>%
    inner_join(status_infected, by = "Id_agent") %>%
    mutate(infected = if_else(infected == TRUE, infected, infected_new))
  
  # Susziple False falls infeziert TRUE
  daten_agent <- daten_agent %>%
    mutate(susceptible = if_else(infected, FALSE, susceptible)) %>%
    select(- infected_new)
  
  return(daten_agent)
  
}

# Infektionsstatus aendern
infected_status_aendern_mit_quarantine <- function(daten_agent, 
                                                   daten_kontakte,
                                                   wsk_infection_hh = 0.25,
                                                   wsk_infection_other = 0.05) {
  
  #TESTCODE
  # daten_agent <- agents_f
  # daten_kontakte <- contacts
  
  # Infektionsstatus fuer jeden Agents waehlen
  temp1 <- daten_agent %>%
    select(Id_agent, infectious) %>%
    rename(Id_contact = Id_agent) 
  
  temp2 <- daten_agent %>%
    select(Id_agent, removed, infectious, infected) %>%
    rename(removed_agent = removed) %>%
    rename(infectious_agent = infectious) %>%
    rename(infected_agent = infected)
  
  # Jene Agents filtern die mit mindestens einem infizieten Agent Kontakt hatten 
  anzahl_infektioeser_kontakte <- daten_kontakte %>%
    left_join(temp1, by = "Id_contact") %>%
    left_join(temp2, by = "Id_agent") %>%
    filter(removed_agent == FALSE & infectious_agent == FALSE & infected_agent == FALSE) %>%
    group_by(Id_agent, type_of_contact) %>%
    summarise(Anzahl = sum(infectious)) %>%
    ungroup() %>%
    filter(Anzahl > 0)
  
  # Infektionswsk fuer Kontakte berechnen
  wsk_infektion <- data.frame(Id_agent = rep(anzahl_infektioeser_kontakte$Id_agent, anzahl_infektioeser_kontakte$Anzahl),
                              type_of_contact = rep(anzahl_infektioeser_kontakte$type_of_contact, anzahl_infektioeser_kontakte$Anzahl),
                              wsk = runif(sum(anzahl_infektioeser_kontakte$Anzahl), 0, 1))
  
  # Alle Agents-Id behalten
  temp2 <- daten_agent %>%
    select(Id_agent)
  
  # Anzahl an Infektionen pro Agent
  anzahl_infektion_pro_agent <- wsk_infektion %>%
    group_by(Id_agent, type_of_contact) %>%
    mutate(infection = if_else(wsk < wsk_infection_hh & type_of_contact == "household", TRUE, 
                               if_else(wsk < wsk_infection_other, TRUE, FALSE))) %>%
    ungroup() 
  
  anzahl_infektion_pro_agent <- anzahl_infektion_pro_agent %>%
    group_by(Id_agent) %>%
    summarise(Anzahl = sum(infection)) %>%
    ungroup()
  
  # Status ob infeziert oder nicht
  status_infected <- anzahl_infektion_pro_agent %>%
    mutate(infected_new = if_else(Anzahl > 0, TRUE, FALSE)) %>%
    right_join(temp2, by = "Id_agent") %>% #damit ich alle Agents habe -> auch die die keinen Kontakt haben 
    select(- Anzahl) 
  
  # NAs in False umwandeln
  status_infected <- status_infected %>%
    mutate(infected_new = if_else(is.na(infected_new), FALSE, infected_new)) %>%
    arrange(Id_agent)
  
  # Ordnen nach Agent-Id
  daten_agent <- daten_agent %>%
    arrange(Id_agent)
  
  # zusammenfuegen mit den Agentsdaten -> und infected-Status aktualisieren
  daten_agent <- daten_agent %>%
    inner_join(status_infected, by = "Id_agent") %>%
    mutate(infected = if_else(infected == TRUE, infected, infected_new))
  
  # Susziple False falls infeziert TRUE
  daten_agent <- daten_agent %>%
    mutate(susceptible = if_else(infected, FALSE, susceptible)) %>%
    select(- infected_new)
  
  return(daten_agent)
  
}

# Neuinfektionen berechnen 
erkannte_neuinfektionen_berechnen <- function(agents_ff,
                                              zeiten_ff) {
  
  #TESTCODE
  # agents_ff <- agents_f
  # zeiten_ff <- zeiten_f
  
  # FIlter nach jene die in Quarantaene sind aber noch zeit 0 darin verbracht haben
  # -> muss nach Status aenderung ausgefuehrt werden
  temp1 <- agents_ff %>%
    filter(quarantine == TRUE) %>%
    inner_join(zeiten_ff, by = "Id_agent") %>%
    filter(time_quarantine == 0) %>%
    nrow()
  
  return(temp1)
}

# Getestet oder nicht
getestet_entscheiden <- function(agents_ff,
                                 zeiten_f,
                                 detection_wsk_per_agent_ff) {
  
  #TESTCODE
  # agents_ff <- agents
  # zeiten_f <- zeiten
  # detection_wsk_per_agent_ff <- detection_wsk_per_agent_f
  
  # waehle interessante Spalten
  temp1 <- agents_ff %>%
    select(Id_agent, infected, infectious, tested, quarantine,removed)
  
  # fuege Zeiten, Gesunheitsstatus und detection wsk zusammen
  temp2 <- zeiten_f %>%
    inner_join(temp1, by = "Id_agent") %>%
    inner_join(detection_wsk_per_agent_ff, by = "Id_agent")
  
  # filter jene, die infektioes sind und inkubation fertig ist
  agents_to_test <- temp2 %>%
    filter(infectious == TRUE & (time_infected + time_infectious) == expected_incubation)
  
  # fuege entdeckungswsk hinzu
  agents_to_test <- agents_to_test %>%
    mutate(test_wsk = round(runif(n()) * 100)) %>%
    select(Id_agent, wsk, test_wsk)
  
  # filter nach jene die getestet werden
  agents_to_test <- agents_to_test %>%
    filter(test_wsk <= wsk)
  
  # setze deren Status tested = TRUE
  agents_ff <- agents_ff %>%
    mutate(tested = if_else(Id_agent %in% agents_to_test$Id_agent, TRUE, tested))
  
  return(agents_ff)
  
}

# Restlichen Status aendern
restlichen_status_aendern <- function(agents_ff,
                                      zeiten_ff) {
  
  #TESTCODE
  # agents_ff <- agents_f
  # zeiten_ff <- zeiten
  
  # Waehle gesundheitsstatus der Agents aus
  temp1 <- agents_ff %>%
    select(Id_agent, infected, infectious, tested, quarantine,removed)
  
  # Fuege Zeiten und Gesundheitsstatus zusammen
  temp2 <- zeiten_ff %>%
    inner_join(temp1, by = "Id_agent")
  
  # Waehle jene aus, infektioes werden 
  # (infected == TRUE und infected_time muss gleich der Latenzzeit sein)
  agents_getting_infectious <- temp2 %>%
    filter(infected == TRUE & time_infected == expected_latency)
  
  # Aendere Status jener Agents, deren Id vorher vorkommt
  agents_ff <- agents_ff %>%
    mutate(infected = if_else(Id_agent %in% agents_getting_infectious$Id_agent, FALSE, infected),
           infectious = if_else(Id_agent %in% agents_getting_infectious$Id_agent, TRUE, infectious))
  
  # Agents, welche ohne Quarantaene removed werden 
  agents_getting_removed_without_quarantine <- temp2 %>%
    filter(infectious == TRUE & tested == FALSE & (time_infected + time_infectious) == (expected_unconfirmed_recovery + expected_incubation))
  
  # Aendere Status jener Agents, deren Id vorher vorkommt
  agents_ff <- agents_ff %>%
    mutate(removed = if_else(Id_agent %in% agents_getting_removed_without_quarantine$Id_agent, TRUE, removed),
           infectious = if_else(Id_agent %in% agents_getting_removed_without_quarantine$Id_agent, FALSE, infectious))
  
  # Agents, welche in Quarantaene gehen
  agents_getting_in_quarantine <- temp2 %>%
    filter(infectious == TRUE & tested == TRUE & (time_infectious + time_infected) == (expected_incubation + expected_reaction_time))
  
  # Aendere Status jener Agents, deren Id vorher vorkommt
  agents_ff <- agents_ff %>%
    mutate(infectious = if_else(Id_agent %in% agents_getting_in_quarantine$Id_agent, FALSE, infectious),
           quarantine = if_else(Id_agent %in% agents_getting_in_quarantine$Id_agent, TRUE, quarantine))
  
  # Agents, die resistant werden aber davor in Quarantaene sind
  agents_getting_removed_with_quarantine <- temp2 %>%
    filter(quarantine == TRUE & time_quarantine == expected_quarantine)
  
  # Aendere Status jener Agents, deren Id vorher vorkommt
  agents_ff <- agents_ff %>%
    mutate(removed = if_else(Id_agent %in% agents_getting_removed_with_quarantine$Id_agent, TRUE, removed),
           quarantine = if_else(Id_agent %in% agents_getting_removed_with_quarantine$Id_agent, FALSE, quarantine))
  
  return(agents_ff)
  
}

# Zeiten aendern
zeiten_aendern <- function(agents_ff, 
                           zeiten_ff) {
  
  #TESTCODE
  # agents_ff <- agents
  # zeiten_ff <- zeiten
  
  # Gesundheitsstatus waehlen
  temp1 <- agents_ff %>%
    select(Id_agent, infected, infectious, quarantine)
  
  # Zusammenfuegen mit Agents
  temp2 <- zeiten_ff %>%
    inner_join(temp1, by = "Id_agent")
  
  # Zeiten um 1 erhoehen, je nachdem was TRUE ist
  temp2 <- temp2 %>%
    mutate(time_infected = if_else(infected == TRUE, time_infected + 1, time_infected), 
           time_infectious = if_else(infectious == TRUE, time_infectious + 1, time_infectious),
           time_quarantine = if_else(quarantine == TRUE, time_quarantine + 1, time_quarantine)) %>%
    select(- c(infected, infectious, quarantine))
  
  return(temp2)
  
}

#----------------------------Models------------------------------
# Function fuer model ohne Quarantaene
corona_model_agent_based <- function(agents_f, 
                                     tage_f, 
                                     type_of_contactfunctions_f,
                                     daten_wsk_f,
                                     goverment_f,
                                     plotting = FALSE){
  
  #TESTCODE
  # agents_f <- agents
  # tage_f <- tage
  # type_of_contactfunctions_f <- c("freizeit", "haushalt", "arbeitsplatz", "volksschule", "schule", "kindergarten")
  # daten_wsk_f <- daten_wsk
  # goverment_f <- goverment
  # plotting <- TRUE
  
  # Namen der Kontakt functions erstellen
  kontakt_functions <- paste0("kontakte_erstellen_", type_of_contactfunctions_f)
  
  # Zeitstempel fuer Infektion hinterlegen
  agents_basic_model <- zeitstempel_hinterlegen(agents_f, "infected")
  
  # Faelle verfolgen
  tracker_cases <- data.frame(Tag = 0,
                              S = sum(agents_basic_model$susceptible),
                              I = sum(agents_basic_model$infected),
                              R = sum(agents_basic_model$removed),
                              Neuinfektionen = 0)
  
  # Zeit verfolgen
  tracker_time <- data.frame(time = c())
  
  for (day in 1:tage_f) {
    
    # Startzeit eines Tages
    time_begin <- Sys.time()
    
    # Regierungseinschraenkung an diesem Tag
    goverment_day <- goverment_f %>%
      filter(Tag == day)
    
    # Kontakte erstellen
    contacts <- data.frame(Id_agent = c(), 
                           type_of_contact = c(), 
                           Id_contact = c())
    
    for (funktion in kontakt_functions) {
      
      temp_func <- get(funktion)
      
      if (grepl("freizeit", funktion) == TRUE) {
        
        contacts <- temp_func(agents_basic_model, contacts, daten_wsk_f, goverment_day)
        
      } else {
        
        contacts <- temp_func(agents_basic_model, contacts)
        
      }
      
    }
    
    # Infected Status aendern
    agents_basic_model <- infected_status_aendern_ohne_quarantine(agents_basic_model, contacts)
    
    # Neuinfektionen berechnen
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
    
    # Zeitreihe bilden
    tracker_cases <- tracker_cases %>%
      bind_rows(temp)
    
    # Zeit mitstoppen -> Validierung
    time_end <- Sys.time()
    difference <- data.frame(time = time_end - time_begin)
    tracker_time <- tracker_time %>%
      bind_rows(difference)
    
    cat(paste0("Tag:", day, "\n"))
  }
  
  cat(paste0("Die durchschnittliche Dauer pro Tag beträgt ", mean(tracker_time$time), "!\n",
             "Die Gesamtzeit beträgt ", sum(tracker_time$time), "!\n"))
  
  # Make the data longer
  tracker_cases <- tracker_cases %>%
    pivot_longer(cols = - Tag, names_to = "type", values_to = "Wert")
  
  if (plotting == TRUE) {
    
    plot <- ggplot(tracker_cases) +
      aes(x = Tag) +
      geom_line(aes(y = Wert, color = type)) +
      ylim(0, length(agents_f$Id_agent))
    
    print(plot)
    
  }
  
  return(tracker_cases)
  
}

# Function fuer Modell mit Quarantaene
corona_model_agent_based_with_quarantine <- function(agents_f, 
                                                     tage_f,
                                                     type_of_contactfunctions_f,
                                                     daten_wsk_f,
                                                     goverment_f,
                                                     detection_wsk_per_agent_f,
                                                     plotting = FALSE){
  
  #TESTCODE
  # agents_f <- agents
  # tage_f <- tage
  # type_of_contactfunctions_f <- type_of_contactfunctions
  # daten_wsk_f <- daten_wsk
  # goverment_f <- goverment
  # detection_wsk_per_agent_f <- detection_wsk_per_agent
  # plotting <- TRUE
  
  # Zeitstempel fuer infectious hinterlegen
  zeiten <- tibble(Id_agent = agents_f$Id_agent)
  zeiten <- zeit_erstellen_infiziert_infektioes(agents_f, zeiten)
  
  # Namen der Kontakt functions erstellen
  kontakt_functions <- paste0("kontakte_erstellen_", type_of_contactfunctions_f)
  
  # Faelle verfolgen
  tracker_cases <- data.frame(Tag = 0,
                              S = sum(agents_f$susceptible),
                              I = sum(agents_f$infected),
                              I_ = sum(agents_f$infectious),
                              Q = sum(agents_f$quarantine), 
                              R = sum(agents_f$removed),
                              Neuinfektionen = 0,
                              Summe_ErkannteFaelle = 0)
  
  # Zeit verfolgen
  tracker_time <- data.frame(time = c())
  
  for (day in 1:tage_f) {
    
    # Startzeit eines Tages
    time_begin <- Sys.time()
    
    # Regierungseinschraenkung an diesem Tag
    goverment_day <- goverment_f %>%
      filter(Tag == day)
    
    # Kontakte erstellen
    contacts <- data.frame(Id_agent = c(), 
                           type_of_contact = c(), 
                           Id_contact = c())
    
    # Nimm jene Agents raus die in Quarantaene sind -> Haben keine Kontakte
    agents_not_quarantine <- agents_f %>%
      filter(quarantine == FALSE)
    
    for (funktion in kontakt_functions) {
      
      temp_func <- get(funktion)
      
      if (grepl("freizeit", funktion) == TRUE) {
        
        contacts <- temp_func(agents_not_quarantine, contacts, daten_wsk_f, goverment_day)
        
      } else {
        
        contacts <- temp_func(agents_not_quarantine, contacts)
        
      }
      
    }
    
    # Infected Status aendern
    agents_f <- infected_status_aendern_mit_quarantine(agents_f, contacts)
    
    # Quarantaene Test
    agents_f <- getestet_entscheiden(agents_ff = agents_f,
                                     zeiten_f = zeiten,
                                     detection_wsk_per_agent_ff = detection_wsk_per_agent_f)
    
    # restlichen Status aendern 
    agents_f <- restlichen_status_aendern(agents_ff = agents_f,
                                          zeiten_ff = zeiten) 
    
    # Neuinfektionen berechnen
    neuinfektionen <- erkannte_neuinfektionen_berechnen(agents_ff = agents_f,
                                                        zeiten_ff = zeiten)
    
    # Zeiten aendern
    zeiten <- zeiten_aendern(agents_ff = agents_f, 
                               zeiten_ff = zeiten)
    
    # Pro Tag Summe merken
    temp <- data.frame(Tag = day,
                       S = sum(agents_f$susceptible),
                       I = sum(agents_f$infected),
                       I_ = sum(agents_f$infectious),
                       Q = sum(agents_f$quarantine),
                       R = sum(agents_f$removed),
                       Neuinfektionen = neuinfektionen,
                       Summe_ErkannteFaelle = sum(tracker_cases$Neuinfektionen) + neuinfektionen)
    
    tracker_cases <- tracker_cases %>%
      bind_rows(temp)
    
    time_end <- Sys.time()
    difference <- data.frame(time = time_end - time_begin)
    tracker_time <- tracker_time %>%
      bind_rows(difference)
    
    cat(paste0("Tag:", day, "\n"))
  }
  
  cat(paste0("Die durchschnittliche Dauer pro Tag beträgt ", mean(tracker_time$time), "!\n",
             "Die Gesamtzeit beträgt ", sum(tracker_time$time), "!\n"))
  
  # Make the data longer
  tracker_cases <- tracker_cases %>%
    pivot_longer(cols = - Tag, names_to = "type", values_to = "Wert")
  
  if (plotting == TRUE) {
    
    plot <- ggplot(tracker_cases) +
      aes(x = Tag) +
      geom_line(aes(y = Wert, color = type)) +
      ylim(0, length(agents_f$Id_agent))
    
    print(plot)
    
  }
  
  return(tracker_cases)
  
}

# Function for Plotting all results together
plot_model_results_together <- function(quarantine, agent_status) {
  
  #TESTCODE
  # quarantine <- TRUE
  # agent_status <- "I"
  
  # Liste alles Data frames mit Model in namen
  liste_von_DataFrames <- names(.GlobalEnv)[grepl("Model", names(.GlobalEnv))]
  
  # Waehle ob mit oder ohne Quarantaene
  if (quarantine == TRUE) {
    
    search_word <- "WithQ"
    
  } else {
    
    search_word <- "WithoutQ"
    
  }
  
  # Nur jene mit richtigem search-Wort darin
  liste_von_DataFrames_relevant <- liste_von_DataFrames[grepl(search_word, liste_von_DataFrames)]
  
  # Falls kein Dataframe uebrig -> schreiben 
  if(length(liste_von_DataFrames_relevant) == 0) {
    
    cat("Kein Modell dieser Art berechnet!\n")
    
  } else {
    
    # Hilfsdataframe
    temp <- data.frame()
    
    for (x in liste_von_DataFrames_relevant) {
      
      df <- get(x) 
      
      df$Model <- x
      
      temp <- temp %>%
        bind_rows(df)
      
    }
    
  }
  
    # Plotten
  p <- temp %>%
    filter(type %in% agent_status) %>%
    ggplot() +
    aes(x = Tag, y = Wert, color = Model) +
    geom_line() +
    facet_wrap(~ type) +
    theme_bw()
    
  print(p)
  
}

#---------------------Monte-Carlo-Simulation---------------------
# Monte Carlo Simulation
monte_carlo_simulation <- function(agents_ff, 
                                   tage_ff, 
                                   type_of_contactfunctions_ff,
                                   daten_wsk_ff,
                                   goverment_ff,
                                   detection_wsk_per_agent_ff,
                                   times_f,
                                   quarantine) {
  
  #TESTCODE
  # agents_ff <- agents
  # tage_ff <- tage
  # type_of_contactfunctions_ff <- type_of_contactfunctions
  # daten_wsk_ff <- daten_wsk
  # goverment_ff <- goverment
  # times_f <- 5
  
  results_MonteCarlo <- data.frame()
  
  for (i in 1:times_f) {
    
    col_name <- paste0("Durchlauf_", i) 
    
    if (quarantine == FALSE) {
      
      temp <- corona_model_agent_based(agents_f = agents_ff,
                                       tage_f = tage_ff,
                                       type_of_contactfunctions_f = type_of_contactfunctions_ff,
                                       daten_wsk_f = daten_wsk_ff,
                                       goverment_f = goverment_ff)
      
      temp$Durchlauf <- i
      
      results_MonteCarlo <- results_MonteCarlo %>%
        bind_rows(temp)
      
      cat(paste0(col_name, " von ", times_f, " ist erledigt!\n"))
      
    } else {
      
      temp <- corona_model_agent_based_with_quarantine(agents_f = agents_ff,
                                                       tage_f = tage_ff,
                                                       type_of_contactfunctions_f = type_of_contactfunctions_ff,
                                                       daten_wsk_f = daten_wsk_ff,
                                                       goverment_f = goverment_ff,
                                                       detection_wsk_per_agent_f = detection_wsk_per_agent_ff)
      
      temp$Durchlauf <- i
      
      results_MonteCarlo <- results_MonteCarlo %>%
        bind_rows(temp)
      
      cat(paste0(col_name, " von ", times_f, " ist erledigt!\n"))
      
    }
    
  }
  
  return(results_MonteCarlo)
  
} 

# Durchschnitt und Konfidenzintervalle hinzufuegen
add_mean_and_ConfInt <- function(daten_MonteCarlo) {
  
  #TESTCODE
  # daten_MonteCarlo <- model_5
  
  # Durchschnitt pro Tag und type (S, I, R, ...) berechnen
  durchschnitt <- daten_MonteCarlo %>%
    group_by(Tag, type) %>%
    summarise(Wert = mean(Wert)) %>%
    ungroup() %>%
    mutate(Durchlauf = "Schnitt")
  
  # Auf Durchlauf auf Character aendern
  daten_MonteCarlo$Durchlauf <- as.character(daten_MonteCarlo$Durchlauf)
  
  # Durchschnitt hinzufuegen
  daten_MonteCarlo <- daten_MonteCarlo %>%
    bind_rows(durchschnitt)
  
  # confidenz intervalle berechnen
  ConfIntervall <- daten_MonteCarlo %>%
    group_by(Tag, type) %>%
    summarise(durchschnitt = mean(Wert),
              Anzahl = n(),
              standardabweichung = sd(Wert)) %>%
    mutate(t_quantil = qt(0.975, df = Anzahl - 1))
  
  ConfIntervall <- ConfIntervall %>%
    mutate(lower = durchschnitt - t_quantil * standardabweichung/sqrt(Anzahl),
           upper = durchschnitt + t_quantil * standardabweichung/sqrt(Anzahl))
  
  # Confidence interval in die richtige Form bringen
  ConfIntervall <- ConfIntervall %>%
    select(c(Tag, type, lower, upper)) %>%
    pivot_longer(cols = c(lower, upper), names_to = "Durchlauf", values_to = "Wert") 
  
  # Zusammenfuegen
  daten_MonteCarlo <- daten_MonteCarlo %>%
    bind_rows(ConfIntervall)
    
  return(daten_MonteCarlo)
  
}

# plot Ergebnisse von MonteCarlo simulation
plot_result_MonteCarlo <- function(daten_MonteCarlo, agent_status) {
  
  temp1 <- daten_MonteCarlo %>%
    filter(Durchlauf %in% c("Schnitt", "upper", "lower") & type %in% agent_status)
  
  temp2 <- daten_MonteCarlo %>%
    filter(!(Durchlauf %in% c("Schnitt", "upper", "lower")) & type %in% agent_status)
  
  p <- ggplot(temp2) + 
    geom_line(aes(x = Tag, y = Wert, fill = Durchlauf), 
              color = "lightgrey",
              show.legend = FALSE) +
    geom_line(data = temp1, 
              aes(x = Tag, y = Wert, color = Durchlauf), 
              show.legend = FALSE) +
    scale_color_manual(values=c("red", "black", "red")) +
    theme_bw() +
    facet_wrap(~ type)
  
  print(p)
  
}

# Function for Plotting all results together
plot_MonteCarlo_results_together <- function(quarantine, agent_status, plot_inhalt) {
  
  #TESTCODE
  # quarantine <- TRUE
  # agent_status <- "I"
  # plot_inhalt <- "Schnitt"
  
  # Liste alles Data frames mit Model in namen
  liste_von_DataFrames <- names(.GlobalEnv)[grepl("MonteCarlo", names(.GlobalEnv))]
  
  # Waehle ob mit oder ohne Quarantaene
  if (quarantine == TRUE) {
    
    search_word <- "WithQ"
    
  } else {
    
    search_word <- "WithoutQ"
    
  }
  
  # Nur jene mit richtigem search-Wort darin
  liste_von_DataFrames_relevant <- liste_von_DataFrames[grepl(search_word, liste_von_DataFrames)]
  
  # Falls kein Dataframe uebrig -> schreiben 
  if(length(liste_von_DataFrames_relevant) == 0) {
    
    cat("Kein Modell dieser Art berechnet!\n")
    
  } else {
    
    # Hilfsdataframe
    temp <- data.frame()
    
    for (x in liste_von_DataFrames_relevant) {
      
      df <- get(x) 
      
      df$Model <- x
      
      temp <- temp %>%
        bind_rows(df)
      
    }
    
  }
  
  anzahl_durchlaeufe <- length(unique(temp$Durchlauf)) - 3
  
  # Plotten
  p <- temp %>%
    filter(type %in% agent_status & Durchlauf %in% plot_inhalt) %>%
    ggplot() +
    aes(x = Tag, y = Wert, color = Model, fill = Durchlauf) +
    geom_line() +
    facet_wrap(~ type) +
    theme_bw() +
    labs(color = paste0("Monte-Carlo Simulation mit ", anzahl_durchlaeufe, " Durchlaeufen:"))
  
  print(p)
  
}

# Teste wie viele Iterationen Monte-Carlo
test_MonteCarlo_Iteration <- function(sequenz_f, 
                                      type_of_contactfunctions_f,
                                      agents_f,
                                      daten_wsk_f,
                                      tage_f,
                                      detection_wsk_per_agent_f,
                                      goverment_f,
                                      quarantine_f) {
  
  MonteCarlo <- tibble()
  
  for (i in sequenz_f) {
    
    temp1 <- monte_carlo_simulation(agents_ff = agents_f,
                                    tage_ff = tage_f,
                                    type_of_contactfunctions_ff = type_of_contactfunctions_f,
                                    daten_wsk_ff = daten_wsk_f,
                                    goverment_ff = goverment_f,
                                    times_f = i,
                                    detection_wsk_per_agent_ff = detection_wsk_per_agent_f,
                                    quarantine = quarantine_f)
    
    temp1 <- add_mean_and_ConfInt(temp1)
    
    temp1$MonteCarlo_Iterationen <- as.character(i)
    
    MonteCarlo <- MonteCarlo %>%
      bind_rows(temp1)
    
    print(head(temp1))
    print(paste0("Iterationen:", i))
    
  }
  
  return(MonteCarlo)
  
}

# plot MonteCarlo Test Iterationen
plot_MonteCarloTest_together <- function(daten_MonteCarloTest,
                                         durchlauf_type, 
                                         agent_status) {
  
  p <- daten_MonteCarloTest %>%
    filter(Durchlauf %in% durchlauf_type & type %in% agent_status) %>%
    ggplot() +
    aes(x = Tag, y = Wert, color = MonteCarlo_Iterationen, fill = Durchlauf) +
    geom_line() +
    theme_bw() +
    facet_wrap(~ type)
  
  print(p)
  
}

#----------------------------------------------------------------
#----------------------------------------------------------------
#---------------------------DEPRECATED---------------------------
#DEPRECATED

# Kontakte pro Tag fuer jeden agent erstellen
kontakte_erstellen <- function(daten_agent){
  
  #TESTCODE
  # daten_agent <- agents
  
  kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
                         contacts = round(rgamma(length(daten_agent$Id_agent),6.11,1)))
  
  kontakte <- kontakte %>%
    group_by(Id_agent) %>%
    mutate(contact_agent = list(sample(kontakte$Id_agent, contacts, replace = TRUE))) %>%
    ungroup() %>%
    select(Id_agent, contact_agent)
  
  if ("contact_agent" %in% colnames(daten_agent)) {
    
    daten_agent$contact_agent <- kontakte$contact_agent
    
  } else {
    
    daten_agent <- daten_agent %>%
      full_join(kontakte, by = "Id_agent")
    
  }
  
  return(daten_agent)
  
}

# Alle kontakte zusammen erstellen
kontakte_erstellen_neu <- function(daten_agent, household, wsk) {
  
  #TESTCODE
  # daten_agent <- agents_basic_model
  # household <- TRUE
  # wsk <- daten_wsk
  
  # # Anzahl an Kontakten fuer alle Agents erstellen
  # kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
  #                        contacts = round(rgamma(length(daten_agent$Id_agent),6.11,1)))
  # 
  # # Kontakte erstellen fuer alle Agents
  # kontakt_leisure <- data.frame(Id_agent = rep(daten_agent$Id_agent, kontakte$contacts),
  #                               type_of_contact = "leisure", 
  #                               Id_contact = sample(kontakte$Id_agent, sum(kontakte$contacts), replace = TRUE))
  # 
  
  # Anzahl an Kontakten fuer alle Agents erstellen
  kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
                         contacts = round(rgamma(length(daten_agent$Id_agent),6.11,1)))
  
  # Data.Table generieren
  kontakte_dt <- as.data.table(kontakte)
  daten_wsk_dt <- as.data.table(wsk) 
  daten_agent_dt <- as.data.table(daten_agent)
  
  temp1 <- agents %>%
    select(Id_agent, Id_municipality)
  
  kontakte <- kontakte %>%
    left_join(temp1, by = "Id_agent")
  
  kontakte <- kontakte %>%
    rename(centre_i = Id_municipality) %>%
    inner_join(daten_wsk, by = "centre_i")
  
  kontakte_dt <- as.data.table(kontakte)
  
  kontakte_dt <- kontakte_dt[, .(Id_municipality = sample(centre_j, unique(contacts), replace = TRUE, prob = wsk)), by="Id_agent"]
  
  temp1_dt <- daten_agent_dt[order(Id_municipality), .(Liste_an_agents = list(Id_agent)), by = "Id_municipality"]
  
  kontakt_leisure_dt <- merge(kontakte_dt, temp1_dt, by = "Id_municipality")
  
  kontakt_leisure_dt <- kontakt_leisure_dt[, .(Id_agent, Id_contact = sample(unlist(Liste_an_agents), .N, replace = TRUE)), by = "Id_municipality"]
  
  kontakt_leisure_dt <- kontakt_leisure_dt[order(Id_agent), .(Id_agent, Id_municipality, Id_contact)]
  
  kontakt_leisure <- as.data.frame(kontakt_leisure_dt) %>%
    select(Id_agent, Id_contact) %>%
    mutate(type_of_contact = "leisure", .after = "Id_agent")
  
  
  if (household == TRUE) {
    
    household_with_agent <- daten_agent %>%
      select(Id_municipality, Id_household, Id_agent) %>%
      arrange(Id_municipality, Id_household)
    
    household_with_agent <- household_with_agent %>%
      rename(Id_contact = Id_agent) 
    
    kontakt_household <- daten_agent %>%
      right_join(household_with_agent, by = c("Id_municipality", "Id_household")) 
    
    kontakt_household <- kontakt_household %>%
      mutate(type_of_contact = "household") %>%
      select(Id_agent, type_of_contact, Id_contact)
    
    kontakt_agent <- kontakt_leisure %>%
      bind_rows(kontakt_household) %>%
      filter(! Id_agent == Id_contact)
    
  } else {
    
    kontakt_agent <- kontakt_leisure %>%
      filter(! Id_agent == Id_contact)
    
  }
  
  return(kontakt_agent)
}


# Agenten Wsk pro Kontakt, dass man infiziert wird waehlen
infektion <- function(daten_agent) {
  
  #TESTCODE
  # daten_agent <- agents
  
  # Status der Kontakte
  daten_agent <- daten_agent %>%
    rowwise() %>%
    mutate(temp = list(daten_agent$infected[unlist(contact_agent)])) %>%
    ungroup()
  
  # Anzahl der infizierten Kontakte
  daten_agent <- daten_agent %>%
    rowwise() %>%
    mutate(temp = sum(unlist(temp))) %>%
    ungroup()
  
  # Wsk infeziert zu werden pro Kontakt
  daten_agent <- daten_agent %>%
    rowwise() %>%
    mutate(temp = if_else(temp > 0, list(runif(temp, 0, 1)), list(1))) %>%
    ungroup()
  
  # Anzahl der Infektionen
  daten_agent <- daten_agent %>%
    rowwise() %>%
    mutate(infektion = (sum(unlist(temp) < 0.05) > 0)) %>%
    ungroup()
  
  daten_agent <- daten_agent %>%
    select(- c(temp, contact_agent))
  
  return(daten_agent)
  
}

# Status aendern falls notwendig
infected_status_aendern_ALT <- function(daten_agent) {
  
  #TESTCODE
  # daten_agent <- agents
  
  # Infiziert wenn nicht removed (resistant)           
  daten_agent <- daten_agent %>%
    rowwise() %>%
    mutate(infected = if_else((infektion == TRUE) & (removed == FALSE), TRUE, infected)) %>%
    ungroup()
  
  # Nicht mehr waehlbar
  daten_agent <- daten_agent %>%
    rowwise() %>%
    mutate(susceptible = if_else(infected == TRUE, FALSE, susceptible)) %>%
    ungroup() %>%
    select(- infektion)
  
  return(daten_agent)
}




