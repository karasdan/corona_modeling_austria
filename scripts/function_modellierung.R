
#----------------------------------------------------------------
#----------------------------------------------------------------
#------------------------Model Functions-------------------------
# Zeitstempel fuer State hinterlegen
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

#----------------------------------------------------------------
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

#----------------------------------------------------------------
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

#----------------------------------------------------------------
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


#----------------------------------------------------------------
# Infektionsstatus aendern
infected_status_aendern_neu <- function(daten_agent, daten_kontakte) {
  
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
infected_status_aendern <- function(daten_agent) {
  
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




