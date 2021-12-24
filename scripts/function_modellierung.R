# Zeitstempel fuer State hinterlegen
zeitstempel_hinterlegen <- function(daten_agent, state) {
  
  #TESTCODE
  # daten_agent <- agents
  # state <- "infected"
  
  time_name <- paste0("time_", state)
  
  daten_agent <- daten_agent %>%
    mutate({{time_name}} := 0) 
  
  daten_agent <- daten_agent %>%
    mutate({{time_name}} := if_else(!!sym(state) == TRUE, 1, !!sym(time_name)))
  
  return(daten_agent)
}

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

kontakte_erstellen_neu <- function(daten_agent) {
  
  #TESTCODE
  # daten_agent <- agents
  
  kontakte <- data.frame(Id_agent = daten_agent$Id_agent, 
                         contacts = round(rgamma(length(daten_agent$Id_agent),6.11,1)))
  
  kontakt_agent <- data.frame(Id_agent = rep(daten_agent$Id_agent, kontakte$contacts),
                              Id_contact = sample(kontakte$Id_agent, sum(kontakte$contacts), replace = TRUE))
  
  return(kontakt_agent)
}

infected_status_aendern_neu <- function(daten_agent, daten_kontakte) {
  
  #TESTCODE
  # daten_agent <- agents
  # daten_kontakte <- kontakte
  
  temp1 <- daten_agent %>%
    select(Id_agent, infected) %>%
    rename(Id_contact = Id_agent)
  
  anzahl_infektioeser_kontakte <- daten_kontakte %>%
    full_join(temp1, by = "Id_contact") %>%
    group_by(Id_agent) %>%
    summarise(Anzahl = sum(infected)) %>%
    ungroup() %>%
    filter(Anzahl > 0)
  
  wsk_infektion <- data.frame(Id_agent = rep(anzahl_infektioeser_kontakte$Id_agent, anzahl_infektioeser_kontakte$Anzahl),
                              wsk = runif(sum(anzahl_infektioeser_kontakte$Anzahl), 0, 1))
  
  temp2 <- daten_agent %>%
    select(Id_agent)
  
  anzahl_infektion_pro_agent <- wsk_infektion %>%
    group_by(Id_agent) %>%
    mutate(infection = if_else(wsk < 0.05, TRUE, FALSE)) %>%
    summarise(Anzahl = sum(infection)) %>%
    ungroup() 
  
  status_infected <- anzahl_infektion_pro_agent %>%
    mutate(infected_new = if_else(Anzahl > 0, TRUE, FALSE)) %>%
    right_join(temp2, by = "Id_agent") %>% #damit ich alle Agents habe -> auch die die keinen Kontakt haben 
    select(- Anzahl) 
  
  status_infected <- status_infected %>%
    mutate(infected_new = if_else(is.na(infected_new), FALSE, infected_new)) %>%
    arrange(Id_agent)
  
  daten_agent <- daten_agent %>%
    arrange(Id_agent)
  
  daten_agent <- daten_agent %>%
    inner_join(status_infected, by = "Id_agent") %>%
    mutate(infected = if_else(infected == TRUE, infected, infected_new))
  
  daten_agent <- daten_agent %>%
    mutate(susceptible = if_else(infected, FALSE, susceptible)) %>%
    select(- infected_new)
  
  return(daten_agent)
  
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




