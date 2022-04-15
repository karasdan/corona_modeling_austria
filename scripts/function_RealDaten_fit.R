
# Functions laden
source("function_modellierung.R")

#----------------------------------------------------------------

test_doubling_time <- function(Real_Daten_selected_f, 
                               Predict_Daten_seleceted_f, 
                               abTag_f,
                               bisTag_f){
  
  temp_doublingtimes <- c()
  
  for (i in abTag_f:(bisTag_f - 3)){
    
    # Zusammenfuegen 
    temp_ForDoublingTime <- Predict_Daten_seleceted_f %>%
      left_join(Real_Daten_selected_f, by = "Tag") %>%
      filter(Tag >= i & Tag < i + 4)
    
    Exp_fit_real <- 
      nls(AnzahlFaelleSum ~ a0 * exp(r * Tag), data = temp_ForDoublingTime, start = list(a0 = 0.5, r = 0.2))
    Exp_fit_predict <- 
      nls(Wert ~ a0 * exp(r * Tag), data = temp_ForDoublingTime, start = list(a0 = 0.5, r = 0.2))
    
    doubling_time_Real <- log(2)/as.numeric(coef(Exp_fit_real)[2])
    doubling_time_Predict <- log(2)/as.numeric(coef(Exp_fit_predict)[2])
    
    temp_doublingtimes <- c(temp_doublingtimes, (doubling_time_Real - doubling_time_Predict)^2)
    
  }
  
  return(sum(temp_doublingtimes))
  
}

#-----------------------Fitting Real Daten-----------------------

# Startwerte bezueglich Infektion waehlen!
initial_agents_erstellen <- function(daten_agent, 
                                     infectious, 
                                     infected, 
                                     quarantine, 
                                     removed) {
  
  #TESTCODE
  # daten_agent <- agents
  # infectious <- 6
  # infected <- 7
  # quarantine <- 1
  # removed <- 0
  
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
    indices_removed <- sample(1:length(daten_agent$Id_agent), removed)
    
    # Setze sie auf infectious = TRUE und susceptible = FALSE
    daten_agent$removed[indices_removed] <- TRUE
    daten_agent$susceptible[indices_removed] <- FALSE
    
    # waehle bestimmte Indices, die infektioese werden aus
    moegliche_indices <- setdiff(1:length(daten_agent$Id_agent), indices_removed)
    
    indices_quarantine <- sample(moegliche_indices, quarantine)
    
    # Setze sie auf infectious = TRUE und susceptible = FALSE
    daten_agent$quarantine[indices_quarantine] <- TRUE
    daten_agent$susceptible[indices_quarantine] <- FALSE
    
    moegliche_indices <- setdiff(moegliche_indices, indices_quarantine)
    
    # waehle bestimmte Indices, die infektioese werden aus
    indices_infectious <- sample(moegliche_indices, infectious)
    
    # Setze sie auf infectious = TRUE und susceptible = FALSE
    daten_agent$infectious[indices_infectious] <- TRUE
    daten_agent$susceptible[indices_infectious] <- FALSE
    
    moegliche_indices <- setdiff(moegliche_indices, indices_infectious)
    
    # waehle bestimmte Indices, die infektioese werden aus
    indices_infected <- sample(moegliche_indices, infected)
    
    # Setze sie auf infectious = TRUE und susceptible = FALSE
    daten_agent$infected[indices_infected] <- TRUE
    daten_agent$susceptible[indices_infected] <- FALSE
    
  }
  
  return(daten_agent)
  
}

# Function fuer Modell mit Quarantaene
corona_model_agent_based_with_quarantine_ForFitting <- function(agents_f, 
                                                                tage_f,
                                                                type_of_contactfunctions_f,
                                                                daten_wsk_f,
                                                                goverment_f,
                                                                detection_wsk_per_agent_f,
                                                                wsk_infection_hh_f = 0.25,
                                                                wsk_infection_other_f = 0.05,
                                                                zeiten_f = NULL,
                                                                plotting = FALSE,
                                                                division_kontakte_ff = 0.25){
  
  #TESTCODE
  # agents_f <- agents
  # tage_f <- tage
  # type_of_contactfunctions_f <- type_of_contactfunctions
  # daten_wsk_f <- daten_wsk
  # goverment_f <- goverment
  # detection_wsk_per_agent_f <- detection_wsk_per_agent
  # plotting <- TRUE
  
  if (is.null(zeiten_f) == TRUE){
    
    # Zeitstempel fuer infectious hinterlegen
    zeiten <- tibble(Id_agent = agents_f$Id_agent)
    zeiten <- zeit_erstellen_infiziert_infektioes(agents_f, zeiten)
    
  } else {
    
    zeiten <- zeiten_f
    
  }
  
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
                              Summe_ErkannteFaelle = sum(agents_f$quarantine))
  
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
        
        contacts <- temp_func(agents_not_quarantine, contacts, daten_wsk_f, goverment_day, 
                              division_kontakte_f = division_kontakte_ff)
        
      } else if (grepl("haushalt", funktion) == TRUE) {
        
        contacts <- temp_func(agents_not_quarantine, contacts)
        
      } else {
        
        contacts <- temp_func(agents_not_quarantine, contacts, goverment_day)
        
      }
      
    }
    
    # for (funktion in kontakt_functions) {
    #   
    #   temp_func <- get(funktion)
    #   
    #   if (grepl("freizeit", funktion) == TRUE) {
    #     
    #     contacts <- temp_func(agents_not_quarantine, contacts, daten_wsk_f, goverment_day)
    #     
    #   } else {
    #     
    #     contacts <- temp_func(agents_not_quarantine, contacts)
    #     
    #   }
    #   
    # }
    
    # Infected Status aendern
    agents_f <- infected_status_aendern_mit_quarantine(agents_f, contacts,
                                                       wsk_infection_hh = wsk_infection_hh_f,
                                                       wsk_infection_other = wsk_infection_other_f)
    
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
    
    vorherige_Summe <- tracker_cases %>%
      select(Tag, Summe_ErkannteFaelle) %>%
      filter(Tag == max(Tag))
    
    vorherige_Summe <- vorherige_Summe$Summe_ErkannteFaelle
    
    # Pro Tag Summe merken
    temp <- data.frame(Tag = day,
                       S = sum(agents_f$susceptible),
                       I = sum(agents_f$infected),
                       I_ = sum(agents_f$infectious),
                       Q = sum(agents_f$quarantine),
                       R = sum(agents_f$removed),
                       Neuinfektionen = neuinfektionen,
                       Summe_ErkannteFaelle = vorherige_Summe + neuinfektionen)
    
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
  
  return(list(TR = tracker_cases, AG = agents_f, ZE = zeiten))
  
}

# Monte Carlo Simulation
monte_carlo_simulation_ForFitting <- function(agents_ff, 
                                              tage_ff, 
                                              type_of_contactfunctions_ff,
                                              daten_wsk_ff,
                                              goverment_ff,
                                              detection_wsk_per_agent_ff,
                                              times_f,
                                              zeiten_ff = NULL,
                                              quarantine,
                                              wsk_infection_hh_ff = 0.25,
                                              wsk_infection_other_ff = 0.05,
                                              division_kontakte_fff = 0.25) {
  
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
      
      temp <- corona_model_agent_based_with_quarantine_ForFitting(agents_f = agents_ff,
                                                                  tage_f = tage_ff,
                                                                  type_of_contactfunctions_f = type_of_contactfunctions_ff,
                                                                  daten_wsk_f = daten_wsk_ff,
                                                                  goverment_f = goverment_ff,
                                                                  detection_wsk_per_agent_f = detection_wsk_per_agent_ff,
                                                                  zeiten_f = zeiten_ff,
                                                                  wsk_infection_hh_f = wsk_infection_hh_ff,
                                                                  wsk_infection_other_f = wsk_infection_other_ff,
                                                                  division_kontakte_ff = division_kontakte_fff)
      
      temp <- temp$TR
      
      temp$Durchlauf <- i
      
      results_MonteCarlo <- results_MonteCarlo %>%
        bind_rows(temp)
      
      cat(paste0(col_name, " von ", times_f, " ist erledigt!\n"))
      
    }
    
  }
  
  return(results_MonteCarlo)
  
}
