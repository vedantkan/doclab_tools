library(tidyverse)

### Function to get gose from all the data

gose_func <- function(data){
  require(tidyverse)
  data <- tibble::as_tibble(data)
  df <- data %>%
    select(record_id, wlst, discharge_gose, follow_up_period.factor, gose) %>%
    group_by(record_id) %>%
    fill(discharge_gose, .direction = "updown") %>%
    fill(wlst, .direction = "updown") %>%
    filter(!(is.na(follow_up_period.factor) & is.na(gose))) %>%
    mutate(gose_3m = ifelse(follow_up_period.factor == "3 Month", gose, NA),
           gose_6m = ifelse(follow_up_period.factor == "6 Month", gose, NA),
           gose_12m = ifelse(follow_up_period.factor == "12 Month", gose, NA),
           gose_ot = ifelse(follow_up_period.factor == "Other", gose, NA)) %>%
    fill(gose_3m, .direction = "updown") %>%
    fill(gose_6m, .direction = "updown") %>%
    fill(gose_12m, .direction = "updown") %>%
    fill(gose_ot, .direction = "updown") %>%
    select(-gose, -follow_up_period.factor) %>%
    distinct() %>%
    ungroup() %>%
    mutate(best_gose = pmax(discharge_gose, gose_3m, gose_6m, gose_12m, gose_ot, na.rm = TRUE))

  return(df)
}


mrs_func <- function(data){
  require(tidyverse)
  data <- tibble::as_tibble(data)
  df <- data %>%
    select(record_id, wlst, discharge_mrs, follow_up_period.factor, mrs) %>%
    group_by(record_id) %>%
    fill(discharge_mrs, .direction = "updown") %>%
    fill(wlst, .direction = "updown") %>%
    filter(!(is.na(follow_up_period.factor) & is.na(mrs))) %>%
    mutate(mrs_3m = ifelse(follow_up_period.factor == "3 Month", mrs, NA),
           mrs_6m = ifelse(follow_up_period.factor == "6 Month", mrs, NA),
           mrs_12m = ifelse(follow_up_period.factor == "12 Month", mrs, NA),
           mrs_ot = ifelse(follow_up_period.factor == "Other", mrs, NA)) %>%
    fill(mrs_3m, .direction = "updown") %>%
    fill(mrs_6m, .direction = "updown") %>%
    fill(mrs_12m, .direction = "updown") %>%
    fill(mrs_ot, .direction = "updown") %>%
    select(-mrs, -follow_up_period.factor) %>%
    distinct() %>%
    ungroup() %>%
    mutate(best_mrs = pmin(discharge_mrs, mrs_3m, mrs_6m, mrs_12m, mrs_ot, na.rm = TRUE))

  return(df)
}


command_score_func <- function(data, date = "test_datetime", time = "test_datetime", type = "all"){

  require(tidyverse)
  data <- tibble::as_tibble(data)

  if(date == time){
    df <- data %>%
      select(record_id, !!sym(date), command_score) %>%
      filter(!(is.na(!!sym(date)) & is.na(command_score) )) %>%
      mutate(date = as.Date(stringr::str_sub(!!sym(date), 1, 10)),
             time = stringr::str_sub(!!sym(date), 12, ),
             .after = !!sym(date)) %>%
      select(-!!sym(date)) %>%
      group_by(record_id) %>%
      arrange(date, .by_group = TRUE) %>%
      mutate(command_yn = ifelse(any(command_score >= 4), 1, 0)) %>%
      ungroup()
  }
  else{
    df <- data %>%
      select(record_id, !!sym(date), !!sym(time), command_score) %>%
      filter(!(is.na(!!sym(date)) & is.na(!!sym(time)) & is.na(command_score))) %>%
      group_by(record_id) %>%
      rename(date = !!sym(date),
             time = !!sym(time)) %>%
      arrange(date, .by_group = TRUE) %>%
      mutate(command_yn = ifelse(any(command_score >= 4), 1, 0)) %>%
      ungroup()
  }

  if(type == "raw"){
    df <- df %>%
      select(-command_yn)
  }
  else if(type == "yesno"){
    df <- df %>%
      select(record_id, command_yn) %>%
      group_by(record_id) %>%
      distinct() %>%
      ungroup()
  }
  else if(type == "all"){
    df <- df
  }

  return(df)
}


crsr_func <- function(data, date = "test_datetime", time = "test_datetime", type = "all"){

  require(tidyverse)
  data <- tibble::as_tibble(data)

  if(date == time){
    df <- data %>%
      select(record_id, !!sym(date), crsr_auditory, crsr_visual, crsr_motor,
             crsr_oromotor_verbal, crsr_communication, crsr_arousal, crsr_total) %>%
      filter(!is.na(crsr_total)) %>%
      mutate(date = as.Date(stringr::str_sub(!!sym(date), 1, 10)),
             time = stringr::str_sub(!!sym(time), 12, ),
             .after = !!sym(date)) %>%
      select(-!!sym(date))
  }
  else{
    df <- data %>%
      select(record_id, !!sym(date), !!sym(time), crsr_auditory, crsr_visual, crsr_motor,
             crsr_oromotor_verbal, crsr_communication, crsr_arousal, crsr_total) %>%
      filter(!is.na(crsr_total)) %>%
      rename(date = !!sym(date),
             time = !!sym(time))
  }

  df <- df %>%
    mutate(cs_group = case_when(crsr_arousal == 0 & crsr_auditory < 3 &
                                  crsr_visual < 2 & crsr_motor < 3 &
                                  crsr_oromotor_verbal <= 1 & crsr_communication == 0 ~ "Coma",
                                crsr_arousal > 0 & crsr_auditory < 3 &
                                  crsr_visual < 2 & crsr_motor < 3 &
                                  crsr_oromotor_verbal < 3 & crsr_communication < 1 ~ "VS",
                                (crsr_visual > 1 | (crsr_motor > 2 & crsr_motor < 6)) &
                                  (crsr_auditory < 3 & crsr_oromotor_verbal < 3 &
                                     crsr_communication < 1) ~ "MCSm",
                                (crsr_auditory >= 3 | crsr_oromotor_verbal == 3 |
                                   crsr_communication == 1) &
                                  (crsr_communication < 2 & crsr_motor < 6) ~ "MCSp",
                                crsr_communication == 2 | crsr_motor == 6 ~ "CS")) %>%
    select(record_id, date, time, crsr_total, cs_group) %>%
    group_by(record_id) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(crsr_yn = ifelse(any(crsr_total >= 8), 1, 0),
           .after = crsr_total)

  if(type == "raw"){
    df <- df %>%
      select(-crsr_yn)
  }
  else if(type == "yesno"){
    df <- df %>%
      select(record_id, crsr_yn) %>%
      group_by(record_id) %>%
      distinct() %>%
      ungroup()
  }
  else if(type == "all"){
    df <- df
  }

  return(df)

}

demo_func <- function(data, database = "consciousness"){

  require(tidyverse)
  data <- tibble::as_tibble(data)

  if(database == "consciousness"){
    etiology <- "primary_adm_dx"
  }
  else if(database == "reconfig" | database == "aphasia"){
    etiology <- "ich_etiology_smashu"
  }

  df <- data %>%
    select(record_id, dob, sex.factor, race.factor, handedness.factor, years_of_education.factor,
           primary_adm_dx_onset, (ends_with("_date") & contains(c("hosp", "icu"))), !!sym(etiology) ) %>%
    group_by(record_id) %>%
    fill(all_of(colnames(.)[-1])) %>%
    distinct() %>%
    ungroup() %>%
    mutate(age = as.numeric(difftime(primary_adm_dx_onset, dob, units = "days")/365.25),
           .after = dob)

  if(length(unique(df$record_id)) == nrow(df) ){
    return(df)
  }
  else{
    return(NA)
  }

}


