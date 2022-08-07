
# see: 'plan/Validatie transplantectomie.docx'

# Extract data -----------------------------------------------------------------------------------

d_rumc <- readxl::read_excel("data/radboudumc.xlsx",
                             sheet="NNYTP",
                             range="B1:Z18",
                             col_names=TRUE)
d_umcu <- readxl::read_excel("data/UMCU.xlsx",
                             sheet="Blad1",
                             range="A1:X88",
                             col_names=TRUE)
d_vumc <- readxl::read_excel("data/vu.xlsx",
                             sheet="NAVTP",
                             range="A1:W25",
                             col_names=TRUE)
d_emc <- readxl::read_excel("data/emc.xlsx",
                            sheet="EMC",
                            range="A1:AE271",
                            col_names=TRUE)
d_amc <- readxl::read_excel("data/AMC_def.xlsx",
                            sheet="NAWTP",
                            range="A2:AF185")

# Transform -----------------------------------------------------------------------------------

## RUMC ----

names(d_rumc) <- names(d_rumc) %>%
  stringr::str_replace_all(., " ","_") %>%
  stringr::str_to_lower(.)
d_rumc <- d_rumc %>% 
  dplyr::mutate( 
    aantal_rejecties = as.numeric(aantal_rejecties),
    recipient_date_of_birth = as.Date(recipient_date_of_birth, format="%d/%m/%Y"),
    initial_graft_fail_date = as.Date(initial_graft_fail_date, format="%d/%m/%Y"),
    reden_transplantectomie = factor(reden_transplantectomie,
                                     levels=1:8,
                                     labels=c("graft intolerance", 
                                              "infectie",
                                              "ruimte maken",
                                              "persistent ns",
                                              "maligniteit",
                                              "hypertensie", 
                                              "nierstenen",
                                              "nos")),
    reden_einde_follow_up = factor(reden_einde_follow_up,
                                   levels=1:4,
                                   labels=c("overlijden",
                                            "re-transplantatie",
                                            "einde follow-up",
                                            "transplantectomie")),
    recipient_date_of_death = as.Date(recipient_date_of_death, format="%d-%m-%Y"), 
    recipient_sex = factor(recipient_sex),
    type_of_donor = factor(type_of_donor),
    type_cadaveric = factor(type_cadaveric),
    type_nhb = factor(nhb_category),
    initial_graft_fail_cause = factor(initial_graft_fail_cause),
    initial_pre_emptive_transplant = factor(initial_pre_emptive_transplant),
    graft_survival_in_dagen = as.numeric(difftime(initial_graft_fail_date,date_of_transplant, units="days")),
    across(starts_with("dat"), ~ as.Date(.x, format="%d/%m/%Y")),
    center = "rumc"
  )
d_rumc <- d_rumc %>%
  dplyr::select(et_recipient_number,
                aantal_rejecties, # predictive feature
                date_of_transplant, # graft survival: predictive feature
                recipient_date_of_birth, # age at graft failure: background
                initial_graft_fail_date, # graft survival: predictive feature
                transplantectomie, # event
                datum_transplantectomie, # time to event
                reden_transplantectomie, # event
                datum_einde_follow_up, # time to event (administrative censoring)
                reden_einde_follow_up, # event 
                recipient_date_of_death, # time to event (competing event)
                recipient_cause_of_death, # event
                recipient_sex, # background
                type_of_donor, # background
                type_cadaveric, # background
                type_nhb, # background
                donor_age, # predictive feature
                center
                )
d_rumc <- d_rumc[, order(names(d_rumc))]

## UMCU ----

names(d_umcu) <- names(d_umcu) %>%
  stringr::str_replace_all(., " ","_") %>%
  stringr::str_to_lower(.)
d_umcu <- d_umcu %>% 
  dplyr::mutate( 
    aantal_rejecties = as.numeric(aantal_rejecties),
    recipient_date_of_birth = as.Date(recipient_date_of_birth, format="%m/%d/%Y"),
    date_of_transplant = as.Date(date_of_transplant, format="%m/%d/%Y"),
    initial_graft_fail_date = as.Date(initial_graft_fail_date...5, format="%m/%d/%Y"),
    datum_transplantectomie = as.Date(datum_transplantectomie, format="%m/%d/%Y"),
    reden_transplantectomie = factor(reden_transplantectomie,
                                     levels=1:8,
                                     labels=c("graft intolerance", 
                                              "infectie",
                                              "ruimte maken",
                                              "persistent ns",
                                              "maligniteit",
                                              "hypertensie", 
                                              "nierstenen",
                                              "nos")),
    datum_einde_follow_up = as.Date(datum_laatste_follow_up, format="%m/%d/%Y"),
    reden_einde_follow_up = factor(reden_einde_follow_up,
                                   levels=1:4,
                                   labels=c("overlijden",
                                            "re-transplantatie",
                                            "einde follow-up",
                                            "transplantectomie")),
    recipient_date_of_death = as.Date(recipient_date_of_death, format="%d-%m-%Y"), 
    recipient_sex = factor(recipient_sex),
    type_of_donor = factor(type_of_donor),
    type_cadaveric = factor(type_cadaveric),
    type_nhb = factor(nhb_category),
    initial_graft_fail_date = as.Date(initial_graft_fail_date, format="%d-%m-%Y"),
    initial_graft_fail_cause = factor(initial_graft_fail_cause),
    initial_pre_emptive_transplant = factor(initial_pre_emptive_transplant),
    across(starts_with("dat"), ~ as.Date(.x, format="%d/%m/%Y")),
    center = "umcu"
  )
d_umcu <- d_umcu %>%
  dplyr::select(et_recipient_number,
                aantal_rejecties, # predictive feature
                date_of_transplant, # graft survival: predictive feature
                recipient_date_of_birth, # age at graft failure: background
                initial_graft_fail_date, # graft survival: predictive feature
                transplantectomie, # event
                datum_transplantectomie, # time to event
                reden_transplantectomie, # event
                datum_einde_follow_up, # time to event (administrative censoring)
                reden_einde_follow_up, # event 
                recipient_date_of_death, # time to event (competing event)
                recipient_cause_of_death, # event
                recipient_sex, # background
                type_of_donor, # background
                type_cadaveric, # background
                type_nhb, # background
                donor_age, # predictive feature
                center
  )

# Non overlapping variables
names(d_rumc)[-which(names(d_rumc) %in% names(d_umcu))]
names(d_umcu)[-which(names(d_umcu) %in% names(d_rumc))]

# reorder data to match d_rumc
d_umcu <- d_umcu[ ,order(names(d_umcu))]

# assert that all variables are of the same type
all.equal(d_rumc, d_umcu)

## VUmc ----------------------------------------------------------------------------------------

names(d_vumc) <- names(d_vumc) %>%
  stringr::str_replace_all(., " ","_") %>%
  stringr::str_to_lower(.)
d_vumc <- d_vumc %>% 
  dplyr::mutate( 
    aantal_rejecties = as.numeric(aantal_rejecties),
    recipient_date_of_birth = as.Date(recipient_date_of_birth, format="%m/%d/%Y"),
    date_of_transplant = as.Date(date_of_transplant, format="%m/%d/%Y"),
    initial_graft_fail_date = as.Date(initial_graft_fail_date...5, format="%m/%d/%Y"),
    datum_transplantectomie = as.Date(datum_transplantectomie, format="%m/%d/%Y"),
    reden_transplantectomie = factor(reden_transplantectomie,
                                     levels=1:8,
                                     labels=c("graft intolerance", 
                                              "infectie",
                                              "ruimte maken",
                                              "persistent ns",
                                              "maligniteit",
                                              "hypertensie", 
                                              "nierstenen",
                                              "nos")),
    datum_einde_follow_up = as.Date(datum_laatste_follow_up, format="%m/%d/%Y"),
    reden_einde_follow_up = factor(reden_einde_follow_up,
                                   levels=1:4,
                                   labels=c("overlijden",
                                            "re-transplantatie",
                                            "einde follow-up",
                                            "transplantectomie")),
    recipient_date_of_death = as.Date(recipient_date_of_death, format="%m/%d/%Y"), 
    recipient_sex = factor(recipient_sex),
    type_of_donor = factor(type_of_donor),
    type_cadaveric = factor(type_cadaveric),
    # type_nhb = factor(nhb_category),
    initial_graft_fail_date = as.Date(initial_graft_fail_date, format="%d-%m-%Y"),
    initial_graft_fail_cause = factor(initial_graft_fail_cause),
    initial_pre_emptive_transplant = factor(initial_pre_emptive_transplant),
    center = "vumc"
  )
d_vumc <- d_vumc %>% 
  dplyr::rename(transplantectomie = `transplantectomie?`,
         graft_survival_in_dagen = graft_survival_in_days)
d_vumc <- d_vumc %>%
  dplyr::select(et_recipient_number,
                aantal_rejecties, # predictive feature
                date_of_transplant, # graft survival: predictive feature
                recipient_date_of_birth, # age at graft failure: background
                initial_graft_fail_date, # graft survival: predictive feature
                transplantectomie, # event
                datum_transplantectomie, # time to event
                reden_transplantectomie, # event
                datum_einde_follow_up, # time to event (administrative censoring)
                reden_einde_follow_up, # event 
                recipient_date_of_death, # time to event (competing event)
                recipient_cause_of_death, # event
                recipient_sex, # background
                type_of_donor, # background
                type_cadaveric, # background
                # type_nhb, # background
                donor_age, # predictive feature
                center
  )

# Non overlapping variables
names(d_rumc)[-which(names(d_rumc) %in% names(d_vumc))]
names(d_vumc)[-which(names(d_vumc) %in% names(d_rumc))]
names(d_umcu)[-which(names(d_umcu) %in% names(d_vumc))]
names(d_vumc)[-which(names(d_vumc) %in% names(d_umcu))]

# reorder data to match d_rumc
d_vumc <- d_vumc[ ,order(names(d_vumc))]

# assert that all variables are of the same type
all.equal(d_rumc, d_vumc)

## EMC ---------------------------------------------------------------------------------------

names(d_emc) <- names(d_emc) %>%
  stringr::str_replace_all(., " ","_") %>%
  stringr::str_to_lower(.)
d_emc <- d_emc %>% 
  dplyr::mutate( 
    aantal_rejecties = as.numeric(totaal_rejecties),
    # recipient_date_of_birth = as.Date(recipient_date_of_birth, format="%m/%d/%Y"),
    et_recipient_number = as.character(et_recipient_number),
    date_of_transplant = as.Date(date_of_transplant, format="%m/%d/%Y"),
    initial_graft_fail_date = as.Date(initial_graft_fail_date, format="%m/%d/%Y"),
    datum_transplantectomie = as.Date(datumtransplantectomie, format="%m/%d/%Y"),
    reden_transplantectomie = factor(reden,
                                     levels=1:8,
                                     labels=c("graft intolerance", 
                                              "infectie",
                                              "ruimte maken",
                                              "persistent ns",
                                              "maligniteit",
                                              "hypertensie", 
                                              "nierstenen",
                                              "nos")),
    datum_einde_follow_up = as.Date(lastseendatum),
    reden_einde_follow_up = factor(lastseenhixreden,
                                   levels=1:4,
                                   labels=c("overlijden",
                                            "re-transplantatie",
                                            "einde follow-up",
                                            "transplantectomie")),
    recipient_date_of_death = as.Date(recipient_date_of_death, format="%m/%d/%Y"), 
    recipient_sex = factor(recipient_sex),
    type_of_donor = factor(type_of_donor),
    type_cadaveric = factor(type_cadaveric),
    type_nhb = factor(nhb_category),
    donor_age = as.numeric(`d-age`),
    initial_graft_fail_date = as.Date(initial_graft_fail_date, format="%d-%m-%Y"),
    # initial_graft_fail_cause = factor(initial_graft_fail_cause),
    initial_pre_emptive_transplant = factor(initial_pre_emptive_transplant),
    dialysis_tex = factor(`last_dialysis_technique_(renine)`),
    dgf = factor(`dgf_(m3)`),
    center = "emc"
  )

d_emc <- d_emc %>% 
  dplyr::rename(dialysis_days = `dialysis_days_(renine)`,
                graft_survival_in_dagen = tijd_falen_en_tect_in_dagen,
                recipient_age = age_recipient)
d_emc <- d_emc %>%
  dplyr::select(et_recipient_number,
                aantal_rejecties, # predictive feature
                date_of_transplant, # graft survival: predictive feature
                recipient_age, # age at graft failure: background
                initial_graft_fail_date, # graft survival: predictive feature
                transplantectomie, # event
                datum_transplantectomie, # time to event
                reden_transplantectomie, # event
                datum_einde_follow_up, # time to event (administrative censoring)
                reden_einde_follow_up, # event 
                recipient_date_of_death, # time to event (competing event)
                recipient_cause_of_death, # event
                recipient_sex, # background
                type_of_donor, # background
                type_cadaveric, # background
                type_nhb, # background
                donor_age, # predictive feature
                center
  )

# Non overlapping variables
names(d_rumc)[-which(names(d_rumc) %in% names(d_emc))]
names(d_emc)[-which(names(d_emc) %in% names(d_rumc))]
names(d_umcu)[-which(names(d_umcu) %in% names(d_emc))]
names(d_emc)[-which(names(d_emc) %in% names(d_umcu))]
names(d_vumc)[-which(names(d_vumc) %in% names(d_emc))]
names(d_emc)[-which(names(d_emc) %in% names(d_vumc))]

# reorder data to match d_rumc
d_emc <- d_emc[ ,order(names(d_emc))]

# assert that all variables are of the same type
all.equal(d_rumc, d_emc)

## AMC ----

names(d_amc) <- names(d_amc) %>%
  stringr::str_replace_all(., " ","_") %>%
  stringr::str_to_lower(.)
d_amc <- d_amc %>% 
  dplyr::mutate( 
    aantal_rejecties = as.numeric(aantal_rejecties),
    recipient_date_of_birth = as.Date(recipient_date_of_birth, format="%m/%d/%Y"),
    date_of_transplant = as.Date(date_of_transplant, format="%m/%d/%Y"),
    initial_graft_fail_date = as.Date(initial_graft_fail_date, format="%m/%d/%Y"),
    datum_transplantectomie = as.Date(datum_transplantectomie, format="%m/%d/%Y"),
    reden_transplantectomie = factor(reden_transplantectomie,
                                     levels=1:8,
                                     labels=c("graft intolerance", 
                                              "infectie",
                                              "ruimte maken",
                                              "persistent ns",
                                              "maligniteit",
                                              "hypertensie", 
                                              "nierstenen",
                                              "nos")),
    datum_einde_follow_up = as.Date(datum_laatste_follow_up, format="%m/%d/%Y"),
    reden_einde_follow_up = factor(reden_laatste_follow_up,
                                   levels=1:4,
                                   labels=c("overlijden",
                                            "re-transplantatie",
                                            "einde follow-up",
                                            "transplantectomie")),
    # recipient_date_of_death = as.Date(recipient_date_of_death, format="%m/%d/%Y"), 
    recipient_sex = factor(recipient_sex),
    type_of_donor = factor(type_of_donor),
    type_cadaveric = factor(type_cadaveric),
    type_nhb = factor(nhb_category),
    initial_graft_fail_date = as.Date(initial_graft_fail_date, format="%d-%m-%Y"),
    initial_graft_fail_cause = factor(initial_graft_fail_cause),
    initial_pre_emptive_transplant = factor(initial_pre_emptive_transplant),
    center = "amc"
  )

d_amc <- d_amc %>% 
  dplyr::rename(graft_survival_in_dagen = interval_tussen_falen_en_tect)
d_amc <- d_amc %>%
  dplyr::select(et_recipient_number,
                aantal_rejecties, # predictive feature
                date_of_transplant, # graft survival: predictive feature
                recipient_date_of_birth, # age at graft failure: background
                initial_graft_fail_date, # graft survival: predictive feature
                transplantectomie, # event
                datum_transplantectomie, # time to event
                reden_transplantectomie, # event
                datum_einde_follow_up, # time to event (administrative censoring)
                reden_einde_follow_up, # event 
                # recipient_date_of_death, # time to event (competing event)
                # recipient_cause_of_death, # event
                recipient_sex, # background
                type_of_donor, # background
                type_cadaveric, # background
                type_nhb, # background
                donor_age, # predictive feature
                center
  )
# Non overlapping variables
names(d_rumc)[-which(names(d_rumc) %in% names(d_amc))]
names(d_amc)[-which(names(d_amc) %in% names(d_rumc))]
names(d_umcu)[-which(names(d_umcu) %in% names(d_amc))]
names(d_amc)[-which(names(d_amc) %in% names(d_umcu))]
names(d_vumc)[-which(names(d_vumc) %in% names(d_amc))]
names(d_amc)[-which(names(d_amc) %in% names(d_vumc))]
names(d_emc)[-which(names(d_emc) %in% names(d_amc))]
names(d_amc)[-which(names(d_amc) %in% names(d_emc))]

# reorder data to match d_rumc
d_amc <- d_amc[ ,order(names(d_amc))]

# assert that all variables are of the same type
all.equal(d_rumc, d_amc)

# Load ----------------------------------------------------------------------------------------

d <- bind_rows(
  d_rumc,
  d_amc,
  d_emc,
  d_umcu, 
  d_vumc
)
rm(list = setdiff(ls(), "d"))

## Calculations ----
# Age patient at graft failure
d <- d %>% 
  dplyr::mutate(
    recipient_age = ifelse(
      is.na(recipient_age),
        difftime(initial_graft_fail_date, recipient_date_of_birth)/365.25,
        recipient_age
    )
)
# graft survival in months 
d <- d %>%
  dplyr::mutate(
    graft_survival = as.numeric(difftime(initial_graft_fail_date, date_of_transplant, units="days")/30.4375)
  )
# encode events: 
## 1: tect b/c of graft intolerance
## 2: tect for other reasons
## 3: re-transplant
## 4: death 
## 0: administrative censoring
d <- d %>%
  dplyr::mutate(
    event = factor(
        case_when(
        reden_transplantectomie == "graft intolerance" ~ 1,
        reden_transplantectomie == "infectie" ~ 2,
        reden_transplantectomie == "maligniteit" ~ 2,
        reden_transplantectomie == "nos" ~ 2,
        reden_transplantectomie == "ruimte maken" ~ 3,
        reden_einde_follow_up == "re-transplantatie" ~ 4,
        reden_einde_follow_up == "overlijden" ~ 5,
        reden_einde_follow_up == "einde follow-up" ~ 0,
        et_recipient_number == 100442 ~ 2,
        et_recipient_number == 068903 ~ 5
      ),
      labels = c(
        "censored",
        "graft intolerance",
        "other reasons",
        "create space",
        "re-transplantation",
        "death"
      )
    )  
  )

# follow-up time between graft failure until event.
d <- d %>%
  dplyr::mutate(
    time_event = as.numeric(
      case_when(
        event == "graft intolerance" ~ difftime(datum_transplantectomie, initial_graft_fail_date, units="days")/30.4375,
        event == "other reasons" ~ difftime(datum_transplantectomie, initial_graft_fail_date, units="days")/30.4375,
        event == "create space" ~ difftime(datum_transplantectomie, initial_graft_fail_date, units="days")/30.4375,
        event == "re-transplantation" ~ difftime(datum_einde_follow_up, initial_graft_fail_date, units="days")/30.4375,
        event == "death" ~ difftime(datum_einde_follow_up, initial_graft_fail_date, units="days")/30.4375,
        event == "censored" ~ difftime(datum_einde_follow_up, initial_graft_fail_date, units="days")/30.4375,
      )
    )
  )
# any rejection ipv aantal rejecties
d <- d %>% 
  dplyr::mutate(
    any_rejecties = ifelse(aantal_rejecties == 0, 0, 1)
  )
# decade of graft failure?
d <- d %>%
  dplyr::mutate(
    dec = as.factor(
      case_when(
        lubridate::year(date_of_transplant)  > 1970 
        & lubridate::year(date_of_transplant) <= 1980 ~ "1970-1980",
        lubridate::year(date_of_transplant)  > 1980 
        & lubridate::year(date_of_transplant) <= 1990 ~ "1980-1990",
        lubridate::year(date_of_transplant)  > 1990 
        & lubridate::year(date_of_transplant) <= 2000 ~ "1990-2000",
        lubridate::year(date_of_transplant)  > 2000 
        & lubridate::year(date_of_transplant) <= 2010 ~ "2000-2010",
        lubridate::year(date_of_transplant)  > 2010 
        & lubridate::year(date_of_transplant) <= 2020 ~ "2010-2020",
        lubridate::year(date_of_transplant)  > 2020 
        & lubridate::year(date_of_transplant) <= 2030 ~ "2020-2022",
      )
    )
  )


# Load ----------------------------------------------------------------------------------------

d <- d %>% dplyr::select(
  donor_age,
  et_recipient_number,
  recipient_cause_of_death,
  recipient_sex,
  reden_einde_follow_up,
  reden_transplantectomie,
  type_cadaveric,
  type_nhb,
  type_of_donor,
  recipient_age,
  graft_survival,
  aantal_rejecties,
  event,
  time_event,
  center,
  dec)

d <- d %>% filter(time_event >= 2.9)
missing <- d %>% 
  filter(is.na(donor_age) | 
           is.na(aantal_rejecties) | 
           is.na(graft_survival) | 
           is.na(time_event)) 
write.csv2(missing, file="output/missing.csv")
d <- d %>% drop_na(donor_age, aantal_rejecties, graft_survival, time_event)

