library(tidyverse)



# importing all data used -------------------------------------------------

## all dispatches filtered to only 911 calls
dispatches_filtered <- read_csv("created_data/dispatches_filtered_cpd.csv")

## officer hours
## includes overtime unless you redo the script: create_panel_analysis_shifts_officer
officer_hours <- read_csv("analysis_data/officer_hours.csv")

## border districts are those districts adjacent to SST districts
border_districts <- read_csv("created_data/border_districts_final.csv") %>% 
  mutate(border_treatment = mdy(border_treatment))

## sst alerts are the number of shotspotter dispatches
sst_alerts <- read_csv("analysis_data/sst_dispatches_cpd.csv") %>% 
  filter(year < 2023) 

## rolloutdates are for the treatment of SST
rollout_dates <- read_csv("created_data/rollout_dates.csv") %>% 
  mutate(across(c(-1), ~mdy(.)))

## victims are victim injuries
victims <- read_csv("created_data/victim_injuries.csv")

## time-sensitive injuries are those that occur only from un-realized injuries
time_sensitive_injury <- read_csv("created_data/victims_descriptions.csv") %>% 
  filter(injury_possibility == 1)

## arrests and crimes both have arrests in them. Need to put in both
arrests <- read_csv("raw_data/arrests.csv") %>% 
  janitor::clean_names() 

crimes <- read_csv("created_data/crimes_cleaned.csv")


## this helps gets the arrests
arrests <- arrests %>% 
  mutate(firearm_arrest = if_else(str_detect(charges_description,"GUN|CARRY CONCL|CONCEAL|FIREARM"), 1, 0)) %>% 
  distinct(case_number, .keep_all = T) %>% 
  filter(!is.na(case_number)) %>% 
  mutate(arrests_data_arrest = 1)

## this helps gets the additional arrests that the arrests data
## for some reason does not have in them
crimes <- crimes %>% 
  distinct(case_number, .keep_all = T) %>% 
  filter(arrest == 1) %>%  ## filters to only those that have an arrest
  select(-arrest) %>% 
  mutate(crimes_data_arrest = 1)

arrests <- arrests %>% 
  full_join(crimes) %>% 
  select(-district) %>% 
  mutate(arrest = 1) 

# creating variables ------------------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(entry_to_dispatch = time_length(dispatch_date - entry_received_date, "seconds"),
         entry_to_onscene = time_length(on_scene_date - entry_received_date, "seconds"),
         dispatch_to_onscene = time_length(on_scene_date - dispatch_date, "seconds"),
         entry_to_close = time_length(close_completed_date - entry_received_date, "seconds"))

## there were some final codes missing. replaced with the initial codes
dispatches_filtered <- dispatches_filtered %>% 
  mutate(final_dispatch_code = if_else(is.na(final_dispatch_code), initial_dispatch_code,
                                       final_dispatch_code),
         final_dispatch_description = if_else(is.na(final_dispatch_description),
                                              initial_dispatch_description,
                                              final_dispatch_description))

## getting rid of any SST alert dispatches
dispatches_filtered <- dispatches_filtered %>% 
  filter(final_dispatch_code != "SST")

## getting rid of priority 0
# dispatches_filtered <- dispatches_filtered %>% 
#   filter(priority_code != 0)

## deleting the negative entry_to_dispatches - only 130 of these
dispatches_filtered <- dispatches_filtered %>% 
  filter(entry_to_dispatch > 0) 

## There are 3256 dispatch to onscenes that are less than or equal to 0.
## As of 6-1-23, I am deleting these. THey represent such a small portion of the data.
## I am creating other columns that get rid of this.
dispatches_filtered <- dispatches_filtered %>% 
  mutate(dispatch_to_onscene_less_than_zero = if_else(dispatch_to_onscene <=0, 1, 0))


dispatches_filtered <- dispatches_filtered %>% 
  filter(dispatch_to_onscene_less_than_zero ==0 | is.na(dispatch_to_onscene_less_than_zero))


# outliers ----------------------------------------------------------------

outliers <- dispatches_filtered %>% 
  summarize(across(c(entry_to_dispatch, entry_to_onscene),
                   list(mean = ~mean(., na.rm = T), sd = ~sd(., na.rm = T))),
            .by = priority_code) %>% 
  mutate(entry_to_dispatch_outlier = entry_to_dispatch_mean + 3 * entry_to_dispatch_sd,
         entry_to_onscene_outlier = entry_to_onscene_mean + 3 *entry_to_onscene_sd,
         .by = priority_code) %>% 
  select(priority_code,ends_with("outlier"))

dispatches_filtered <- dispatches_filtered %>% 
  left_join(outliers)

dispatches_filtered <- dispatches_filtered %>% 
  group_by(priority_code) %>% 
  mutate(entry_to_dispatch_outlier = if_else(entry_to_dispatch > entry_to_dispatch_outlier, 1, 0),
         entry_to_onscene_outlier = if_else(entry_to_onscene > entry_to_onscene_outlier, 1, 0)) %>% 
  ungroup()


## this will filter out any of the outliers
## you can remove this if you do not want the outliers
dispatches_filtered <- dispatches_filtered %>% 
  mutate(entry_to_onscene_outlier = if_else(is.na(entry_to_onscene_outlier),
                                            0, entry_to_onscene_outlier))


dispatches_filtered <- dispatches_filtered %>% 
  filter(entry_to_onscene_outlier != 1) %>% 
  filter(entry_to_dispatch_outlier != 1)


# creating indicators for certain crimes we care about --------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(shots_fired =if_else(final_dispatch_description == "SHOTS FIRED", 1, 0),
         domestic_distrub = if_else(final_dispatch_description == "DOMESTIC DISTURBANCE", 1, 0),
         person_wgun = if_else(final_dispatch_description == "PERSON WITH A GUN", 1, 0),
         person_shot = if_else(final_dispatch_description == "PERSON SHOT", 1, 0),
         domestic_battery = if_else(final_dispatch_description == "DOMESTIC BATTERY", 1, 0)) %>% 
  mutate(gun_crime_report = if_else(shots_fired == 1 | person_wgun == 1 | person_shot == 1, 1, 0))




# arrests -----------------------------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  left_join(arrests, join_by("rd" == "case_number"))

dispatches_filtered <- dispatches_filtered %>% 
  mutate(arrest_made = if_else(!is.na(arrest), 1, 0)) 

other_arrests_from_pivot <- dispatches_filtered %>% 
  distinct(rd, event_number) %>% 
  drop_na(rd) %>% 
  separate_rows(rd, sep = ",") %>% 
  mutate(arrest_made = if_else(rd %in% arrests$case_number, 1,0)) %>% 
  filter(arrest_made == 1)

dispatches_filtered <- dispatches_filtered %>% 
  mutate(arrest_made = if_else(arrest_made == 0 & event_number %in% other_arrests_from_pivot$event_number,
                               1, arrest_made)) 

dispatches_filtered <- dispatches_filtered %>% 
  mutate(gun_crime_arrest = if_else(gun_crime_report ==1 & arrest_made ==1, 1, 0)) %>%
  mutate(non_gun_crime_arrest = if_else(gun_crime_report == 0 & arrest_made ==1, 1, 0)) %>% 
  mutate(non_gun_report = if_else(gun_crime_report == 0, 1, 0)) %>% 
  mutate(across(c(gun_crime_arrest, gun_crime_report,
                  non_gun_crime_arrest, non_gun_report), ~replace_na(., 0))) 

## creating intervals by first watch, second watch, third watch.

dispatches_filtered <- dispatches_filtered %>% 
  mutate(time = hms::as_hms(entry_received_date), .before = 1) %>% 
  mutate(first_watch = if_else(between(time, hms::as_hms("00:00:00"), hms::as_hms("06:59:59")) |
                                 between(time, hms::as_hms("23:00:00"), hms::as_hms("23:59:59")) , 1, 0),
         second_watch = if_else(between(time, hms::as_hms("07:00:00"), hms::as_hms("14:59:59")), 1, 0),
         third_watch = if_else(between(time, hms::as_hms("15:00:00"), hms::as_hms("22:59:59")), 1, 0),
         .before = 1)



dispatches_filtered <- dispatches_filtered %>% 
  mutate(watch = case_when(
    first_watch == 1 ~1,
    second_watch == 1 ~2, 
    third_watch == 1 ~3,
    .default = NA
  )) 



# victim injuries ---------------------------------------------------------


time_sensitive_injury_gun <-  time_sensitive_injury %>% 
  filter(injury_possibility_gun == 1)
time_sensitive_injury_no_gun <- time_sensitive_injury %>% 
  filter(injury_possibility_gun == 0)

dispatches_filtered <- dispatches_filtered %>% 
  left_join(victims, join_by(rd == rd_no)) %>% 
  mutate(victim_injury = replace_na(victim_injury, 0)) %>% 
  mutate(victim_injury_and_arrest = if_else(victim_injury == 1 &
                                              arrest_made == 1, 1, 0),
         victim_injury_gun_crime = if_else(victim_injury ==1 & 
                                             gun_crime_report == 1,
                                           1, 0),
         victim_injury_no_gun_crime = if_else(victim_injury ==1 &
                                                gun_crime_report == 0, 
                                              1, 0)) 
## only time-sensitive calls
dispatches_filtered <- dispatches_filtered %>% 
  mutate(time_sensitive_call = if_else(final_dispatch_description %in% 
                                         time_sensitive_injury$final_dispatch_description,
                                       1,0 ),
         time_sensitive_call_no_gun = if_else(final_dispatch_description %in% 
                                                time_sensitive_injury_no_gun$final_dispatch_description,
                                              1,0 ),
         time_sensitive_call_gun = if_else(final_dispatch_description %in% 
                                             time_sensitive_injury_gun$final_dispatch_description,
                                           1,0 ),
         victim_injury_time_sensitive_call = if_else(time_sensitive_call == 1 &
                                                       victim_injury == 1, 1, 0),
         victim_injury_time_sensitive_call_no_gun = if_else(time_sensitive_call_no_gun == 1 &
                                                              victim_injury == 1, 1, 0),
         victim_injury_time_sensitive_call_gun = if_else(time_sensitive_call_gun == 1 &
                                                           victim_injury == 1, 1, 0)) 


# aggregating at daily level ----------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date)) 

## creating number of dispatches
dispatches_filtered <- dispatches_filtered %>% 
  group_by(district, date, priority_code) %>% 
  mutate(number_dispatches = n()) %>% 
  ungroup()

# attaching other data ----------------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  left_join(rollout_dates) %>% 
  mutate(treatment = if_else(shotspot_activate <= date, 1, 0), 
         treatment_sdsc = if_else(sdsc <= date, 1, 0 ),
         treatment_official = if_else(shotspot_activate_official <= date, 1, 0),
         treatment_first_shot = if_else(shotspot_activate_first_shot <= date, 1, 0),
         treatment_cpd = if_else(shotspot_activate_cpd <= date, 1, 0),
         .by = district) %>% 
  mutate(never_treated = if_else(is.na(treatment),1, 0), .by = district) %>% 
  mutate(treatment = if_else(is.na(treatment), 0, treatment
  ),
  treatment_sdsc = if_else(is.na(treatment_sdsc), 0 , treatment_sdsc),
  treatment_official = if_else(is.na(treatment_official), 0, treatment_official),
  treatment_first_shot = if_else(is.na(treatment_first_shot), 0, treatment_first_shot),
  .by = district)

dispatches_filtered <- dispatches_filtered %>% 
  mutate(hour = hour(entry_received_date)) 

## getting rid of columns we don't need so the data is smaller
dispatches_filtered <- dispatches_filtered %>% 
  select(-c(call_acknowledged_date, primary_unit,
            radio_zone, area_of_service, sector_of_service,
            ward, community_area, fbi_code, arrest_date,
            updated_on, race, id, block, iucr, primary_type, description,
            location_description, beat, x_coordinate, y_coordinate,
            year, month, year_month, domestic.y),
         -starts_with("charge"),
         -starts_with("service")) %>% 
  select(-starts_with("occurrence"),
         -c(mental, domestic.x, call_source, call_source_description,
            datetime, time, longitude, latitude, location)) %>% 
  relocate(event_number, entry_received_date, district, entry_to_onscene)


dispatches_filtered <- dispatches_filtered %>% 
  mutate(date = as_date(entry_received_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))


# attaching other data sets -----------------------------------------------


# number sst dispatches ---------------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  left_join(sst_alerts, join_by(date == date, district == district, year == year)) %>% 
  mutate(number_sst_dispatches =if_else(is.na(number_sst_dispatches), 0, number_sst_dispatches))



# border districts --------------------------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  left_join(border_districts, join_by(district == border_district)) %>% 
  group_by(district) %>% 
  mutate(shotspot_border_treatment = ifelse(date >= border_treatment,1, 0 )) %>% 
  ungroup() %>% 
  mutate(shotspot_border_treatment = ifelse(is.na(shotspot_border_treatment), 0, shotspot_border_treatment))


# officer hours -----------------------------------------------------------

officer_hours <- officer_hours %>% 
  mutate(date = as_date(date)) %>% 
  select(date, officer_hours, district)

dispatches_filtered <- dispatches_filtered %>% 
  left_join(officer_hours)



# omitting july 4/december 31/jan 1 ---------------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  filter(!(month ==7 & day == 4)) %>% 
  filter(!(month == 1 & day == 1)) %>% 
  filter(!(month == 12 & day == 31)) 

dispatches_filtered <- dispatches_filtered %>% 
  drop_na(final_dispatch_description)




# Post-Rejection JPUBE code -----------------------------------------------


# adding in further arrests from disposition ------------------------------

dispatches_filtered <- dispatches_filtered %>% 
  mutate(arrest_made = if_else(str_detect(final_disposition_code, "R") | arrest_made == 1, 1, 0)) %>% 
  mutate(arrest_made = replace_na(arrest_made, 0)) 



# Post Rejection AEJ: Policy ----------------------------------------------


dispatches_filtered_sf <- dispatches_filtered

dispatches_filtered <- dispatches_filtered %>% 
  filter(final_dispatch_description != "SHOTS FIRED")

write_csv(dispatches_filtered, file = "analysis_data/xxdispatches_clevel.csv")
write_csv(dispatches_filtered_sf, file = "analysis_data/xxdispatches_clevel_sf.csv")
