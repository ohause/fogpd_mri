# This script is build to create a patient list of the subset of the Parkinson's Disease (PD) PPMI dataset including Freezing of Gait (FoG) patients and non-FoG patients. Setup variables can be changed for intended purpose.
# Author: Ole Hausendorf, last change 11/2025


# setup

library(tidyverse)
library(MatchIt)

remove(list = ls())
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
mainpath <- getwd()
datapath <- file.path(mainpath, "data")
outputpath <- file.path(mainpath, "output")


source(file.path(mainpath, "scripts/falls_freezing_questionnaire.R"))
rm(list = setdiff(ls(), c("fog_falls_list", "mainpath", "datapath", "outputpath")))


fog_count <- 1                  # minimum amount of fog needed in each patient for fog
no_fog_count <- 0               # maximum amount of fog in control group allowed
med_state <- "noON"             # use medication state ON, OFF, ON_and_OFF, noON (OFF & ON_and_OFF) or ALL for FoG subjects
cnt_dti_scans <- 1              # define number of available DWI scans for the SAME subject. Only "1" or "2" usable
dwi_scan_min <- 1.8             # defines minimum number of years apart for dwi scans. Only applicable if cnt_dti_scans == 2.
dwi_scan_max <- 2.2             # defines maximum number of years apart for dwi scans. Only applicable if cnt_dti_scans == 2.
ratio_matching <- 1             # ratio of control:fog_patients
add_fog_falls_extra <- 1        # add extra subjects with multiple fog from freezing_and_falls PPMI data. "0" or "1" available.
six_digit_subjects <- 1         # allow only 6-digit subjects "1" or allow all available subject IDs "0"
add_updrs_part2 <- 1            # adds MDS-UPDRS 2.13 "Freezing" from questionnaire to patients if "1". Otherwise "0".

excluded_patients <- c(102305, 120403, 123594, 130828, 182341, #BIDS conversion error
                       212766, 215976, 157589, 172084, 186755, 101680, 102027, 116870, 160822, 170179, 103809, #DWI missing/ corrupted
                       101186, 163420, 182427, 107648, 136600, 137424, 142629, 143419, 174131, 202671, 250240, #DWI corrupted, FOV error
                       218338, 113446, 116742, 142007, #fMRI missing/ corrupted
                       101174, 143192, 152582, 194971,  #DWI in LR and AP axis recorded
                       240389, 240509, 111383, 156676, 161628, 182745, 188170, 195159, 206833, 209141, 209270, 212042, 217828, 226603, 228551, 236950, 240385, 292820, #multishell subjects
                       101175, 114272) # subcortical atlases corrupted


# loading data ------------------------------------------------------------

raw_control_data <- read_csv(file.path(datapath, "Participant_Status_26Nov2025.csv"), show_col_types = FALSE)

pd_patients_list <- read_csv(file.path(datapath, "Participant_Status_26Nov2025.csv"), show_col_types = FALSE) %>%
  filter(COHORT_DEFINITION == "Parkinson's Disease") %>% # filter only for PD patients
  select(PATNO) # make dataset smaller

dti_subjects_bl_list <- read_csv(file.path(datapath, "dmri_bl_idaSearch_11_26_2025.csv"), show_col_types = FALSE) %>% #contains DTI data scans at BL
  filter(nchar(`Subject ID`) != 10) %>% # filter out meaningless subject IDs
  mutate(
    PATNO = as.numeric(`Subject ID`)
  ) %>%
  select(PATNO, Sex, Age) %>%
  distinct()

dti_subjects_all_list <- read.csv(file.path(datapath, "dmri_all_idaSearch_11_26_2025.csv")) %>%
  select(!Description)

consecutive_fog_data <- read_csv(file.path(datapath, "MDS-UPDRS_Part_III_26Nov2025.csv"), show_col_types = FALSE) %>%
  filter(PATNO %in% pd_patients_list$PATNO) %>% # filter for PD patients only
  filter(!EVENT_ID %in% c("SC", "ST")) %>% # SC = screening, ST = screening visit filtered out
  mutate(
    PDSTATE = replace_na(PDSTATE, "OFF")) %>% # setting all NA values to OFF values as no medication was given in visit
  select(PATNO, EVENT_ID, PDSTATE, NP3FRZGT) # make dataset smaller

last_visit_data <- read_csv(file.path(datapath, "MDS-UPDRS_Part_III_26Nov2025.csv"), show_col_types = FALSE) %>%
  filter(PATNO %in% pd_patients_list$PATNO) %>% # filter for PD patients only
  filter(!EVENT_ID %in% c("SC", "ST")) %>% # SC = screening, ST = screening visit filtered out
  #  filter(!is.na(PDSTATE)) %>%
  select(PATNO, EVENT_ID) %>%
  mutate(last_visit = as.numeric(gsub("[^0-9]", "", EVENT_ID))) %>%
  group_by(PATNO) %>%
  filter(last_visit == max(last_visit, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct() %>%
  select(!EVENT_ID)

# find all subjects with at least two dwi scans. For comparability: dwi_scan_min and dwi_scan_max in years apart
ordered_dti_subjects <- order(dti_subjects_all_list$Subject.ID)
cnt <- 1
dti_subjects_two_times <- c()

for (dti_scans in 1:(nrow(dti_subjects_all_list) - 1)) {  # Loop through rows except the last one
  if (dti_subjects_all_list$Subject.ID[ordered_dti_subjects[dti_scans]] == dti_subjects_all_list$Subject.ID[ordered_dti_subjects[dti_scans + 1]]) {
    if (dti_subjects_all_list$Age[ordered_dti_subjects[dti_scans + 1]] - dti_subjects_all_list$Age[ordered_dti_subjects[dti_scans]] > dwi_scan_min && dti_subjects_all_list$Age[ordered_dti_subjects[dti_scans + 1]] - dti_subjects_all_list$Age[ordered_dti_subjects[dti_scans]] <= dwi_scan_max) {
      tmp_dti_subject <- dti_subjects_all_list$Subject.ID[ordered_dti_subjects[dti_scans]]
      dti_subjects_two_times[cnt] <- tmp_dti_subject  # Append the matching subject ID
      cnt <- cnt + 1
    }
  }
}
dti_subjects_two_times <- unique(dti_subjects_two_times)

freeze_selfquest_data <- read_csv(file.path(datapath, "MDS_UPDRS_Part_II__Patient_Questionnaire_26Nov2025.csv"), show_col_types = FALSE) %>%
  filter(PATNO %in% pd_patients_list$PATNO) %>%
  filter(!EVENT_ID %in% c("SC", "ST", "BL")) %>%
  filter(PATNO %in% dti_subjects_bl_list$PATNO) %>%
  group_by(PATNO) %>%
  filter(NP2FREZ > 0) %>%
  ungroup() %>%
  distinct(PATNO)

# manipulating patient dataset ----------------------------------------------------

consecutive_fog_data <- consecutive_fog_data %>%
  pivot_wider(
    id_cols = PATNO,
    names_from = c(EVENT_ID, PDSTATE),
    values_from = NP3FRZGT,
    values_fn = mean) %>% # get dataset in wide format, using fog values
  rowwise() %>%
  mutate(sum_values = sum(c_across(2:last_col()) > 0, na.rm = TRUE)) %>% # calculate amount of fog values > 0
  filter(sum_values >= fog_count) %>% # filter out all patients with less than "fog_count" times fog
  ungroup() %>%
  mutate(across(everything(), ~ replace_na(., -99))) %>% # Replace NA with -99 for all columns
  select(PATNO, sum_values, sort(names(.)[-c(1, which(names(.) == "sum_values"))]))

# checking for medication status
on_columns <- grep("_ON$", names(consecutive_fog_data)) 
off_columns <- grep("_OFF$", names(consecutive_fog_data))

consecutive_fog_data <- consecutive_fog_data %>%
  rowwise() %>%
  mutate(MEDICATION = case_when(
    any(c_across(all_of(on_columns)) > 0) & any(c_across(all_of(off_columns)) > 0) ~ "ON_and_OFF",  # both ON and OFF states present
    any(c_across(all_of(on_columns)) > 0) ~ "ON",  # only ON state present
    any(c_across(all_of(off_columns)) > 0) ~ "OFF",  # only OFF state present
    TRUE ~ "NONE"  # no ON or OFF state present
  )) %>%
  ungroup() %>%
  filter(
    case_when(
      med_state == "noON" ~ MEDICATION %in% c("OFF", "ON_and_OFF"),
      med_state == "ALL" ~ TRUE,
      TRUE ~ MEDICATION == med_state
    )
  ) %>%
  filter(!(BL_ON > 0)) # filters out subjects with medication on BL

# patient dataset -------------------------------------------------------------

if (add_fog_falls_extra == 0) {
  tmp_patient_dataset <- consecutive_fog_data %>%
    filter(PATNO %in% dti_subjects_bl_list$PATNO) %>%
    select(PATNO)
  if (add_updrs_part2 == 1) { # add updrs 2.13 subjects with >0 if add_updrs_part2 == 1
    tmp_patient_dataset <- tmp_patient_dataset %>%
      bind_rows(freeze_selfquest_data) %>%
      filter(PATNO %in% dti_subjects_bl_list$PATNO)
  }
} else if (add_fog_falls_extra == 1) {
  fog_falls_list <- fog_falls_list %>%
    left_join(dti_subjects_bl_list, by = c("PATNO" = "PATNO"))
  
  tmp_patient_dataset <- consecutive_fog_data %>%
    bind_rows(fog_falls_list) %>%
    filter(PATNO %in% dti_subjects_bl_list$PATNO) %>%
    select(PATNO)
  
  if (add_updrs_part2 == 1) { # add updrs 2.13 subjects with >0 if add_updrs_part2 == 1
    tmp_patient_dataset <- tmp_patient_dataset %>%
      bind_rows(freeze_selfquest_data) %>%
      filter(PATNO %in% dti_subjects_bl_list$PATNO)
  }
}

# Apply the join based on the value of cnt_dti_scans
if (cnt_dti_scans == 1) {
  patient_dataset <- tmp_patient_dataset %>%
    left_join(dti_subjects_bl_list, by = c("PATNO" = "PATNO"))
} else if (cnt_dti_scans == 2) {
  patient_dataset <- tmp_patient_dataset %>%
    filter(PATNO %in% dti_subjects_two_times) %>%
    left_join(dti_subjects_bl_list, by = c("PATNO" = "PATNO"))
} else {
  stop("Error. Choose value of '1' or '2' for cnt_dti_scans")
}

# Continue with the next join and select desired columns
patient_dataset <- patient_dataset %>%
  left_join(last_visit_data, by = c("PATNO" = "PATNO")) %>%
  distinct()  %>%
  filter(!PATNO %in% c(144131, 243040)) %>% # screening and baseline error/ withdraw
filter(!PATNO %in% excluded_patients)

if (six_digit_subjects == 1) { # filter for 6-digit subjects if six_digit_subjects == 1
  patient_dataset <- patient_dataset %>%
    filter(PATNO > 99999)
}


# control dataset ---------------------------------

control_dataset = read_csv(file.path(datapath, "MDS-UPDRS_Part_III_26Nov2025.csv"), show_col_types = FALSE) %>%
  filter(PATNO %in% last_visit_data$PATNO) %>%
  group_by(PATNO) %>%
  summarize(
    sum_fog = sum(NP3FRZGT > 0, na.rm = TRUE) 
  ) %>%
  filter(sum_fog <= no_fog_count) %>% #filters out all subjects >no_fog_count
  ungroup() %>% 
  filter(PATNO %in% dti_subjects_bl_list$`PATNO`) #ensures DTI images at BL

if (cnt_dti_scans == 1) {
  control_dataset <- control_dataset %>%
    left_join(dti_subjects_bl_list, by = c("PATNO" = "PATNO"))
} else if (cnt_dti_scans == 2) {
  control_dataset <- control_dataset %>%
    filter(PATNO %in% dti_subjects_two_times) %>%
    left_join(dti_subjects_bl_list, by = c("PATNO" = "PATNO"))
} else {
  stop("Error. Choose value of '1' or '2' for cnt_dti_scans")
}

control_dataset <- control_dataset %>%
  left_join(last_visit_data, by = c("PATNO" = "PATNO")) %>%
  select(PATNO, Sex, Age, last_visit) %>%
  distinct() %>%
  filter(!PATNO %in% patient_dataset$PATNO) %>%
  filter(!PATNO %in% excluded_patients)

if (six_digit_subjects == 1) { # filter for 6-digit subjects if six_digit_subjects == 1
  control_dataset <- control_dataset %>%
    filter(PATNO > 99999)
}

# matching datasets -------------------------------------------------------

patient_dataset$matching <- 1
control_dataset$matching <- 0

matching_dataset <- rbind(patient_dataset, control_dataset)

match_model_nearest = matchit(
  matching ~ Sex + Age + last_visit,
  data = matching_dataset,
  method = "nearest",
  ratio = ratio_matching
)

final_dataset = match.data(match_model_nearest) %>%
  filter(!PATNO %in% excluded_patients)

t.test(final_dataset$Age[final_dataset$matching == 0], final_dataset$Age[final_dataset$matching == 1])
t.test(final_dataset$last_visit[final_dataset$matching == 0], final_dataset$last_visit[final_dataset$matching == 1])

# saving data -------------------------------------------------------------

write.csv(final_dataset, file.path(outputpath, "final_dataset.csv"))
if (cnt_dti_scans == 1) {
  writeLines(as.character(final_dataset$PATNO), con = file.path(outputpath, "subject_ids_bl_dwi_scan.txt"))
} else if (cnt_dti_scans == 2) {
  writeLines(as.character(final_dataset$PATNO), con = file.path(outputpath, "subject_ids_two_dwi_scans.txt"))
}

