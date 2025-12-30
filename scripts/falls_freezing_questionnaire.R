# This script uses the "Determination of Freezing and Falls" from PPMI database to find participant IDs reporting Freezing. The file was downloaded 11/25
# author: Ole Hausendorf
# last changed: 11/25

#   - Why only 260 PATNOs of those two lists are identical?
#       - fog_falls_list subjects because of last 12 months question
#       - fog_patients_list subjects because of earlier start than 01/2019 as freezing_and_falls
#     -> results in new/ additional subjects!

# setup -------------------------------------------------------------------

library(tidyverse)
library(MatchIt)


remove(list = ls())
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

mainpath <- getwd()
datapath <- file.path(mainpath, "data")
outputpath <- file.path(mainpath, "output")


# loading data ------------------------------------------------------------


# DATA OF ALL SUBJECTS OF PPMI DATA
raw_patients_data <- read.csv(file.path(datapath, "MDS-UPDRS_Part_III_26Nov2025.csv"), header = T)

# LIST OF ALL SUBJECTS OF PPMI DATA
raw_patients_list <- raw_patients_data %>%
  select(PATNO) %>%
  distinct(PATNO)

# LIST FOR ALL PD PATIENTS IN PPMI DATA
pd_patients_list <- read_csv(file.path(datapath, "Participant_Status_26Nov2025.csv"), show_col_types = FALSE) %>%
  filter(COHORT_DEFINITION == "Parkinson's Disease") %>% # filter only for PD patients
  select(PATNO) # save only PATNO

# DATA OF ALL PD SUBJECTS WITH FREEZING IN LAST 12 MONTHS OR RECENTLY
fog_falls_data <- read.csv(file.path(datapath, "Determination_of_Freezing_and_Falls_26Nov2025.csv"), header = T) %>%
  filter(PATNO %in% pd_patients_list$PATNO) %>% # filter only for PD patients
  group_by(PATNO) %>%
  mutate(
    frzgt1w_sum = sum(FRZGT1W, na.rm = T),
    frzgt12m_sum = sum(FRZGT12M, na.rm = T)
  ) %>%
  filter(frzgt1w_sum != 0 | frzgt12m_sum != 0) %>% # filter for FoG values > 0 recently or last 12 months
  ungroup()
#NOTE:  FRGT1W = currently experiencing FoG (if > 0)
#       FRGT12M = experienced FoG in last 12 months (if > 0)

# LIST OF ALL PD SUBJECTS WITH FREEZING IN LAST 12 MONTHS OR RECENTLY
fog_falls_list <- fog_falls_data %>%
  select(PATNO) %>% # save only PATNO
  distinct(PATNO)

# DATA OF ALL FOG PD PATIENTS OF PPMI DATA
fog_patients_data <- read.csv(file.path(datapath, "MDS-UPDRS_Part_III_26Nov2025.csv"), header = T) %>%
  filter(PATNO %in% pd_patients_list$PATNO) %>% # filter for PD patients only
  filter(!EVENT_ID %in% c("SC", "ST")) %>% # SC = screening, ST = screening visit filtered out
  filter(PDSTATE %in% c("ON", "OFF")) %>% # filter out all patients visits with medication unclear
  filter(!NP3FRZGT == 101) %>% # filter out unmeasurable FoG values
  group_by(PATNO) %>%
  mutate(
    fog_sum = sum(NP3FRZGT, na.rm = T)) %>%
  ungroup() %>%
  filter(fog_sum > 0) %>%
  select(PATNO, EVENT_ID, PDSTATE, NP3FRZGT, fog_sum) # make dataset smaller

# LIST OF ALL FOG PD PATIENTS OF PPMI DATA
fog_patients_list <- fog_patients_data %>%
  select(PATNO) %>%
  distinct(PATNO)

# check for identical PATNOs of fog_falls_list & fog_patients_list
intersect(fog_falls_list, fog_patients_list)



