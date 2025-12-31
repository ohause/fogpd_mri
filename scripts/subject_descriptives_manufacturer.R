# This script is made to check for manufacturer, TR and b-values of create_subset_list.R script. The file was downloaded 12/25.
# author: Ole Hausendorf
# last changed: 12/25
# needed PPMI data downloads: scanner_type
# needed datatype: final_dataset from create_subset_list.R


# setup -------------------------------------------------------------------

library(tidyverse)
library(MatchIt)


remove(list = ls())
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

mainpath <- getwd()
datapath <- file.path(mainpath, "data")
outputpath <- file.path(mainpath, "output")


# loading data ------------------------------------------------------------

subjects_match_data <- read.csv(file.path(outputpath, "final_dataset.csv")) %>%
  select(PATNO, last_visit, matching)


manufacturers_data <- read.csv(file.path(datapath, "scanner_type_all_questionnaire.csv")) %>%
  filter(startsWith(Imaging.Protocol, "Manufacturer=")) %>%
  mutate(PATNO = Subject.ID) %>%
  select(!c(Description, Subject.ID)) %>%
  distinct() %>%
  left_join(subjects_match_data, by = "PATNO")

fmri_data <- read.csv(file.path(datapath, "scanner_type_all_questionnaire.csv")) %>%
  filter(startsWith(Imaging.Protocol, "TR=")) %>%
  mutate(PATNO = Subject.ID) %>%
  select(!c(Description, Subject.ID)) %>%
  distinct() %>%
  left_join(subjects_match_data, by = "PATNO")


dmri_data <- read.csv(file.path(datapath, "scanner_type_all_questionnaire.csv")) %>%
  filter(startsWith(Imaging.Protocol, "Gradient Directions=")) %>%
  mutate(PATNO = Subject.ID) %>%
  select(!c(Description, Subject.ID)) %>%
  distinct() %>%
  left_join(subjects_match_data, by = "PATNO")


# display descriptives ----------------------------------------------------

# DISPLAYING MANUFACTURERS
# SIEMENS
cat("SIEMENS subjects:", sum(manufacturers_data$Imaging.Protocol == "Manufacturer=SIEMENS"), "\n",
    "SIEMENS subjects, control:", sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==0] == "Manufacturer=SIEMENS"), "\n",
    "SIEMENS subjects, patient:", sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==1] == "Manufacturer=SIEMENS"), "\n")

# PHILIPS
cat("PHILIPS subjects:", sum(manufacturers_data$Imaging.Protocol == "Manufacturer=Philips") + sum(manufacturers_data$Imaging.Protocol == "Manufacturer=Philips Medical Systems"), "\n",
    "PHILIPS subjects, control:", sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==0] == "Manufacturer=Philips") + sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==0] == "Manufacturer=Philips Medical Systems"), "\n",
    "PHILIPS subjects, patient:", sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==1] == "Manufacturer=Philips") + sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==1] == "Manufacturer=Philips Medical Systems"), "\n")

# GE
cat("GE subjects:", sum(manufacturers_data$Imaging.Protocol == "Manufacturer=GE MEDICAL SYSTEMS"), "\n",
    "GE MEDICAL SYSTEMS subjects, control:", sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==0] == "Manufacturer=GE MEDICAL SYSTEMS"), "\n",
    "GE MEDICAL SYSTEMS subjects, patient:", sum(manufacturers_data$Imaging.Protocol[manufacturers_data$matching==1] == "Manufacturer=GE MEDICAL SYSTEMS"), "\n")


# DISPLAYING fMRI TR VALUES
# TR=2400—2750
cat("TR~2500 subjects:", sum(startsWith(fmri_data$Imaging.Protocol, "TR=2")), "\n",
  "TR~2500 subjects, control:", sum(startsWith(fmri_data$Imaging.Protocol[fmri_data$matching == 0], "TR=2")), "\n",
  "TR~2500 subjects, patient:", sum(startsWith(fmri_data$Imaging.Protocol[fmri_data$matching == 1], "TR=2")), "\n")

# TR<500
cat("TR<500 subjects:", sum(startsWith(fmri_data$Imaging.Protocol, "TR=4")), "\n",
    "TR<500 subjects, control:", sum(startsWith(fmri_data$Imaging.Protocol[fmri_data$matching == 0], "TR=4")), "\n",
    "TR<500 subjects, patient:", sum(startsWith(fmri_data$Imaging.Protocol[fmri_data$matching == 1], "TR=4")), "\n")

# TR~1000
cat("TR~1000 subjects:", sum(startsWith(fmri_data$Imaging.Protocol, "TR=1")), "\n",
    "TR~1000 subjects, control:", sum(startsWith(fmri_data$Imaging.Protocol[fmri_data$matching == 0], "TR=1")), "\n",
    "TR~1000 subjects, patient:", sum(startsWith(fmri_data$Imaging.Protocol[fmri_data$matching == 1], "TR=1")), "\n")


# DISPLAYING dMRI GRADIENT DIRECTIONS
# GradientDirections=30-32
cat("GD=30-32 subjects:", sum(startsWith(dmri_data$Imaging.Protocol, "Gradient Directions=3")), "\n",
    "GD=30-32 subjects, control:", sum(startsWith(dmri_data$Imaging.Protocol[dmri_data$matching == 0], "Gradient Directions=3")), "\n",
    "GD=30-32 subjects, patient:", sum(startsWith(dmri_data$Imaging.Protocol[dmri_data$matching == 1], "Gradient Directions=3")), "\n")

# GradientDirections=64
cat("GD=64 subjects:", sum(startsWith(dmri_data$Imaging.Protocol, "Gradient Directions=64")), "\n",
    "GD=64 subjects, control:", sum(startsWith(dmri_data$Imaging.Protocol[dmri_data$matching == 0], "Gradient Directions=64")), "\n",
    "GD=64 subjects, patient:", sum(startsWith(dmri_data$Imaging.Protocol[dmri_data$matching == 1], "Gradient Directions=64")), "\n")

# GradientDirections=0
cat("GD=0 subjects:", sum(startsWith(dmri_data$Imaging.Protocol, "Gradient Directions=0")), "\n",
    "GD=0 subjects, control:", sum(startsWith(dmri_data$Imaging.Protocol[dmri_data$matching == 0], "Gradient Directions=0")), "\n",
    "GD=0 subjects, patient:", sum(startsWith(dmri_data$Imaging.Protocol[dmri_data$matching == 1], "Gradient Directions=0")), "\n")


