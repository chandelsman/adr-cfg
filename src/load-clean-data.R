### ADR Report: Centers for Gastroenterology

# This script combines client provided records of screening colonoscopies with 
# LigoLab records of biopsies and assembles a dataset that is used to label 
# each record as a screening colonoscopy, tubular adenomoa, serrated adenoma, 
# malignancy, or needing manual interpretation. After manually verifying the 
# output, the combined data are used by the Rmarkdown script to produce the ADR 
# for the client. 

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(here)

# Import and clean data ---------------------------------------------------
# Client data provided by CFG -----
client_raw <-
  list.files(path = here("data"),
             pattern = "(\\d){4}\\D\\d-\\D\\d{2}\\.\\d+\\.xlsx",
             full.names = TRUE) %>%
  sapply(readxl::read_excel, simplify = FALSE) %>%
  bind_rows()

client_clean <-
  client_raw %>%
  mutate(
    dob_client = ymd(`Date of Birth`),
    match_name = str_extract(client_raw$`Patient Name`, "^.*,\\s..."),
    provider = `Provider Name`,
    dos_client = as_date(ymd_hms(`Exam Date`)),
    age = as.period(interval(start = dob_client, end = dos_client))$year
  ) %>%
  rename(sex = Gender) %>%
  filter(age >= 50) %>%
  select(provider, match_name, dos_client, dob_client, age, sex)

# Summit data from LigoLab -----
ligo_raw <-
  list.files(path = here("data"),
             pattern = "(\\d){4}\\D\\d-ligo",
             full.names = TRUE) %>%
  sapply(readxl::read_excel, simplify = FALSE) %>%
  bind_rows()

# Filter data to include relevant locations/clients
#   - CFG Harmony, CFG Process at Summit, and Harmony Surgery Center)

# Filter cases performed for CFG Harmony Surgery Center -----
# client is CFG (HARMONY), CFG (PROESS AT SUMMIT), or HARMONY SURGERY CENTER
ligo_harmony <-
  ligo_raw %>%
  filter(str_detect(
    client,
    regex("cfg \\(h.*|cfg \\(p|harmony surgery.*", ignore_case = TRUE)
  ))

# separate provider last name, patient name to title case, filter age >= 50 
ligo_clean <-
  ligo_harmony %>%
  mutate(
    dob_ligo = mdy(dob),
    dos_ligo = mdy(dos),
    age = as.period(interval(start = dob_ligo, end = dos_ligo))$year,
    match_name = str_to_title(name),
    provider = str_to_title(doctor),
    colon = grepl("colon", diagnosis, ignore.case = TRUE)
  ) %>%
  separate(provider,
           c("provider", "f_name"),
           sep = " ",
           extra = "drop") %>%
  filter(age >= 50, colon == TRUE) %>%
  select(-c(age, sex, provider, dob, dos, client, doctor, name, f_name, colon))

# Merge client and LigoLab data
# Resulting dataset includes **all** screening colonoscopies
colonoscopy <-
  client_clean %>%
  left_join(ligo_clean,
            by = c("match_name" = "match_name", "dos_client" = "dos_ligo"))
