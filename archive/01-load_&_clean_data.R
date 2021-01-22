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

# Classify adenomas using text in the diagnosis field
neg_results <-
  str_c(
    c(
      "negative for hyperplastic and adenomatous change",
      "negative for adenomatous change and malignancy",
      "negative for high-grade dysplasia and malignancy",
      "negative for high grade dysplasia and malignancy",
      "negative for malignancy",
      "negative for dysplasia and malignancy",
      "negative for adenomatous epithelium",
      "negative for active gastritis",
      "no invasive carcinoma",
      "no invasive adenocarcinoma",
      "no definitive carcinoma",
      "no definite invasive carcinoma",
      "no high-grade dysplasia or carcinoma",
      "no high-grade dysplasia or invasive carcinoma",
      "negative for intestinal metaplasia, dysplasia, and carcinoma",
      "negative for intestinal metaplasia, dysplasia and carcinoma",
      "negative for high-grade dysplasia and invasive carcinoma",
      "negative for dysplasia and carcinoma"
    ),
    collapse = "|"
  )

colon_class <-
  colonoscopy %>%
  mutate(
    diagnosis =
      str_replace_all(diagnosis, regex(neg_results, ignore_case = TRUE),
                      "xxxxx"),
    qtr = quarter(dos_client),
    TA = str_detect(diagnosis, fixed("tubul", ignore_case = TRUE)) |
      str_detect(diagnosis, fixed("villous", ignore_case = TRUE)),
    SSA = str_detect(diagnosis, fixed("serrated", ignore_case = TRUE)),
    negative = str_detect(diagnosis, fixed("negative", ignore_case = TRUE)),
    carcinoma = str_detect(diagnosis, fixed("carcinoma", ignore_case = TRUE)),
    category = case_when(
      diagnosis %in% NA ~ "screen",
      carcinoma == TRUE ~ "evaluate",
      TA == TRUE  & SSA == FALSE ~ "ta",
      TA == FALSE & SSA == FALSE ~ "screen",
      TA == TRUE  & SSA == TRUE & negative == FALSE ~ "ta_ssa",
      TA == FALSE & SSA == TRUE & negative == FALSE ~ "ssa",
      TA == TRUE  & SSA == TRUE & negative == TRUE  ~ "evaluate",
      TA == FALSE & SSA == TRUE & negative == TRUE  ~ "evaluate",
      TRUE ~ "check_code"
    )
  )

# write data to Excel file and manually evaluate unresolved classifications
writexl::write_xlsx(colon_class, here("output", "2020qX-ADR-review.xlsx"))

# write file containing all screening colonoscopies to send to Dr. Sears
writexl::write_xlsx(colonoscopy,
                    here("output", "2020qX-colonoscopies-CFG-Harmony.xlsx"))

###############################################################################
# manually assign all cases categorized as "evaluate" and save file to data 
# folder as:
# YYYYq#-ADR-scored.xlsx
# Open 02-CFG_ADR_Report.Rmd to finish analysis
###############################################################################
