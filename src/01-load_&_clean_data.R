###############################################################################
##############################  ADR Report  ###################################
#####################  Centers for Gastroenterology  ##########################
##################  Screening colonoscopies by provider  ######################
###############################################################################

# data cleaning

# set-up workspace
library(tidyverse)
library(lubridate)

# client data provided by Centers for Gastroenterology Harmony
# load raw data provided by client
client_z12.11 <- readr::read_csv("data/2020q3-Z12.11.csv")
client_z80.0 <- readr::read_csv("data/2020q3-Z80.0.csv")
client_raw <- bind_rows(client_z12.11, client_z80.0)

# client_raw <- client_raw[rowSums(is.na(client_raw)) != ncol(client_raw), ]
# client_raw <- slice(client_raw, 1:(n() - 2))

# clean client data and selct provider, date of service, patient sex
client_clean <- client_raw %>%
  mutate(
    dob_client = 
      ymd(parse_date_time2(`Date of Birth`, "mdy", cutoff_2000 = 20)),
    match_name = 
      str_extract(client_raw$`Patient Name`, "^.*,\\s..."),
    provider = str_to_title(`Proc Prov`),
    dos_client = mdy(`Key DOS`),
    age = as.period(interval(start = dob_client, end = dos_client))$year
  ) %>%
  rename(sex = Sex) %>%
  filter(age >= 50) %>%
  select(c(provider, match_name, dos_client, dob_client, age, sex))

# Summit Pathology data from LigoLab
# load raw data from CFG monthly ADR dynamic report
#   - report run for cases received during previous quarter
#     - result ID *SURG (CFG) and SURG OP (SP)
ligo_raw <- readxl::read_excel("data/adr_ligo.xls")

# filter data to include relevant locations/clients
#   - CFG Harmony, CFG Process at Summit, and Harmony Surgery Center)
loc_harmony <- ligo_raw %>%
  filter(str_detect(client, fixed("cfg (harmony", ignore_case = TRUE)))

loc_summit <- ligo_raw %>%
  filter(str_detect(client, fixed("cfg (process", ignore_case = TRUE)))

loc_harmony_surg <- ligo_raw %>%
  filter(str_detect(client, fixed("harmony surgery", ignore_case = TRUE)))

ligo_harmony <- bind_rows(loc_harmony, loc_summit, loc_harmony_surg)

# clean data
#   - separate doctor's last name, convert patient name to title case, age >=50
ligo_clean <- ligo_harmony %>%
  mutate(
    dob_ligo = mdy(dob),
    dos_ligo = mdy(dos),
    age = as.period(interval(start = dob_ligo, end = dos_ligo))$year,
    match_name = str_to_title(name),
    provider = str_to_title(doctor),
    colon = grepl("colon", diagnosis, ignore.case = TRUE)
  ) %>%
  separate(provider, c("provider", "f_name"), sep = " ", extra = "drop") %>%
  filter(age >= 50, colon == TRUE) %>%
  select(-c(age, sex, provider, dob, dos, client, doctor, name, f_name, colon))

# merge client and ligo data
# This is the data set with all screening colonoscopies
colonoscopy <- client_clean %>% left_join(ligo_clean,
  by = c(
    "match_name" = "match_name",
    "dos_client" = "dos_ligo"
  )
)

# Classify adenomas from diagnosis field
colon_class <- colonoscopy %>%
  mutate(
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for hyperplastic and adenomatous change",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for adenomatous change and malignancy",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for high-grade dysplasia and malignancy",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for high grade dysplasia and malignancy",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for malignancy",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for dysplasia and malignancy",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for adenomatous epithelium",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for active gastritis",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("no invasive carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("no invasive adenocarcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("no definitive carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("no definite invasive carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("no high-grade dysplasia or carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("no high-grade dysplasia or invasive carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for intestinal metaplasia, dysplasia, and carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for intestinal metaplasia, dysplasia and carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for high-grade dysplasia and invasive carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    diagnosis =
      str_replace_all(
        diagnosis,
        fixed("negative for dysplasia and carcinoma",
          ignore_case = TRUE
        ), "xxxxx"
      ),
    TA = str_detect(diagnosis, fixed("tubul", ignore_case = TRUE)) |
      str_detect(diagnosis, fixed("villous", ignore_case = TRUE)),
    SSA = str_detect(diagnosis, fixed("serrated", ignore_case = TRUE)),
    negative = str_detect(diagnosis, fixed("negative", ignore_case = TRUE)),
    carcinoma = str_detect(diagnosis, fixed("carcinoma", ignore_case = TRUE)),
    category =
      if_else(diagnosis %in% NA, "screen",
        if_else(carcinoma == T, "evaluate",
          if_else(TA == T & SSA == F, "ta",
            if_else(TA == F & SSA == F, "screen",
              if_else(TA == T & SSA == T & negative == F, "ta_ssa",
                if_else(TA == F & SSA == T & negative == F, "ssa",
                  if_else(TA == T & SSA == T & negative == T, "evaluate",
                    if_else(TA == F & SSA == T & negative == T, "evaluate",
                      "check_code"
                    )
                  )
                )
              )
            )
          )
        )
      )
  )

# write data to Excel file and manually evaluate unresolved classifications
writexl::write_xlsx(colon_class, "output/HarmonyADR_for_review.xlsx")

# write file containing all screening colonoscopies to send to Dr. Sears
writexl::write_xlsx(
  colonoscopy, "output/2020_Q2_colonoscopies_CFG-Harmony.xlsx")

###############################################################################
# manually assign all cases categorized as "evaluate" and save file as:
# Harmony_final.xlsx
# Open 02-CalculateADR.R to finish analysis
###############################################################################