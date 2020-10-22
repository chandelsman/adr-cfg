###############################################################################
##############################  ADR Report  ###################################
#####################  Centers for Gastroenterology  ##########################
############## screening colonoscopies by provider by month  ##################
###############################################################################

# ADR calculations

# set-up workspace
library(tidyverse)
library(lubridate)
library(readxl)
library(kableExtra)

# load data after manual entry of "evaluate" cases
adr_data <- read_excel("input/HarmonyADR_final.xlsx")

# calculate ADR for each provider as:
# sum of cases with tubular adenomas, tubulovillous adenomas, and carcinomas
# divided by the sum of all non-serrated adenomas and screening colonoscopies

# calculate numerator (i.e., performance met)
adr_num <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Numerator = sum(c(ta, ta_ssa)))

# calculate denominator (performance met + screening - performance not met)
adr_den <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Denominator = sum(c(ta, ta_ssa, screen)))

# calculate ADR (serrated adenoma only results are excluded)
adr <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(adr = (sum(c(ta, ta_ssa))) / (sum(c(ta, ta_ssa, screen))) * 100) %>%
  mutate_at(3, ~ round(., 2))

# merge numerator, denominator, and ADR into common table
adr_table <- adr_num %>%
  left_join(adr_den, by = c("provider", "sex")) %>%
  left_join(adr, by = c("provider", "sex")) %>%
  rename(Provider = provider, Sex = sex, `ADR (%)` = adr)

# calculate SSADR for each provider as:
# sum of tubular, tubulovillous, and serrated adenomas plus carcinomas
# divided by total screening colonoscopies

# calculate numerator (i.e., performance met + serrated adenomas)
ssadr_num <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Numerator = sum(c(ta_ssa, ssa)))

# calculate denominator (performance met + screening + serrated adenomas)
ssadr_den <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Denominator = sum(c(ta, ta_ssa, ssa, screen)))

# calculate SSADR (all serrated adenoma only results are included)
ssadr <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(
    ssadr = (sum(c(ta_ssa, ssa))) / (sum(c(ta, ta_ssa, ssa, screen))) * 100
  ) %>%
  mutate_at(3, ~ round(., 2))

# merge numerator, denominator, and SSADR into common table
ssadr_table <- ssadr_num %>%
  left_join(ssadr_den, by = c("provider", "sex")) %>%
  left_join(ssadr, by = c("provider", "sex")) %>%
  rename(Provider = provider, Sex = sex, `SSADR (%)` = ssadr)

# formatted tables
### ADR ###
kable(adr_table) %>%
  kable_styling(bootstrap_options = "stripe")

### SSADR ###
kable(ssadr_table) %>%
  kable_styling(bootstrap_options = "stripe")