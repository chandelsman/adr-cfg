###############################################################################
##############################  ADR Report  ###################################
#####################  Centers for Gastroenterology  ##########################
############## screening colonoscopies by provider by month  ##################
###############################################################################

# ADR calculations

# Setup
library(tidyverse)
library(lubridate)
library(kableExtra)

# Load data after manual entry of "evaluate" cases
adr_data <- readxl::read_excel("data/2020q3-ADR-final.xlsx")

# Calculate ADR for each provider as:
#   sum of cases with tubular adenomas, tubulovillous adenomas, and carcinomas
#   divided by the sum of all non-serrated adenomas and screening colonoscopies

# Calculate numerator (i.e., performance met)
adr_num <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Numerator = sum(c(ta, ta_ssa)))

# Calculate denominator (performance met + screening - performance not met)
adr_den <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Denominator = sum(c(ta, ta_ssa, screen)))

# Calculate ADR (serrated adenoma only results are excluded)
adr <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(adr = (sum(c(ta, ta_ssa))) / (sum(c(ta, ta_ssa, screen))))

# Merge numerator, denominator, and ADR into common table
adr_table <- adr_num %>%
  left_join(adr_den, by = c("provider", "sex")) %>%
  left_join(adr, by = c("provider", "sex")) %>%
  rename(
    Provider = provider,
    Sex = sex,
    ADR = adr
  ) %>% 
  ungroup()

# Calculate SSADR for each provider as:
#   sum of tubular, tubulovillous, and serrated adenomas plus carcinomas
#   divided by total screening colonoscopies

# Calculate numerator (i.e., performance met + serrated adenomas)
ssadr_num <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Numerator = sum(c(ta_ssa, ssa)))

# Calculate denominator (performance met + screening + serrated adenomas)
ssadr_den <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(Denominator = sum(c(ta, ta_ssa, ssa, screen)))

# Calculate SSADR (all serrated adenoma only results are included)
ssadr <- adr_data %>%
  select(provider, sex, category) %>%
  count(provider, sex, category) %>%
  pivot_wider(names_from = category, values_from = n) %>%
  group_by(provider, sex) %>%
  summarise(ssadr = (sum(c(ta_ssa, ssa))) / (sum(c(
    ta, ta_ssa, ssa, screen
  ))))
  
  # Merge numerator, denominator, and SSADR into common table
  ssadr_table <- ssadr_num %>%
  left_join(ssadr_den, by = c("provider", "sex")) %>%
  left_join(ssadr, by = c("provider", "sex")) %>%
  rename(
    Provider = provider,
    Sex = sex,
    SSADR = ssadr
  ) %>% 
  ungroup()    

# Formatted tables
### ADR ###
adr_table %>% 
  ungroup() %>% 
  gt(groupname_col = "Provider", 
     rowname_col = "Sex"
  ) %>% 
  tab_header(
    title = md("**ADR by Provider for All Screening Colonoscopies**")
  ) %>% 
  fmt_percent(
    columns = vars(ADR),
    decimals = 2
  ) %>% 
  cols_align(
    align = "right",
    columns = everything()
  ) %>% 
  tab_options(
    column_labels.font.weight = "bold",
    table.width = pct(65)
  )

### SSADR ###
ssadr_table %>% 
  ungroup() %>% 
  gt(groupname_col = "Provider", 
     rowname_col = "Sex"
  ) %>% 
  tab_header(
    title = md("**SSADR by Provider for All Screening Colonoscopies**")
  ) %>% 
  fmt_percent(
    columns = vars(SSADR),
    decimals = 2
  ) %>% 
  cols_align(
    align = "right",
    columns = everything()
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.width = pct(65)
  )
