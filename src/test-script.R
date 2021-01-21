# Classify adenomas using text in the diagnosis field
colonoscopy %>%
  mutate(diagnosis =
           str_replace_all(
             diagnosis,
             regex(
               "negative for hyperplastic and adenomatous change|negative for adenomatous change and malignancy",
               ignore_case = TRUE
             ),
             "xxxxx"
           )) %>%
  filter(str_detect(diagnosis, "xxxxx"))

colonoscopy %>%
  mutate(diagnosis = case_when(
    str_replace_all(diagnosis, regex("negative for hperplastic and adenomatous change",
          ignore_case = TRUE) ~ "xxxxx"
  )))

