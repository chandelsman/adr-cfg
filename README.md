# ADR Reporting for CFG
Adenoma Detection Rate Reports provided for Centers for Gastroenterology. Reporting is performed quarterly for CFG Harmony providers. Both ADR and SSADR are calculated using the 2018 CMS guidelines. We receive a files containing all colonoscopies performed from the client because we can only identify colonoscopies when tissue was sent to us. All cases with diagnosis codes Z12.11 or Z80.0 are included. We export data from the 'CFG ADR' dynamic report in LigoLab and use text searches in the diagnosis to identify tubular, tubulovillous, sessile, and malignant results.

*note that all screening colonoscopies are used and the analysis is not restricted to first time colonoscopies

## Overview

Quarterly adenoma detection rate report provided to Centers for Gastroenterology (CFG). The client sends us a list of all screening colonoscopies and we match their data to colonoscopies where biopsies were processed by Summit. ADR is calculated per provider with independent statistics for male and female patients. The resulting report is sent to the client.

## Data

Client data and LigoLab data are combined to produce the dataset for calculating ADR.

### Client data

All colonoscopies with diagnosis codes Z12.11 or Z80.0 are included in the analysis as screening colonoscopies. Data are received as two Excel files (requested quarterly from Cassie Seiler: [Catherine.Seiler@uchealth.org](mailto:Catherine.Seiler@uchealth.org))

Files from Cassie will be named Z12.11.xlsx and Z80.0.xlsx

- Add “YYYYqX-” as a prefix to the file name and move to IT > Projects > adr-cfg > data

### Summit data

Query is performed in LigoLab

- Reporting > Dynamic Reports > CFG Quarterly ADR
- Collected =  Last Quarter
- Result ID prefix = "*SURG (CFG)" and "SURG OP (SP)"
- export results as “YYYYqX-ligo.xls” to IT > Projects > adr-cfg > data

## Output

### Data Processing and classification of adenomatous tissue

- run 01_load-clean-data.R and save final dataset to data folder as "YYYYq#-ADR-review.xlsx"
- open file and save a copy to the data folder as "YYYYq#-ADR-scored.xlsx"
- adjust column widths and text wrapping so diagnosis can be read
- filter to show cases where the category is "evaluate"
- read the diagnosis and assign "ta", "ta_ssa", "ssa", "carcinoma", or "screen"
  - note that **site must be colon!** Other GI site diagnoses will be in the dataset
- save file and proceed to next step: Produce Report

### Produce Report

- knit the 02-CFG-ADR-report.Rmd script with parameters in RStudio
- open the output in a browser print to PDF at 80% scaling
- save the file to the "output" folder as "CFG-ADR-YYYY-Q#
- send final report to ~~Dr. Stephen Sears ([ssears@digestive-health.net](mailto:ssears@digestive-health.net))~~ Dr. Sean Caufield [scaufield@digestive-health.net](mailto:scaufield@digestive-health.net) and Kevin Seely (CEO)([kseely@cfgnoco.com](mailto:kseely@cfgnoco.com))
    - Dr. Sears requested to be removed on 2023-01-25