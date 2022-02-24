# Modelling outpatient care follow-up for patients with diabetes
## How can we use methods from operational research to test approaches to redesigning diabetic outpatient services?

#### Project Status: archived

## Project Description
Over the last decade demand for outpatient care in England has grown and the number of outpatient appointments has almost doubled from 54 to 94 million yearly attendances. Outpatient care currently represents the largest proportion of NHS contact with patients in a hospital setting and accounts for around £8 billion in yearly healthcare expenditure. The recent NHS Long Term Plan set out plans to fundamentally transform outpatient services, through offering digital appointments, shifting care outside into community settings. The aim is to reduce face-to-face outpatient visits by up to a third, equivalent to 30 million scheduled appointments per year.

The outpatient specialty ‘Diabetic Medicine’ alone accounted for 1.1% of all outpatient appointments in 2017/18 (or 1,074,013 appointments in total). The follow-up-to new ratio in this specialty was 5.4 during the same time period, indicating that many diabetes patients attend several hospital follow-ups. In the light of rising risk factors and an ageing population, the demand for diabetes outpatient care is likely to increase over time.  

Operational research methods offer the opportunity to test and evaluate the effect of different transformation strategies in silico, i.e. prior to implementation. In this project, we aim to develop a stochastic model to test alternative models of outpatient care intervals or discharge rates, on demand for diabetic clinic appointments over time.

## Data source
We used data from the Clinical Practice Research Datalink (CPRD) linked to Hospital Episode Statistics (HES), [ISAC protocol number 19_138](https://www.cprd.com/protocol/variation-healthcare-utilisation-across-primary-and-secondary-care-patients-type-1-and-type). 

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in The Health Foundation's Secure Data Environment, which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual was used. Variables labelled 'patid' do not refer to NHS IDs or other identifiable patient data.

## How does it work?
As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on other patient-level CPRD extracts. 

### Requirements
These scripts were written in R version (to be added) and RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)

### Getting started

The 'R' folder contains:
* to be added

## Useful references
* to be added

## Authors
* **Meetali Kakad**  -[@tali_md](https://twitter.com/tali_md?lang=en)
* **Fiona Grimm** - [@fiona_grimm](https://twitter.com/fiona_grimm) - [fiona-grimm](https://github.com/fiona-grimm)

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/Diabetes_outpatient_modelling/blob/master/LICENSE).

