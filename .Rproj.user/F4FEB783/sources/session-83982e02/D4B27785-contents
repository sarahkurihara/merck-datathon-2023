# Code by Sarah Kurihara
# Merck Datathon 2023

library(dplyr)
library(tidyr)
library(tidyverse)
library(glmnet)
library(gamlr)
library(naniar)
library(ggplot2)

path = '../merck-datathon-2023/data/'
treatments <- read.csv(paste0(path, 'treatments_2017-2020.csv'))
populations <- read.csv(paste0(path, 'state_county_cbsa_population.csv'))

# Select columns of interest
cols_to_explore <- c("DISYR", "AGE", "GENDER", 
                     "STFIPS", "REGION", "DIVISION", 
                     "SERVICES", "DAYWAIT", "REASON", "LOS", "PSOURCE",
                     "NOPRIOR", "SUB1", "ALCDRUG", "PSYPROB", "DSMCRIT")

# 4049136 rows x 16 columns
treatments_reduced <- treatments[,cols_to_explore]

# Remove rows with unknown data points and convert to factor
#treatments_reduced[treatments_reduced == -9] <- NA

# 1068595 rows
#treatments_reduced <- na.omit(treatments_reduced)

# Convert to factor and re-code
treatments_reduced <- data.frame(lapply(treatments_reduced, factor))

levels(treatments_reduced$GENDER) <- c("Male", "Female", "Unknown")

levels(treatments_reduced$AGE) <- c("12-14",
                                    "15-17",
                                    "18-20",
                                    "21-24",
                                    "25-29",
                                    "30-34",
                                    "35-39",
                                    "40-44",
                                    "45-49",
                                    "50-54",
                                    "55-64",
                                    "65+")

levels(treatments_reduced$REGION) <- c("U.S. territories", "Northeast","Midwest","South", "West")

levels(treatments_reduced$DIVISION) <- c("U.S. territories",
                                         "New England",
                                         "Middle Atlantic",
                                         "East North Central", 
                                         "West North Central",
                                         "South Atlantic",
                                         "East South Central",
                                         "West South Central",
                                         "Mountain",
                                         "Pacific")

treatments_reduced$NOPRIOR <- recode(treatments_reduced$NOPRIOR, 
                                     "0" = "No Prior", 
                                     "1" = "Prior", 
                                     "-9" = "Unknown") 
                                    
levels(treatments_reduced$REASON) <- c("Treatment completed",
                                       "Dropped out",
                                       "Terminated",
                                       "Transferred",
                                       "Incarcerated",
                                       "Death",
                                       "Other")

levels(treatments_reduced$SERVICES) <- c("Detox, 24-hour, hospital inpatient",
                                          "Detox, 24-hour, free-standing residential",
                                          "Rehab/residential, hospital (non-detox)",
                                          "Rehab/residential, short term (30 days or fewer)", 
                                          "Rehab/residential, long term (more than 30 days)",
                                          "Ambulatory, intensive outpatient",
                                          "Ambulatory, non-intensive outpatient",
                                          "Ambulatory, detoxification")

levels(treatments_reduced$PSOURCE) <- c("Individual",
                                        "Alcohol/drug use care provider",
                                        "Other health care provider",
                                        "School",
                                        "Employer",
                                        "Other community",
                                        "Court",
                                        "Unknown")


levels(treatments_reduced$SUB1) <- c("None",
                                     "Alcohol",
                                     "Cocaine/crack",
                                     "Marijuana/hashish",
                                     "Heroin",
                                     "Non-prescription methadone",
                                     "Other opiates and synthetics",
                                     "PCP",
                                     "Hallucinogens",
                                     "Methamphetamine/speed",
                                     "Other amphetamines",
                                     "Other stimulants",
                                     "Benzodiazepines",
                                     "Other tranquilizers",
                                     "Barbiturates",
                                     "Other sedatives or hypnotics",
                                     "Inhalants",
                                     "Over-the-counter medications",
                                     "Other drugs",
                                     "Unknown")


levels(treatments_reduced$ALCDRUG) <- c("None",
                                        "Alcohol only",
                                        "Other drugs only",
                                        "Alcohol and other drugs")

treatments_reduced$DSMCRIT <- recode(treatments_reduced$DSMCRIT, 
                                      "1"= "Alcohol-induced disorder",
                                      "2"= "Substance-induced disorder", 
                                      "3"= "Alcohol intoxication",
                                      "4"= "Alcohol dependence",
                                      "5"= "Opioid dependence",
                                      "6"= "Cocaine dependence",
                                      "7"= "Cannabis dependence",
                                      "8"= "Other substance dependence", 
                                      "9" =  "Alcohol abuse",
                                      "10" = "Cannabis abuse",
                                      "11" = "Other substance abuse",
                                      "12" = "Opioid abuse",
                                      "13" = "Cocaine abuse",
                                      "14" = "Anxiety disorders",
                                      "15" = "Depressive disorders",
                                      "16" = "Schizophrenia/other psychotic disorders",
                                      "17" = "Bipolar disorders",
                                      "18" = "Attention deficit/disruptive behavior disorders ",
                                      "19" = "Other mental health condition",
                                      "-9" = "Unknown")


treatments_reduced$STFIPS <- recode(treatments_reduced$STFIPS, 
       "1" = "AL",
       "2" = "AK",
       "4" = "AZ",
       "5" = "AR",
       "6" = "CA",
       "8" = "CO",
       "9" = "CT",
       "10" = "DE",
       "11" = "DC",
       "12" = "FL",
       "13" = "GA",
       "15" = "HI",
       "16" = "ID",
       "17" = "IL",
       "18" = "IN",
       "19" = "IA",
       "20" = "KS",
       "21" = "KY",
       "22" = "LA",
       "23" = "ME",
       "24" = "MD",
       "25" = "MA",
       "26" = "MI",
       "27" = "MN",
       "28" = "MS",
       "29" = "MO",
       "30" = "MT",
       "31" = "NE",
       "32" = "NV",
       "33" = "NH",
       "34" = "NJ",
       "35" = "NM",
       "36" = "NY",
       "37" = "NC",
       "38" = "ND",
       "39" = "OH",
       "40" = "OK",
       "41" = "OR",
       "42" = "PA",
       "44" = "RI",
       "45" = "SC",
       "46" = "SD",
       "47" = "TN",
       "48" = "TX",
       "49" = "UT",
       "50" = "VT",
       "51" = "VA",
       "53" = "WA",
       "54" = "WV",
       "55" = "WI",
       "56" = "WY",
       "60" = "AS",
       "66" = "GU",
       "69" = "MP",
       "72" = "PR",
       "74" = "UM",
       "78" = "VI")

# Make a copy of dataset
patient_cohorts <- treatments_reduced
patient_cohorts$COMPLETED <- as.factor(with(patient_cohorts, ifelse(REASON != "Treatment completed", 0, 1)))

# Evaluate demographic factors in program completion
df3 <- patient_cohorts %>% 
  select(DISYR, COMPLETED, REASON,
         GENDER, AGE, PSYPROB,
         SUB1, ALCDRUG, NOPRIOR, SERVICES, LOS, PSOURCE,
         REGION, DIVISION, 
         DSMCRIT, STFIPS) 

#df3 %>% filter(NOPRIOR == "No Prior") %>% group_by(REASON) %>% count(COMPLETED) 

