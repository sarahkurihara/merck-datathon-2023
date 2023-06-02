# Code by Sarah Kurihara
# Merck Datathon 2023

library(dplyr)
library(glmnet)
library(gamlr)
library(naniar)
library(ggplot2)

path = '../merck-2023-datathon/data/'
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

# ----------------------NO PRIOR ADMISSIONS MODEL---------------------------

# 1,323,141 rows 
df4 <- df3 %>% filter(NOPRIOR == "No Prior")

set.seed(1)
df4$id <- 1:nrow(df4)
df4$TRANS <- as.factor(with(df4, ifelse(REASON != "Transferred", 0, 1)))

train <- df4 %>% filter(COMPLETED != 1) %>% dplyr::sample_frac(0.70)
test  <- df4 %>% filter(COMPLETED != 1) %>% dplyr::anti_join(df4, train, by = 'id')

model1 <- glm(TRANS ~ AGE + SUB1 + PSOURCE, 
              family = binomial,
              data = train)

summary(model1)
coefs <- coef(model1)

test$preds <- predict(model1, test, type="response")

test <- test %>% 
  mutate(prediction = 1*(preds > .5)) %>% 
  mutate(accurate = 1*(prediction==TRANS))

#---------------------------PRIOR ADMISSIONS MODEL------------------------------

df5 <- df3 %>% filter(NOPRIOR == "Prior")


df5$id <- 1:nrow(df5)
train2 <- df5 %>% dplyr::sample_frac(0.70)
test2  <- dplyr::anti_join(df5, train2, by = 'id')

model2 <- glm(COMPLETED ~ SUB1 + SERVICES + DIVISION, 
              family = binomial,
              data = train2)

summary(model2)
coefs <- coef(model2)

test2$preds <- predict(model2, test2, type="response")

test2 <- test2 %>% 
  mutate(prediction = 1*(preds > .5)) %>% 
  mutate(accurate = 1*(prediction==COMPLETED))

acc2 <- sum(test2$accurate/nrow(test2))

#####################TESTS FOR INDEPENDENCE##########################

set.seed(10)

sample_VT <- df4 %>% filter(STFIPS == "VT") %>% sample_n(1000, replace = FALSE, prob = NULL)
sample_MO <- df4 %>% filter(STFIPS == "MO") %>% sample_n(1000, replace = FALSE, prob = NULL)

sample_CT %>% count(REASON)
sample_NY %>% count(REASON)

res <- prop.test(x = c(200, 9), n = c(1000, 1000))
res

######################## EDA #####################################

##TOTALS

df4 %>% group_by(REASON) %>% 
  summarize(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3))

df5 %>% group_by(REASON) %>% 
  summarize(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3))

# df4 %>% group_by(REASON, DISYR) %>% 
#   summarize(count = n()) %>% 
#   mutate(percent = round(count / sum(count), 3)) %>% 
#   filter(REASON == "Treatment completed") %>% 
#   ggplot(aes(x=REASON, y=percent, fill = DISYR)) +
#   geom_bar(stat = "identity") + 
#   ggtitle("Program Completions (No Prior Admission)")


#-------------------DROPOUTS (No Prior)-------------------------------------

# Do younger people drop out of treatment more than older people?

df4 %>% 
  group_by(AGE, REASON) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  filter(REASON == "Dropped out") %>% 
  ggplot(aes(x=AGE, y=percent)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  ggtitle("Percent of group who dropped out (new cases)") +
  theme_minimal()

sample_teens <- df4 %>% filter(AGE == "12-14" | AGE == "15-17" | AGE == "18-20") %>% 
  sample_n(1000, replace = FALSE, prob = NULL)

sample_yadults <- df4 %>% filter(AGE == "21-24" | AGE == "25-29" | AGE == "30-34") %>% 
  sample_n(1000, replace = FALSE, prob = NULL)

sample_teens %>% count(REASON)
sample_yadults %>% count(REASON)

res <- prop.test(x = c(281, 219), n = c(1000, 1000))
res

df4 %>% 
  filter(REASON == "Dropped out") %>% 
  filter(PSOURCE != "Unknown") %>% 
  group_by(AGE, PSOURCE) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  ggplot(aes(x=AGE, y=percent, fill=PSOURCE)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  ggtitle("Referrals by age group (new cases)") +
  theme_minimal()

df4 %>% 
  filter(REASON == "Dropped out") %>% 
  group_by(AGE, SERVICES) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  ggplot(aes(x=AGE, y=percent, fill=SERVICES)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  ggtitle("Services by age group (new cases)") +
  theme_minimal()

df4 %>%
  filter(REASON == "Dropped out") %>%
  group_by(AGE, SUB1) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count), 3)) %>%
  filter(percent > 0.05) %>% 
  ggplot(aes(x=AGE, y=percent, fill=SUB1)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  guides(fill = guide_legend(title = "Substance")) +
  ggtitle("Substances used by dropouts (new cases)") +
  theme_minimal()


# df5 %>% 
#   group_by(AGE, REASON) %>%
#   summarise(count = n()) %>% 
#   mutate(percent = round(count / sum(count), 3)) %>% 
#   filter(REASON == "Dropped out of treatment") %>% 
#   ggplot(aes(x=AGE, y=percent)) +
#   geom_bar(stat = "identity") + 
#   scale_y_continuous(labels = scales::percent) +
#   ggtitle("AGE vs. Dropout (Prior Admission)") 
# 

#-------------------TERMINATIONS (No Prior)-------------------------------------
df4 %>% 
  filter(REASON == "Terminated by facility") %>% 
  group_by(GENDER) %>% 
  count(SERVICES) %>% 
  ggplot(aes(x=GENDER, y=n, fill=SERVICES)) +
  geom_bar(stat = "identity") + 
  ggtitle("GENDER vs. Terminated (No Prior Admission - Services)")

#--------------------COMPLETIONS--------------------------------

df4 %>% count(STFIPS)

sample_CA <- df4 %>% filter(STFIPS == "CA") %>% 
  sample_n(500, replace = FALSE, prob = NULL)

sample_NY <- df4 %>% filter(STFIPS == "NY") %>% 
  sample_n(500, replace = FALSE, prob = NULL)

sample_CA %>% count(REASON)
sample_NY %>% count(REASON)

res <- prop.test(x = c(1, 3), n = c(500, 500))
res

#-------------------TRANSFERS -------------------------------------

between_groups <- function(df, group, subgroup, prior){
  df %>% 
    filter(REASON == group) %>%
    group_by({{subgroup}}) %>%
    summarise(count = n()) %>% 
    mutate(percent = round(count / sum(count), 3)) %>% 
    ggplot(aes(x=reorder({{subgroup}}, percent), y=percent)) +
    geom_bar(stat = "identity") +
    ggtitle(sprintf("BETWEEN GROUPS: %s (%s)", group, prior)) +
    coord_flip()
}

between_groups(df4, "Transferred", STFIPS, "New Cases")
between_groups(df5, "Transferred", STFIPS, "Repeat Cases")
between_groups(df3, "Transferred", STFIPS, "All Cases")

within_groups <- function(df, group, subgroup, prior){
  df %>% 
    group_by({{subgroup}}, {{group}}) %>%
    summarise(count = n()) %>% 
    mutate(percent = round(count / sum(count), 3)) 
    #filter(STFIPS == "Other tranquilizers" | STFIPS == "Methamphetamine/speed" | STFIPS == "Other stimulants")
    #filter(REASON == group) %>% 
    #ggplot(aes(x=reorder({{subgroup}}, percent), y=percent)) +
    #geom_bar(stat = "identity") + 
    #ggtitle(sprintf("WITHIN GROUPS: %s (%s)", group, prior)) +
    #coord_flip()
}

within_groups(df4, "Transferred", STFIPS, "New Cases")

df3 %>% 
  filter(NOPRIOR != "Unknown") %>% 
  filter(REASON != "Treatment completed") %>% 
  group_by(NOPRIOR, REASON) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  ggplot(aes(x=NOPRIOR, y=percent, fill=REASON)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Reason for Program Non-Completion") +
  theme_minimal()

# df4 %>% 
#   filter(REASON == "Transferred") %>%
#   group_by(DSMCRIT) %>%
#   summarise(count = n()) %>% 
#   mutate(percent = round(count / sum(count), 3)) %>% 
#   #filter(STFIPS == "MI" | STFIPS == "MO" | STFIPS == "IL")
#   ggplot(aes(x=reorder(DSMCRIT, percent), y=percent)) +
#   geom_bar(stat = "identity") + 
#   #ggtitle("BETWEEN GROUPS: ") +
#   coord_flip()

df4 %>% 
  group_by(DSMCRIT, REASON) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  #filter(STFIPS == "IL" | STFIPS == "MO" | STFIPS == "NM") %>% 
  filter(REASON == "Transferred") %>% 
  ggplot(aes(x=reorder(DSMCRIT, percent), y=percent)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("DSM Criteria") +
  ggtitle("Transfer rates based on DSM criteria (new cases)") +
  coord_flip() +
  theme_minimal()

df4 %>% 
  group_by(SUB1, REASON) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  filter(SUB1 == "Other opiates and synthetics" | SUB1 == "PCP" | SUB1 == "Alcohol") %>% 
  filter(REASON == "Transferred") %>% 
  filter(SUB1 != "Unknown") %>% 
  filter(SUB1 != "None") %>% 
  ggplot(aes(x=reorder(SUB1, percent), y=percent)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Substance") +
  ggtitle("Transfer rates based on substance (within groups)") +
  coord_flip() +
  theme_minimal()

df4 %>% 
  filter(REASON == "Transferred") %>% 
  filter(SUB1 != "Unknown") %>% 
  filter(SUB1 != "None") %>% 
  group_by(SUB1) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  #filter(percent > 0.05) %>% 
  filter(SUB1 == "Non-prescription methadone" | SUB1 == "Cocaine/crack" | SUB1 == "Other amphetamines") %>% 
  ggplot(aes(x=reorder(SUB1, percent), y=percent)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Substance") +
  ggtitle("Transfer rates based on primary substance (between groups)") +
  coord_flip() +
  theme_minimal()


df4 %>% 
  group_by(STFIPS, REASON) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  filter(STFIPS == "KY" | STFIPS == "NC" | STFIPS == "NM") %>% 
  filter(REASON == "Transferred") %>% 
  filter(percent > 0.20) %>% 
  ggplot(aes(x=reorder(STFIPS, percent), y=percent)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("State") +
  ggtitle("Transfer rates based on state (within groups)") +
  coord_flip() +
  theme_minimal()

df4 %>% 
  filter(REASON == "Transferred") %>% 
  group_by(STFIPS) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  filter(percent > 0.05) %>% 
  #filter(STFIPS == "NC" | STFIPS == "CA" | STFIPS == "KY") %>% 
  ggplot(aes(x=reorder(STFIPS, percent), y=percent)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("State") +
  ggtitle("Transfer rates based on primary state (between groups)") +
  coord_flip() +
  theme_minimal()

sample_CT <- df4 %>% filter(STFIPS == "CT") %>% sample_n(1000, replace = FALSE, prob = NULL)
sample_DE <- df4 %>% filter(STFIPS == "DE") %>% sample_n(1000, replace = FALSE, prob = NULL)

sample_CT %>% count(REASON)
sample_DE %>% count(REASON)

state <- prop.test(x = c(191, 161), n = c(1000, 1000))
state

sample_new <- df4 %>% 
  filter(PSYPROB == "Yes") %>% 
  sample_n(1000, replace = FALSE, prob = NULL)

sample_repeat <- df5 %>% sample_n(1000, replace = FALSE, prob = NULL)

sample_new %>% count(REASON)
sample_repeat %>% count(REASON)

comp <- prop.test(x = c(648, 352), n = c(1000, 1000))
trans <- prop.test(x = c(47, 294), n = c(1000, 1000))
drop <- prop.test(x = c(242, 256), n = c(1000, 1000))
term <- prop.test(x = c(29, 50), n = c(1000, 1000))

comp$p.value
trans$p.value
drop$p.value
term$p.value


# #-------------------Reason vs. Completion-----------------------------
# df4 %>% 
#   group_by(REASON) %>%
#   count(COMPLETED) %>% 
#   ggplot(aes(x=REASON, y=n, fill=COMPLETED)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("REASON vs. Program Completion (No Prior Admission)")
# 
# df5 %>% 
#   group_by(REASON) %>%
#   count(COMPLETED) %>% 
#   ggplot(aes(x=REASON, y=n, fill=COMPLETED)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("REASON vs. Program Completion (Prior Admission)")
# 
# #-------------------Age vs. Completion-----------------------------
# df4 %>% 
#   group_by(AGE) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=AGE, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Age vs. Program Completion (No Prior Admission")
# 
# df5 %>% 
#   group_by(AGE) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=AGE, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Age vs. Program Completion (Prior Admission)")
# 
# #-------------- Gender vs. Completion----------------------------
# df4 %>% group_by(GENDER) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=GENDER, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Gender vs. Program Completion (No Prior Admission)")
# 
# df5 %>% group_by(GENDER) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=GENDER, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Gender vs. Program Completion (Prior Admission)") 
# 
# #-------------- First Use vs. Completion----------------------------
# df4 %>% group_by(FRSTUSE1) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=FRSTUSE1, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("FRSTUSE1 vs. Program Completion (No Prior Admission)")
# 
# df5 %>% group_by(FRSTUSE1) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=FRSTUSE1, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("FRSTUSE1 vs. Program Completion (Prior Admission)") 
# 
# #-------------- ALCDRUG vs. Completion----------------------------
# df4 %>% group_by(ALCDRUG) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=ALCDRUG, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("ALCDRUG vs. Program Completion (No Prior Admission)")
# 
# df5 %>% group_by(ALCDRUG) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=ALCDRUG, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("ALCDRUG vs. Program Completion (Prior Admission)") 
# 
# #----------Year vs. Completion------------------------------
# df4 %>% group_by(DISYR) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=DISYR, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Year vs. Program Completion (No Prior Admission)")
# 
# df5 %>% group_by(DISYR) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=DISYR, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Year vs. Program Completion (Prior Admission)")
# 
# #--------------- DIVISION vs. Completion----------------------
# df4 %>% group_by(DIVISION) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=DIVISION, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Division vs. Program Completion (No Prior Admission)")
# 
# df5 %>% group_by(DIVISION) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=DIVISION, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("Division vs. Program Completion (Prior Admission)")
# 
# #-------------- DSMCRIT vs. Completion----------------------
# df5 %>% group_by(DSMCRIT, REASON) %>% 
#   filter(REASON != "Treatment completed" & REASON != "Other") %>% 
#   summarise(count = n()) %>% 
#   mutate(percent = round(count / sum(count), 3)) %>% 
#   ggplot(aes(x=DSMCRIT, y=percent, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("DSM Criteria vs. Program Non-Completion (Prior Admission)") +
#   coord_flip()
# 
# df5 %>% group_by(DSMCRIT) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=DSMCRIT, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("DSMCRIT vs. Program Completion (Prior Admission)") +
#   coord_flip()
# 
# #---------- STATE vs. Completion-------------------------
# df4 %>% group_by(STFIPS) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=STFIPS, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("STFIPS vs. Program Completion (No Prior Admission)") +
#   coord_flip()
# 
# df5 %>% group_by(STFIPS) %>%
#   count(REASON) %>% 
#   ggplot(aes(x=STFIPS, y=n, fill=REASON)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("STFIPS vs. Program Completion (Prior Admission)") +
#   coord_flip()
# 
# 
# #------------Length of Stay----------------------------------
# df4 %>% 
#   filter(COMPLETED == 0) %>% 
#   group_by(LOS, SERVICES) %>%
#   count(COMPLETED) %>% 
#   ggplot(aes(x=LOS, y=n, fill=SERVICES)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("LOS vs. Services (Not Completed, No Prior Admission)") 
# 
# df5 %>% 
#   filter(COMPLETED == 1) %>% 
#   group_by(LOS, SERVICES) %>%
#   count(COMPLETED) %>% 
#   ggplot(aes(x=LOS, y=n, fill=SERVICES)) +
#   geom_bar(position = "fill", stat = "identity") + 
#   ggtitle("LOS vs. Services (Completed, Prior Admission)") 
# 
# 
