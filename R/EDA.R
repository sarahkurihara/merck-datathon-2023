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