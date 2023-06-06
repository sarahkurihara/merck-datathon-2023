######################## EDA #####################################

# Create dataset for new cases (No Prior) 
df4 <- df3 %>% filter(NOPRIOR == "No Prior")

df4 %>% group_by(REASON) %>% 
  summarize(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3))

#  Create dataset for repeat cases (Prior)
df5 <- df3 %>% filter(NOPRIOR == "Prior")

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

# Figure 1. Dropout Rate by Age: for each age group, plot the percent of people who dropped out.

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

# Figure 2. Services by Age: for each age group, plot the services attended.
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

# Figure 3. Primary Referral Sources by Age: for each age group, visualize who the primary referral source is.

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

# Figure 4. Substances Used by Age: for each age group, identify the primary substance used.

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

#-------------------TRANSFERS -------------------------------------

# Figure 5. Transfer Rates per DSM Criteria
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
 
between_groups(df4, "Transferred", STFIPS, "New Cases")
between_groups(df5, "Transferred", STFIPS, "Repeat Cases")
between_groups(df3, "Transferred", STFIPS, "All Cases")
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

# Figure 6a (substance - between analysis)
df4 %>% 
  filter(REASON == "Transferred") %>% 
  filter(SUB1 != "Unknown") %>% 
  filter(SUB1 != "None") %>% 
  group_by(SUB1) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  #filter(percent > 0.05) %>% 
  #filter(SUB1 == "Non-prescription methadone" | SUB1 == "Cocaine/crack" | SUB1 == "Other amphetamines") %>% 
  ggplot(aes(x=reorder(SUB1, percent), y=percent)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent) +
  ylab("Percent") +
  xlab("Substance") +
  ggtitle("Transfer rates based on primary substance (between groups)") +
  coord_flip() +
  theme_minimal()

# Figure 6b. (substance - within analysis)
df4 %>% 
  group_by(SUB1, REASON) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  #filter(SUB1 == "Other opiates and synthetics" | SUB1 == "PCP" | SUB1 == "Alcohol") %>% 
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

# Figure 7a (states - between analysis)
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

# Figure 7b (states - within analysis)
df4 %>% 
  group_by(STFIPS, REASON) %>%
  summarise(count = n()) %>% 
  mutate(percent = round(count / sum(count), 3)) %>% 
  #filter(STFIPS == "KY" | STFIPS == "NC" | STFIPS == "NM") %>% 
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
