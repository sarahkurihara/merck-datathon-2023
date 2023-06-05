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