set.seed(10)

# Test for differences between states
sample_VT <- df4 %>% filter(STFIPS == "VT") %>% sample_n(1000, replace = FALSE, prob = NULL)
sample_MO <- df4 %>% filter(STFIPS == "MO") %>% sample_n(1000, replace = FALSE, prob = NULL)

sample_CT %>% count(REASON)
sample_NY %>% count(REASON)

res <- prop.test(x = c(200, 9), n = c(1000, 1000))
res

# Test for differences between ages
sample_teens <- df4 %>% filter(AGE == "12-14" | AGE == "15-17" | AGE == "18-20") %>% 
  sample_n(1000, replace = FALSE, prob = NULL)

sample_yadults <- df4 %>% filter(AGE == "21-24" | AGE == "25-29" | AGE == "30-34") %>% 
  sample_n(1000, replace = FALSE, prob = NULL)

sample_teens %>% count(REASON)
sample_yadults %>% count(REASON)

res <- prop.test(x = c(281, 219), n = c(1000, 1000))
res

# Test for differences between states

sample_CA <- df4 %>% filter(STFIPS == "CA") %>% 
  sample_n(500, replace = FALSE, prob = NULL)

sample_NY <- df4 %>% filter(STFIPS == "NY") %>% 
  sample_n(500, replace = FALSE, prob = NULL)

sample_CA %>% count(REASON)
sample_NY %>% count(REASON)

res <- prop.test(x = c(1, 3), n = c(500, 500))
res


sample_CT <- df4 %>% filter(STFIPS == "CT") %>% sample_n(1000, replace = FALSE, prob = NULL)
sample_DE <- df4 %>% filter(STFIPS == "DE") %>% sample_n(1000, replace = FALSE, prob = NULL)

sample_CT %>% count(REASON)
sample_DE %>% count(REASON)

state <- prop.test(x = c(191, 161), n = c(1000, 1000))
state

# Test for differences between completions for people with co-occuring mental disorders
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