set.seed(10)

# DROPOUTS
# Test in differences between age groups
df4$COHORT <- as.factor(with(df4, ifelse(AGE == "12-14" | AGE == "15-17" | AGE == "18-20", "Teen",
                                         ifelse(AGE == "21-24" | AGE == "25-29" | AGE == "30-34", "Young Adult", 
                                                ifelse(AGE == "35-39" | AGE == "40-44" | AGE == "45-49", "Adult", "Elder")))))


prop_test(df4, "Dropped out", COHORT, "Teen", "Young Adult", 1000)


# TRANSFERS
prop_test(df4, "Transferred", STFIPS, "VT", "MO", 1000)



# COMPLETIONS 
prop_test(df4, "Treatment completed", PSYPROB, "1", "2", 1000)
