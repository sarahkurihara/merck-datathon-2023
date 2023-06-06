# Script to house functions used in project

# Function creates a bar chart to analyze the leading contributors of a given subgroup (column) 
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


within_groups <- function(df, group, subgroup, prior){
  df %>% 
    group_by({{subgroup}}, {{group}}) %>%
    summarise(count = n()) %>% 
    mutate(percent = round(count / sum(count), 3)) %>% 
    #filter(STFIPS == "Other tranquilizers" | STFIPS == "Methamphetamine/speed" | STFIPS == "Other stimulants") %>% 
    filter(REASON == group) %>%
    ggplot(aes(x=reorder({{subgroup}}, percent), y=percent)) +
    geom_bar(stat = "identity") +
    ggtitle(sprintf("WITHIN GROUPS: %s (%s)", group, prior)) +
    coord_flip()
}

# Hypothesis testing for various subgroups within transfers
prop_test <- function(df, reason, category, group_1, group_2, n){
  sample_1 <- df %>% filter({{category}} == group_1) %>% 
    sample_n(n, replace = FALSE, prob = NULL) %>% 
    count(REASON == reason)
  sample_2 <- df %>% filter({{category}} == group_2) %>% 
    sample_n(n, replace = FALSE, prob = NULL) %>% 
    count(REASON == reason)
  res <- prop.test(x = c(sample_1[2,2], sample_2[2,2]), n = c(1000, 1000))
  res
}



