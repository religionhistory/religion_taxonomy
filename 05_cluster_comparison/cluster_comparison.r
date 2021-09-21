# Make output for historians

rm(list = ls())

source("../project_support.r")

# Load data
raw_data <- read_csv("./input/drh.csv")
metadata <- read_csv("./input/b_f_con_50_50.csv") 

# Create dictionary of all questions
questions <- raw_data %>%
  select(`Question ID`, Question, `Question description`, `Parent question`, `Parent question ID`) %>%
  distinct()

# Find distinguishing questions between clusters
# Cluster 1 vs. Cluster 2
C1vC2 <- compare_clusters(metadata, cluster1 = c("1.1", "1.2"), cluster2 = c("2.1.1", "2.1.2", "2.2"), name_cluster1 = "C1", name_cluster2 = "C2")

# Cluster 1.1 vs. Cluster 1.2
C1.1vC1.2 <- compare_clusters(metadata, cluster1 = "1.1", cluster2 = "1.2", name_cluster1 = "C1.1", name_cluster2 = "C1.2")

# Cluster 2.1 vs Cluster 2.2 
C2.1vC2.2 <- compare_clusters(metadata, cluster1 = c("2.1.1", "2.1.2"), cluster2 = "2.2", name_cluster1 = "C2.1", name_cluster2 = "C2.2")

# Cluster 2.1.1 vs Cluster 2.1.2 
C2.1.1vC2.1.2 <- compare_clusters(metadata, cluster1 = "2.1.1", cluster2 = "2.1.2", name_cluster1 = "C2.1.1", name_cluster2 = "C2.1.2")

test <- C1vC2 %>%
  group_by(`Question ID`) %>%
  mutate(max_diff = max(Difference)) %>%
  filter(max_diff >= 55)


test <- C1.1vC1.2 %>%
  group_by(`Question ID`) %>%
  mutate(max_diff = max(Difference)) %>%
  filter(max_diff >= 50)

test <- C2.1vC2.2 %>%
  group_by(`Question ID`) %>%
  mutate(max_diff = max(Difference)) %>%
  filter(max_diff >= 50)

# By sub-cluster
# Cluster 2.1 vs. C1 & C2.2
C2.1vC1C2.2 <- metadata %>%
  mutate(Cluster = case_when(Cluster == "2.1.1" | Cluster == "2.1.2" ~ "C2.1",
                             Cluster == "1.1" | Cluster == "1.2" | Cluster == "2.2" ~ "C1&C2.2")) %>%
  mutate(Cluster = as.factor(as.character(Cluster))) %>%
  select(-ID, -`Entry ID`, -`Branching question`, -`Entry name`, -`Entry source`, -`Entry description`, -`Entry tags`, -Expert, -`Region ID`, -`Region name`, -`Region description`, -`Region tags`, -elite, -non_elite, -religious_specialist) %>%
  mutate_all(as.character) %>%
  pivot_longer(c(-Cluster), names_to = "Question", values_to = "Answers") %>%
  group_by(Cluster, Question, Answers) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  group_by(Cluster, Question) %>%
  mutate(group_total = sum(Frequency)) %>%
  group_by(Cluster, Question, Answers) %>%
  mutate(Percentage = case_when(Cluster == "C2.1" ~ Frequency/group_total * 100,
                                Cluster == "C1&C2.2" ~ Frequency/group_total * 100)) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  select(-Frequency, -group_total) %>%
  pivot_wider(names_from = Cluster, values_from = Percentage) %>%
  mutate(C2.1 = ifelse(is.na(C2.1), 0, C2.1)) %>%
  mutate(`C1&C2.2` = ifelse(is.na(`C1&C2.2`), 0, `C1&C2.2`)) %>%
  mutate(Difference = abs(C2.1 - `C1&C2.2`)) %>%
  rename("Question ID" = "Question") %>%
  mutate(`Question ID` = as.numeric(`Question ID`)) %>%
  inner_join(questions) %>%
  select(`Question ID`, Question, everything()) %>%
  arrange(desc(Difference))

# Cluster 2.2 vs. C1 & C2.1
C2.2vC1C2.1 <- metadata %>%
  mutate(Cluster = case_when(Cluster == "2.2" ~ "C2.2",
                             Cluster == "1.1" | Cluster == "1.2" | Cluster == "2.1.1" | Cluster == "2.1.2" ~ "C1&C2.1")) %>%
  mutate(Cluster = as.factor(as.character(Cluster))) %>%
  select(-ID, -`Entry ID`, -`Branching question`, -`Entry name`, -`Entry source`, -`Entry description`, -`Entry tags`, -Expert, -`Region ID`, -`Region name`, -`Region description`, -`Region tags`, -elite, -non_elite, -religious_specialist) %>%
  mutate_all(as.character) %>%
  pivot_longer(c(-Cluster), names_to = "Question", values_to = "Answers") %>%
  group_by(Cluster, Question, Answers) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  group_by(Cluster, Question) %>%
  mutate(group_total = sum(Frequency)) %>%
  group_by(Cluster, Question, Answers) %>%
  mutate(Percentage = case_when(Cluster == "C2.2" ~ Frequency/group_total * 100,
                                Cluster == "C1&C2.1" ~ Frequency/group_total * 100)) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  select(-Frequency, -group_total) %>%
  pivot_wider(names_from = Cluster, values_from = Percentage) %>%
  mutate(C2.2 = ifelse(is.na(C2.2), 0, C2.2)) %>%
  mutate(`C1&C2.1` = ifelse(is.na(`C1&C2.1`), 0, `C1&C2.1`)) %>%
  mutate(Difference = abs(C2.2 - `C1&C2.1`)) %>%
  rename("Question ID" = "Question") %>%
  mutate(`Question ID` = as.numeric(`Question ID`)) %>%
  inner_join(questions) %>%
  select(`Question ID`, Question, everything()) %>%
  arrange(desc(Difference))

# C1 vs. C2.1
C1vC2.1 <- metadata %>%
  filter(Cluster != "2.2") %>%
  mutate(Cluster = case_when(Cluster == "1.1" | Cluster == "1.2" ~ "C1",
                             Cluster == "2.1.1" | Cluster == "2.1.2" ~ "C2.1")) %>%
  mutate(Cluster = as.factor(as.character(Cluster))) %>%
  select(-ID, -`Entry ID`, -`Branching question`, -`Entry name`, -`Entry source`, -`Entry description`, -`Entry tags`, -Expert, -`Region ID`, -`Region name`, -`Region description`, -`Region tags`, -elite, -non_elite, -religious_specialist) %>%
  mutate_all(as.character) %>%
  pivot_longer(c(-Cluster), names_to = "Question", values_to = "Answers") %>%
  group_by(Cluster, Question, Answers) %>%
  summarise(Frequency = n()) %>%
  ungroup() %>%
  group_by(Cluster, Question) %>%
  mutate(group_total = sum(Frequency)) %>%
  group_by(Cluster, Question, Answers) %>%
  mutate(Percentage = case_when(Cluster == "C1" ~ Frequency/group_total * 100,
                                Cluster == "C2.1" ~ Frequency/group_total * 100)) %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  select(-Frequency, -group_total) %>%
  pivot_wider(names_from = Cluster, values_from = Percentage) %>%
  mutate(C1 = ifelse(is.na(C1), 0, C1)) %>%
  mutate(C2.1 = ifelse(is.na(C2.1), 0, C2.1)) %>%
  mutate(Difference = abs(C1 - C2.1)) %>%
  rename("Question ID" = "Question") %>%
  mutate(`Question ID` = as.numeric(`Question ID`)) %>%
  inner_join(questions) %>%
  select(`Question ID`, Question, everything()) %>%
  arrange(desc(Difference))

