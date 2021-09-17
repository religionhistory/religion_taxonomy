# Clean data

rm(list = ls())

source("../project_support.r")

# Load DRH data and related questions
data <- read_csv("./input/drh.csv")
related_questions <- read_csv("./input/related_questions.csv")
v6_questions <- read_csv("./input/drh_v6_poll.csv") %>% rename(quest = Question, quest_desc = `Question description`)

# Convert questions from v5 poll into equivalent v6 poll questions
data_stand <- data %>% left_join(related_questions) %>%
  mutate(`Question ID` = if_else(Poll == "Religious Group (v5)" & !is.na(`Related question ID`), `Related question ID`, `Question ID`)) %>%
  left_join(v6_questions) %>%
  mutate(Question = quest, `Parent question` = `Parent Question`) %>%
  select(-Poll, -quest, -quest_desc, -`Parent Question`, -`Related question ID`)

# Remove questions that have answers in formats that cannot be used for analysis (keep only categorical data with yes/no/field doens't know/I don't know answers)
data_categorical <- data_stand %>%
  filter(`Data Type` == "Nominal") %>%
  # Change branching question labels
  mutate(`Branching question` = gsub("\\(common people, general populace\\)", "", `Branching question`)) %>%
  mutate(`Branching question` = gsub("Status of Participants:", "", `Branching question`)) %>%
  mutate(`Branching question` = gsub("Non-elite", "N", `Branching question`)) %>%
  mutate(`Branching question` = gsub("Elite", "E", `Branching question`)) %>%
  mutate(`Branching question` = gsub("Religious Specialists", "R", `Branching question`)) %>%
  # Split data by branching question answers
  mutate(`Branching question` = strsplit(`Branching question`, ","))
# Convert to data.table and unnest
data_categorical <- as.data.table(data_categorical)
data_categorical <- data_categorical[, list(`Branching question` = as.character(unlist(`Branching question`))), by = setdiff(names(data_categorical), "Branching question")]
data_categorical <- as_tibble(data_categorical)

# Fill in missing questions
data_all_questions <- data_categorical %>%
  complete(nesting(`Entry ID`, `Branching question`),
           nesting(Question, `Question ID`, `Parent question`),
           fill = list(value=0)) %>%
  filter(!is.na(`Question ID`)) %>%
  # remove duplicate rows
  distinct()

# Extract parent questions with negative/ field doesn't know answers 
parent_question <- unique(data_all_questions$`Parent question`)
# If parent question has a negative/ field doesn't know answer replace the answer of the corresponding child question
data_parent_question <- data_all_questions %>%
  filter(Question %in% parent_question) %>%
  select(`Entry ID`, `Branching question`, Question, Answers, `Answer values`) %>%
  rename(`Parent question` = Question, panswer = Answers, panswervalue = `Answer values`) %>%
  filter(panswer != "Yes" & !is.na(panswer)) %>%
  right_join(data_all_questions) %>% 
  mutate(Answers = ifelse(is.na(Answers), panswer, Answers), `Answer values` = ifelse(is.na(`Answer values`), panswervalue, `Answer values`)) %>%
  # remove questions without answers
  filter(!is.na(`Answer values`))

# Split questions into those with a single answer for each entry and those with multiple answers for the same entry
data_ans_no <- data_parent_question %>%
  # Remove duplicates
  distinct() %>%
  select(`Entry ID`, `Branching question`, Question, `Question ID`, Answers, `Answer values`) %>%
  distinct() %>%
  group_by(`Entry ID`, `Branching question`, `Question ID`) %>%
  group_split()
data_single_ans <- do.call(rbind, data_ans_no[sapply(data_ans_no, nrow)==1]) %>%
  select(-Answers, -Question)
data_multi_ans <- do.call(rbind, data_ans_no[sapply(data_ans_no, nrow)>1]) 

# If there are multiple answers to the same question, use the non Field doesn't know or I don't know answer
# If there are yes and no answers treat this as uncertainty, with yes and no both possible answers
data_multi_ans_cor <- data_multi_ans %>%
  group_by(`Entry ID`, `Branching question`, Question, `Question ID`) %>%
  # Replace "Yes [specify] ..." with "Yes
  mutate(Answers = if_else(grepl("Yes \\[specify\\]", Answers), "Yes", Answers)) %>%
  # Yes and Field doesn't know treated as Yes
  mutate(answer_yes_fdk = ifelse(Answers == "Field doesn't know" & lead(Answers) == "Yes" | Answers == "Field doesn't know" & lag(Answers) == "Yes", "1", NA)) %>%
  # No and Field doesn't know treated as No
  mutate(answer_no_fdk = ifelse(Answers == "Field doesn't know" & lead(Answers) == "No" | Answers == "Field doesn't know" & lag(Answers) == "No", "0", NA)) %>%
  # Yes and I don't know treated as Yes
  mutate(answer_yes_idk = ifelse(Answers == "I don't know" & lead(Answers) == "Yes" | Answers == "I don't know" & lag(Answers) == "Yes", "1", NA)) %>%
  # No and I don't know treated as No
  mutate(answer_no_idk = ifelse(Answers == "I don't know" & lead(Answers) == "No" | Answers == "I don't know" & lag(Answers) == "No", "0", NA)) %>%
  # I don't know and Field doesn't know treated as Field doesn't know
  mutate(answer_fdk_idk = ifelse(Answers == "I don't know" & lead(Answers) == "Field doesn't know" | Answers == "I don't know" & lag(Answers) == "Field doesn't know", "-1", NA)) %>%
  # Yes and No treated as Yes or No 
  mutate(answer_yes_no = ifelse(Answers == "Yes" & lead(Answers) == "No" | Answers == "No" & lead(Answers) == "Yes" |Answers == "Yes" & lag(Answers) == "No" | Answers == "No" & lag(Answers) == "Yes", "{01}", NA)) %>%
  mutate(`Answer values` = ifelse(!is.na(answer_yes_fdk), answer_yes_fdk, 
                           ifelse(!is.na(answer_no_fdk), answer_no_fdk,
                           ifelse(!is.na(answer_yes_idk), answer_yes_idk, 
                           ifelse(!is.na(answer_no_idk), answer_no_idk,
                           ifelse(!is.na(answer_fdk_idk), answer_fdk_idk,
                           ifelse(!is.na(answer_yes_no), answer_yes_no,`Answer values`))))))) %>%
  ungroup() %>%
  select(-answer_yes_fdk, -answer_no_fdk, -answer_yes_idk, -answer_no_idk, -answer_fdk_idk, -answer_yes_no, -Answers, -Question) %>%
  distinct() %>%
  # Remove any row with an NA answer
  filter(!is.na(`Answer values`))

# Recombine questions with single and multiple answers and data for each group of people
# Non-elite (common people, general populace), Elite and Religious Specialists
data_group_split <- bind_rows(data_single_ans, data_multi_ans_cor) %>%
  # Replace incorrectly coded answer values
  # Yes or field doesn't know with Yes, Yes or No with correct formatting
  mutate(`Answer values` = if_else(`Answer values` == "1; 0", "1",
                           if_else(`Answer values` == "1; -1", "{01}",`Answer values`)))
  
# Transpose question and answer data
data_t <- data_group_split %>%
  pivot_wider(names_from = `Question ID`, values_from = `Answer values`)

# Recombine entries where the different branching question answers (Elite, Non-elite and religious specialist) have the same answers for all questions
data_group_comb <- data_t %>%
  # Remove additional whitespace from branching questions
  mutate(`Branching question` = str_trim(`Branching question`)) %>%
  group_by_at(setdiff(names(data_t),"Branching question")) %>%
  summarise(`Branching question` = paste(unique(`Branching question`), collapse=",")) %>% 
  ungroup() %>%
  select(`Entry ID`, `Branching question`, everything())

# Create output directory
make.dir("./output")

# Save data
write_csv(data_group_comb, "./output/data_transposed.csv")
