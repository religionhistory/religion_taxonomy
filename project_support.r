# Check if packages are installed and if they are not installed, install them
packages <- c("tidyverse", "data.table", "ape", "igraph", "adephylo", "h2o", "rlist")
install.packages(setdiff(packages, rownames(installed.packages())))
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
bioc_packages <- "ggtree"
bioc_missing <- setdiff("ggtree", rownames(installed.packages())) 
if( length(bioc_missing) ) {
  biocLite(bioc_packages[bioc_missing])
}

# Load packages
library(tidyverse)
library(data.table)
library(ape)
library(igraph)
library(adephylo)
library(ggtree)
library(h2o)
library(rlist)

# Functions

# Create a new directory or delete and replace an existing directory
# From https://stackoverflow.com/questions/38375305/how-to-overwrite-a-folder-on-the-computer-using-functions-in-r
make.dir <- function(fp) {
  if(!file.exists(fp)) {  # If the folder does not exist, create a new one
    make.dir(dirname(fp))
    dir.create(fp)
  } else {   # If it existed, delete and replace with a new one  
    unlink(fp, recursive = FALSE)
    dir.create(fp)
  }
} 

# Recombine entries where the different branching question answers (Elite, Non-elite and religious specialist) have the same answers for all questions
group_combine <- function(data) {
  data <- data %>%
  group_by_at(setdiff(names(data),"Branching question")) %>%
  # Combine branching questions with identical answers
  summarise(`Branching question` = paste(unique(`Branching question`), collapse=",")) %>% 
  ungroup() %>%
  # Standardise order of branching questions
  mutate(`Branching question` = gsub(",", "", `Branching question`)) %>%
  mutate(`Branching question` = sub("NE", "EN", `Branching question`)) %>%
  mutate(`Branching question` = sub("RN", "NR", `Branching question`)) %>%
  mutate(`Branching question` = sub("REN", "ENR", `Branching question`)) %>%
  select(`Entry ID`, `Branching question`, everything())
}

# Replace field doesn't know, I don't know and yes or no answers with NA for filtering missing values
replace_unknown_na <- function(data){
  # Convert data to data frame
  data <- as.data.frame(data)
  # Field doesn't know
  data[data == "-1"] <- NA
  # I don't know
  data[data == "-2"] <- NA
  data
}

# Filter data
filter_data <- function(data, var_filter, entry_filter) {
  entry_filt <- rowMeans(is.na(data)) < entry_filter
  question_filt <- colMeans(is.na(data)) < var_filter
  data <- data[entry_filt, question_filt]
  # Recombine entries where the different branching question answers (Elite, Non-elite and religious specialist) have the same answers for all questions
  data <- group_combine(data)
}

# Create dictionary of IDs, entry names and branching questions
id_dictionary <- function(data) {
  data <- data %>%
    mutate(ID = paste0(`Entry ID`, "_", `Branching question`)) %>%
    select(ID, `Entry ID`, `Branching question`)
}

# Create ID for each entry and group of people 
data_id <- function(data) {
  data <- data %>%
    mutate(ID = paste0(`Entry ID`, "_", `Branching question`)) %>%
    select(-`Entry ID`, -`Branching question`) %>%
    select(ID, everything())
}

# Format data for creation of nexus file
nexus_formatting <- function(data){
  # Convert data to data frame
  data <- as.data.frame(data)
  # Convert NA to ?
  data[is.na(data)] <- "?"
  # Convert -1 to 2
  data[data == "-1"] <- "2"
  # Add row names
  data <- as.data.frame(data)
  row.names(data) <- data$ID
  data <- data %>% select(-ID)
  # Convert to matrix
  data <- as.matrix(data)
}

# Write morphological nexus data, based on write.nexus.data from ape
# input is a matrix with row names as taxa labels, a single column data frame with character labels and output files
write_morph_nexus <- function (x, char_labels, file, gap = NULL, missing = NULL) {
  indent <- "        "
  maxtax <- 5
  charsperline <- ncol(x)
  defgap <- "-"
  defmissing <- "?"
  if (is.matrix(x)) {
      xbak <- x
      x <- vector("list", nrow(xbak))
      for (i in seq_along(x)) x[[i]] <- xbak[i, ]
      names(x) <- rownames(xbak)
      rm(xbak)
  }
  ntax <- length(x)
  nchars <- length(x[[1]])
  zz <- file(file, "w")
  if (is.null(names(x))) 
    names(x) <- as.character(1:ntax)
  fcat <- function(..., file = zz) cat(..., file = file, sep = "", 
                                       append = TRUE)
  find.max.length <- function(x) max(nchar(x))
  print.matrix <- function(x, dindent = "    ", collapse = "") {
    Names <- names(x)
    printlength <- find.max.length(Names) + 2
    ntimes <- ceiling(nchars/charsperline)
      start <- 1
      end <- charsperline
      for (j in seq_len(ntimes)) {
        for (i in seq_along(x)) {
          sequence <- paste(x[[i]][start:end], collapse = collapse)
          taxon <- Names[i]
          thestring <- sprintf("%-*s%s%s", printlength, 
                               taxon, dindent, sequence)
          fcat(thestring, "\n")
        }
        if (j < ntimes) 
          fcat("\n")
        start <- start + charsperline
        end <- end + charsperline
        if (end > nchars) 
          end <- nchars
    }
  }
  fcat("#NEXUS\n[Generated by write_morph_nexus.R, ", date(), 
       "]\n\n")
  NCHAR <- paste("NCHAR=", nchars, sep = "")
  NTAX <- paste0("NTAX=", ntax)
  if (is.null(missing)) 
    missing <- defmissing
  MISSING <- paste0("MISSING=", missing)
  if (is.null(gap)) 
    gap <- defgap
  GAP <- paste0("GAP=", gap)
    fcat("BEGIN TAXA;\n")
    fcat(indent, "TITLE  combined;\n")
    fcat(indent, "DIMENSIONS ", NTAX, ";\n") 
    fcat(indent, "TAXLABELS\n")
    fcat(indent, "  ")
    for (i in seq_len(ntax)) {
      fcat(names(x)[i], "\n")
      fcat(indent, "  ")
    }
    fcat(";\n")
    fcat("END;\n\n")
    fcat("BEGIN CHARACTERS;\n")
    fcat(indent, "TITLE  Character Descriptions;\n")
    fcat(indent, "LINK TAXA = combined;\n")
    fcat(indent,"DIMENSIONS ", NCHAR, ";\n")
    fcat(indent,'FORMAT DATATYPE=Standard SYMBOLS= "0 1 2 3 4 5 6 7 8 9" ', MISSING, " ", GAP, ";\n")
    fcat("\nMATRIX\n")
    print.matrix(x)
    fcat(";\nEND;\n")
  close(zz)
}

# Format data for each robustness test
robustness_test_format <- function(raw_data, filtered_data, data_ID, robustness_test = "1") {
  # Subset raw data by filtered data 
  filtered_raw_data = raw_data %>%
    select(colnames(filtered_data)) 
  data = group_combine(filtered_raw_data) %>%
    mutate(ID = paste0(`Entry ID`, "_", `Branching question`)) %>%
    filter(ID %in% data_ID$ID)
  # Find any rows not subsetted
  if(nrow(data) != nrow(data_ID)) {
    non_extracted_data = data_ID %>%
      filter(!ID %in% data$ID) %>%
      separate(ID, c("Entry ID","Branching question"), sep = '_')
    if(max(nchar(non_extracted_data$`Branching question`)) > 1) {
      non_extracted_data = non_extracted_data %>%
        mutate(`Branching question` = strsplit(`Branching question`, "")) %>%
        unnest(`Branching question`) }
    # Extract entries not initially extracted
    non_extracted_entry = filtered_raw_data %>%
      filter(`Entry ID` %in% non_extracted_data$`Entry ID` & `Branching question` %in% non_extracted_data$`Branching question`) 
    if(length(unique(non_extracted_entry$`Entry ID`)) < nrow(non_extracted_entry)) {
      non_extracted_entry = non_extracted_entry %>%
        mutate_all(as.character) %>%
        pivot_longer(c(-`Entry ID`, -`Branching question`), names_to = "Question", values_to = "Answers") %>%
        group_by(`Entry ID`, Question) %>%
        # Yes and unanswered, yes and field doesn't know and yes and I don't know treated as Yes
        mutate(answer_yes = ifelse(is.na(Answers) & lead(Answers) == "1" | is.na(Answers) & lag(Answers) == "1" | Answers == "-1" & lead(Answers) == "1" | Answers == "-1" & lag(Answers) == "1" | Answers == "-2" & lead(Answers) == "1" | Answers == "-2" & lag(Answers) == "1", "1", NA)) %>%
        # No and unanswered, no and field doesn't know and no and I don't know treated as No
        mutate(answer_no = ifelse(is.na(Answers) & lead(Answers) == "0" | is.na(Answers) & lag(Answers) == "0" | Answers == "-1" & lead(Answers) == "0" | Answers == "-1" & lag(Answers) == "0" | Answers == "-2" & lead(Answers) == "0" | Answers == "-2" & lag(Answers) == "0", "0", NA)) %>%
        # Field doesn't know and unanswered, and Field doesn't know and I don't know treated as Field doesn't know
        mutate(answer_fdk = ifelse(is.na(Answers) & lead(Answers) == "-1" | is.na(Answers) & lag(Answers) == "-1" | Answers == "-2" & lead(Answers) == "-1" | Answers == "-2" & lag(Answers) == "-1", "-1", NA)) %>%
        # I don't know and unanswered treated as I don't know 
        mutate(answer_idk = ifelse(is.na(Answers) & lead(Answers) == "-2" | is.na(Answers) & lag(Answers) == "-2", "-2", NA)) %>%
        mutate(Answers = ifelse(!is.na(answer_yes), answer_yes, 
                                ifelse(!is.na(answer_no), answer_no,
                                       ifelse(!is.na(answer_fdk), answer_fdk,
                                              ifelse(!is.na(answer_idk), answer_idk,
                                                     Answers))))) %>%
        ungroup() %>%
        select(-answer_yes, -answer_no, -answer_fdk, -answer_idk) %>%
        pivot_wider(names_from = Question, values_from = Answers) %>%
        group_by_at(setdiff(names(non_extracted_entry),"Branching question")) %>%
        # Combine branching questions with identical answers
        summarise(`Branching question` = paste(unique(`Branching question`), collapse="")) %>% 
        ungroup() %>%
        pivot_longer(c(-`Entry ID`, -`Branching question`), names_to = "Question", values_to = "Answers") %>%
        group_by(`Entry ID`, Question) %>%
        # Yes and unanswered, yes and field doesn't know and yes and I don't know treated as yes
        mutate(answer_yes = ifelse(is.na(Answers) & lead(Answers) == "1" | is.na(Answers) & lag(Answers) == "1" | Answers == "-1" & lead(Answers) == "1" | Answers == "-1" & lag(Answers) == "1" | Answers == "-2" & lead(Answers) == "1" | Answers == "-2" & lag(Answers) == "1", "1", NA)) %>%
        # No and unanswered, no and field doesn't know and no and I don't know treated as No
        mutate(answer_no = ifelse(is.na(Answers) & lead(Answers) == "0" | is.na(Answers) & lag(Answers) == "0" | Answers == "-1" & lead(Answers) == "0" | Answers == "-1" & lag(Answers) == "0" | Answers == "-2" & lead(Answers) == "0" | Answers == "-2" & lag(Answers) == "0", "0", NA)) %>%
        # Field doesn't know and unanswered treated as field doesn't know
        mutate(answer_fdk = ifelse(is.na(Answers) & lead(Answers) == "-1" | is.na(Answers) & lag(Answers) == "-1", "-1", NA)) %>%
        # I don't know and unanswered treated as I don't know
        mutate(answer_idk = ifelse(is.na(Answers) & lead(Answers) == "-2" | is.na(Answers) & lag(Answers) == "-2", "-2", NA)) %>%
        # Yes and No treated as Yes or No 
        mutate(answer_yes_no = ifelse(Answers == "1" & lead(Answers) == "0" | Answers == "0" & lead(Answers) == "1" | Answers == "1" & lag(Answers) == "0" | Answers == "0" & lag(Answers) == "1" | Answers == "{01}" & lag(Answers) == "0" | Answers == "{01}" & lead(Answers) == "0" | Answers == "0" & lag(Answers) == "{01}" | Answers == "0" & lead(Answers) == "{01}" | Answers == "{01}" & lag(Answers) == "1" | Answers == "{01}" & lead(Answers) == "1" | Answers == "1" & lag(Answers) == "{01}" | Answers == "1" & lead(Answers) == "{01}", "{01}", NA)) %>%
        mutate(Answers = ifelse(!is.na(answer_yes), answer_yes, 
                                ifelse(!is.na(answer_no), answer_no,
                                       ifelse(!is.na(answer_yes_no), answer_yes_no, 
                                              ifelse(!is.na(answer_fdk), answer_fdk,
                                                     ifelse(!is.na(answer_idk), answer_idk, Answers)))))) %>%
        ungroup() %>%
        select(-answer_yes, -answer_no, -answer_yes_no, -answer_fdk, -answer_idk) %>%
        pivot_wider(names_from = Question, values_from = Answers) %>%
        group_by_at(setdiff(names(non_extracted_entry),"Branching question")) %>%
        # Combine branching questions with identical answers
        group_combine()
    } 
    non_extracted_entry = non_extracted_entry %>%
      mutate(ID = paste0(`Entry ID`, "_", `Branching question`)) 
    # Readd entries that were not initially extracted
    data = rbind(data, non_extracted_entry)
  } else {
    data = data
  } # Format data for each robustness test
  if(robustness_test == "1") {
    # Replace I don't know with missing
    data[data == "-2"] <- NA
  } else if (robustness_test == "2"){
    # Replace field doesn't know with missing
    data[data == "-1"] <- NA
  } else {
    # Do not replace any values
    data = data
  }
  data
}

# Find percentage difference between each group of peoples (branching questions) per entry
percent_diff_group <- function(data) {
 data <- data %>%
    group_by(`Entry ID`) %>%
    summarise_all(n_distinct) %>%
    ungroup() %>%
    mutate(sum = select(., -`Entry ID`, -`Branching question`) %>% rowSums() - (ncol(data) - 2)) %>%
    mutate(per_diff = sum/(ncol(data) - 2) * 100) %>%
    select(`Entry ID`, `Branching question`, per_diff, sum, everything())
}

# Combine groups of people for entries with <10% difference between groups
comb_diff_group_10 <- function(data, percent_difference){
  if(nrow(percent_difference %>% filter(per_diff > 0 & per_diff < 10)) > 0){
    diff_group_10 <- data %>%
      filter(`Entry ID` %in% (percent_difference %>% filter(per_diff > 0 & per_diff < 10))$`Entry ID`) %>%
      mutate_all(as.character) %>%
      pivot_longer(c(-`Entry ID`, -`Branching question`), names_to = "Question", values_to = "Answers") %>%
      group_by(`Entry ID`, Question) %>%
      # Yes and unanswered, yes and field doesn't know and yes and I don't know treated as Yes
      mutate(answer_yes = ifelse(is.na(Answers) & lead(Answers) == "1" | is.na(Answers) & lag(Answers) == "1" | Answers == "-1" & lead(Answers) == "1" | Answers == "-1" & lag(Answers) == "1" | Answers == "-2" & lead(Answers) == "1" | Answers == "-2" & lag(Answers) == "1", "1", NA)) %>%
      # No and unanswered, no and field doesn't know and no and I don't know treated as No
      mutate(answer_no = ifelse(is.na(Answers) & lead(Answers) == "0" | is.na(Answers) & lag(Answers) == "0" | Answers == "-1" & lead(Answers) == "0" | Answers == "-1" & lag(Answers) == "0" | Answers == "-2" & lead(Answers) == "0" | Answers == "-2" & lag(Answers) == "0", "0", NA)) %>%
      # Field doesn't know and unanswered, and Field doesn't know and I don't know treated as Field doesn't know
      mutate(answer_fdk = ifelse(is.na(Answers) & lead(Answers) == "-1" | is.na(Answers) & lag(Answers) == "-1" | Answers == "-2" & lead(Answers) == "-1" | Answers == "-2" & lag(Answers) == "-1", "-1", NA)) %>%
      # I don't know and unanswered treated as I don't know 
      mutate(answer_idk = ifelse(is.na(Answers) & lead(Answers) == "-2" | is.na(Answers) & lag(Answers) == "-2", "-2", NA)) %>%
      mutate(Answers = ifelse(!is.na(answer_yes), answer_yes, 
                       ifelse(!is.na(answer_no), answer_no,
                       ifelse(!is.na(answer_fdk), answer_fdk,
                       ifelse(!is.na(answer_idk), answer_idk,
                                                  Answers))))) %>%
      ungroup() %>%
      select(-answer_yes, -answer_no, -answer_fdk, -answer_idk) %>%
      pivot_wider(names_from = Question, values_from = Answers) %>%
      group_by_at(setdiff(names(data),"Branching question")) %>%
      # Combine branching questions with identical answers
      summarise(`Branching question` = paste(unique(`Branching question`), collapse="")) %>% 
      ungroup() %>%
      pivot_longer(c(-`Entry ID`, -`Branching question`), names_to = "Question", values_to = "Answers") %>%
      group_by(`Entry ID`, Question) %>%
      # Yes and unanswered, yes and field doesn't know and yes and I don't know treated as yes
      mutate(answer_yes = ifelse(is.na(Answers) & lead(Answers) == "1" | is.na(Answers) & lag(Answers) == "1" | Answers == "-1" & lead(Answers) == "1" | Answers == "-1" & lag(Answers) == "1" | Answers == "-2" & lead(Answers) == "1" | Answers == "-2" & lag(Answers) == "1", "1", NA)) %>%
      # No and unanswered, no and field doesn't know and no and I don't know treated as No
      mutate(answer_no = ifelse(is.na(Answers) & lead(Answers) == "0" | is.na(Answers) & lag(Answers) == "0" | Answers == "-1" & lead(Answers) == "0" | Answers == "-1" & lag(Answers) == "0" | Answers == "-2" & lead(Answers) == "0" | Answers == "-2" & lag(Answers) == "0", "0", NA)) %>%
      # Field doesn't know and unanswered treated as field doesn't know
      mutate(answer_fdk = ifelse(is.na(Answers) & lead(Answers) == "-1" | is.na(Answers) & lag(Answers) == "-1", "-1", NA)) %>%
      # I don't know and unanswered treated as I don't know
      mutate(answer_idk = ifelse(is.na(Answers) & lead(Answers) == "-2" | is.na(Answers) & lag(Answers) == "-2", "-2", NA)) %>%
      # Yes and No treated as Yes or No 
      mutate(answer_yes_no = ifelse(Answers == "1" & lead(Answers) == "0" | Answers == "0" & lead(Answers) == "1" | Answers == "1" & lag(Answers) == "0" | Answers == "0" & lag(Answers) == "1" | Answers == "{01}" & lag(Answers) == "0" | Answers == "{01}" & lead(Answers) == "0" | Answers == "0" & lag(Answers) == "{01}" | Answers == "0" & lead(Answers) == "{01}" | Answers == "{01}" & lag(Answers) == "1" | Answers == "{01}" & lead(Answers) == "1" | Answers == "1" & lag(Answers) == "{01}" | Answers == "1" & lead(Answers) == "{01}", "{01}", NA)) %>%
      mutate(Answers = ifelse(!is.na(answer_yes), answer_yes, 
                              ifelse(!is.na(answer_no), answer_no,
                                     ifelse(!is.na(answer_yes_no), answer_yes_no, 
                                            ifelse(!is.na(answer_fdk), answer_fdk,
                                                   ifelse(!is.na(answer_idk), answer_idk, Answers)))))) %>%
      ungroup() %>%
      select(-answer_yes, -answer_no, -answer_yes_no, -answer_fdk, -answer_idk) %>%
      pivot_wider(names_from = Question, values_from = Answers) %>%
      group_by_at(setdiff(names(data),"Branching question")) %>%
      # Combine branching questions with identical answers
      group_combine()
  } else {
    diff_group_10 <- data
  }
}

# Find groups of people from the same entry with >10% difference between them and recombine taxa
diff_group_comb <- function(data) {
  percent_difference <- percent_diff_group(data)
  diff_group_10 <- comb_diff_group_10(data, percent_difference) 
  if(nrow(percent_difference %>% filter(per_diff > 0 & per_diff < 10)) > 0){
    # Recombine data
    data_group_other <- data %>%
      filter(!`Entry ID` %in% (percent_difference %>% filter(per_diff > 0 & per_diff < 10))$`Entry ID`) %>%
      mutate_all(as.character)
    data_group_all <- bind_rows(diff_group_10, data_group_other)
  } else {
    data 
  }
}

# Select index of values to convert to missing
sample_index <- function(data, seed) {
  # Index all cells with non-missing data
  non_missing_idx <- as.data.frame(which(!is.na(data), arr.ind=TRUE)) 
  set.seed(seed)
  # Extract a random sample of cells to convert to missing values
  sample_idx <- sample_n(non_missing_idx, (sum(!is.na(data))/10)) 
}

# Add an additional 10% missing values for testing glrm prediction accuracy 
add_10_NA <- function(data, seed) {
  # Index all cells with non-missing data
  non_missing_idx <- as.data.frame(which(!is.na(data), arr.ind=TRUE)) 
  set.seed(seed)
  # Extract a random sample of cells to convert to missing values
  sample_idx <- sample_n(non_missing_idx, (sum(!is.na(data))/10)) 
  data_extra_missing <- data
  for(i in 1:nrow(sample_idx)) {
    data_extra_missing[sample_idx$row[i], sample_idx$col[i]] <- NA
  }
  # Convert all columns to factors
  data_extra_missing <- data_extra_missing %>%
    mutate_if(is.numeric, as.factor) %>%
    mutate_if(is.character, as.factor) 
}

# Predict missing values from full data set using glrm
glrm_impute <- function(data, k_idx){
  data_no_id <- data %>%
    mutate_if(is.numeric, as.factor) %>%
    mutate_if(is.character, as.factor) %>%
    as.h2o()
  glrm_model <- h2o.glrm(training_frame = data_no_id, cols = 1:ncol(data_no_id), k = k_idx,
                         loss = "Quadratic", transform = "None", regularization_x = "None", regularization_y = "None",
                         multi_loss = "Categorical", loss_by_col = rep("Categorical", ncol(data_no_id)), loss_by_col_idx = c(0:(ncol(data_no_id)-1)),
                         recover_svd = TRUE, seed = 5, ignore_const_cols = FALSE, max_iterations = 1000)
  data_imputed <- as.data.frame(predict(glrm_model, data_no_id)) 
  data_imputed <- data_imputed %>%
    rename_all(~ sub("reconstr_", "", names(data_imputed)))
}

# Find optimal value of k for glrm 
glrm_k_idx <- function(raw_data, analysis, granularity, var_filter, entry_filter, seed) {
  if(granularity == "c") {
    parent_var <- colnames(raw_data)[colnames(raw_data) %in% parent_quest$`Question ID`]
    raw_data <- raw_data %>% select(`Entry ID`, `Branching question`, all_of(parent_var))
  } else {
    raw_data <- raw_data
  }
  # Replace Field doesn't know and I don't know (and Yes or No for analyis a) answers with NA for filtering missing values
  data <- replace_unknown_na(raw_data)
  # Filter data to remove questions with >x% missing variables and >y% missing entries
  filtered_data <- filter_data(data, var_filter, entry_filter)
  
  if(analysis == "b") {
    # Find groups of people from the same entry with >10% difference between them and recombine taxa
    # recombine entries where the different branching question answers (Elite, Non-elite and religious specialist) have the same answers for all questions
    filtered_data <- diff_group_comb(filtered_data)
  }
  # Remove constant columns
  filtered_data <- filtered_data[sapply(filtered_data, function(x) length(unique(na.omit(x)))) > 1]
  # Remove Yes or No answers for glrm
  filtered_data[filtered_data == "{01}"] <- "3"
  # Select entry ID and branching question
  entry_id <- filtered_data %>% select(`Entry ID`, `Branching question`)
  # Remove ID for imputation
  data_no_id <- filtered_data %>% select(-`Entry ID`, -`Branching question`)
  # Create index of values to convert to missing
  sample_idx <- sample_index(data_no_id, seed)
  # Add an additional 10% missing values
  data_extra_missing <- add_10_NA(data_no_id, seed)
  # Convert filtering to percentage
  var_filt_per <- var_filter * 100
  entry_filt_per <- entry_filter * 100
  glrm_data <- as.h2o(data_extra_missing)
  glrm.list <- list()
  for (i in 1:(ceiling(ncol(data_extra_missing)/2))){
    glrm.list[[i]] <- h2o.glrm(training_frame = glrm_data, cols = 1:ncol(glrm_data), k = i,
                               loss = "Quadratic", transform = "None", regularization_x = "None", regularization_y = "None",
                               multi_loss = "Categorical", loss_by_col = rep("Categorical", ncol(glrm_data)), loss_by_col_idx = c(0:(ncol(glrm_data)-1)),
                               recover_svd = TRUE, seed = seed, ignore_const_cols = FALSE, max_iterations = 1000)
  }
  # Predict missing values
  predict.na <- list()
  for (i in 1:length(glrm.list)){
    predict.na[[i]] <- predict(glrm.list[[i]], glrm_data)
  }
  
  # Extract the original values of all data points replaced with NA
  sample <- list()
  for(i in 1:nrow(sample_idx)) {
    sample[[i]] <- data_no_id[sample_idx$row[i], sample_idx$col[i]]
  }
  sample <- as.data.frame(unlist(sample))
  
  # Extract the predicted values of all data points replaced with NA
  predict.list <- list()
  for(i in 1:length(predict.na)) {
    data <- as.data.frame(predict.na[[i]])
    data.list <- list()
    for(j in 1:nrow(sample_idx)) {
      data.list[[j]] <- data[sample_idx$row[j], sample_idx$col[j]]
    }
    predict.list[[i]] <- as.data.frame(unlist(data.list))
  }
  
  # Bind predicted and observed values
  predict.list <- lapply(predict.list, setNames, nm = "predicted")
  predicted <- lapply(predict.list, function(x) as.numeric(as.character(x$predicted)))
  colnames(sample) <- "observed"
  sample <- sample %>% mutate(observed = as.numeric(as.character(observed)))
  predict.comp <- Map(cbind, predicted, sample)
  predict.comp <- lapply(predict.comp, function(x) as.data.frame(x))
  predict.comp <- lapply(predict.comp, setNames, nm = c("predicted", "observed"))
  
  # Find difference between predicted and actual values
  predict.diff <- lapply(predict.comp, function(x) {x$difference <- abs(x$observed - x$predicted); return(x)})
  predict.pcc <- lapply(predict.comp, function(x) sum(x$observed == x$predicted)/nrow(x))
  predict.pcc <- unlist(predict.pcc)
  predict.df <- data.frame(k = 1:length(predict.pcc), PCC = round(predict.pcc, 3)) %>%
    mutate(`K Percentage Correctly Classified (PCC)` = paste0(k, " ", PCC)) %>%
    select(`K Percentage Correctly Classified (PCC)`)
  
  # Find index of value of K with the highest accuracy
  k_idx <- which.max(predict.pcc)
  
  # Prepare output
  output <- rbind(paste("Optimal value k =", k_idx, "\n\n", colnames(predict.df)), predict.df) 
  colnames(output) <- ""
  # Save output
  write.table(output, file = paste0("./k_value/", analysis, "_", granularity, "_", var_filt_per, "_", entry_filt_per, "_k_value", ".txt"), quote = FALSE, row.names = FALSE)
}

# Make nexus files with corresponding dictionaries and data
make_nexus_dict <- function(raw_data, analysis, granularity, var_filter, entry_filter, k){
  if(granularity == "c") {
    parent_var <- colnames(raw_data)[colnames(raw_data) %in% parent_quest$`Question ID`]
    raw_data <- raw_data %>% select(`Entry ID`, `Branching question`, all_of(parent_var))
  } else {
    raw_data <- raw_data
  }
  # Replace Field doesn't know and I don't know (and Yes or No for analyis a) answers with NA for filtering missing values
  data <- replace_unknown_na(raw_data)
  # Filter data to remove questions with >x% missing values 
  filtered_data <- filter_data(data, var_filter, entry_filter)
  if(analysis == "b") {
    # Find groups of people from the same entry with >10% difference between them and recombine taxa
    # recombine entries where the different branching question answers (Elite, Non-elite and religious specialist) have the same answers for all questions
    filtered_data <- diff_group_comb(filtered_data)
  }
  # Remove constant columns
  filtered_data <- filtered_data[sapply(filtered_data, function(x) length(na.omit(unique(x)))) > 1]
  
  # Convert filtering to percentage
  var_filt_per <- var_filter * 100
  entry_filt_per <- entry_filter * 100
  
  #### Conservative analysis
  # Include yes, no and yes|no answers
  # Create ID for each entry and group of people 
  data_ID <- data_id(filtered_data)
  # Create dictionary of IDs, entry names and branching questions
  dictionary <- id_dictionary(filtered_data)
  # Format data for creation of nexus file
  data_nexus <- nexus_formatting(data_ID)
  # Save data
  write_csv(data_ID, paste0("./output/", analysis, "_", granularity, "_con_data_", var_filt_per, "_", entry_filt_per, ".csv"))
  write_csv(dictionary, paste0("./output/", analysis, "_", granularity, "_con_ID_dict_", var_filt_per, "_", entry_filt_per, ".csv"))
  # Write nexus data
  write_morph_nexus(data_nexus, file = paste0("./output/", analysis, "_", granularity, "_con_", var_filt_per, "_", entry_filt_per, ".nex"), missing = "?")
  
  #### GLRM	
  # For robustness tests 1-4 impute missing values using glrm 	
  # Select entry ID and branching question	
  entry_id <- filtered_data %>% select(`Entry ID`, `Branching question`)	
  # Remove ID for imputation	
  data_no_id <- filtered_data %>% select(-`Entry ID`, -`Branching question`)
  # Remove Yes or No answers for glrm
  data_no_id[data_no_id == "{01}"] <- "3"

  # GLRM
  data_imputed <- glrm_impute(data_no_id, k)
  # Recombine data with entry names and branching questions
  data_imputed_id <- cbind(entry_id, data_imputed) %>%
    mutate_if(is.factor, as.character)
  
  #### Robustness test 1
  # Impute field doesn’t know answers with glrm
  # Format data for robustness test 1
  r_test_1_data <- robustness_test_format(raw_data, filtered_data, data_ID, robustness_test = "1")
  # Index field doesn't know values
  fdk_idx <- as.data.frame(which(r_test_1_data == -1, arr.ind=TRUE)) 
  # If there are field doesn't know values answers perform robustness test
  if(nrow(fdk_idx) > 0) {
    # Extract the imputed values of field doesn't know answers
    fdk_imp <- list()
    for(i in 1:nrow(fdk_idx)) {
      fdk_imp[[i]] <- data_imputed_id[fdk_idx$row[i], fdk_idx$col[i]]
    }
    fdk_imp <- as.numeric(unlist(fdk_imp))
    # Replace field doesn't know answers with imputed values
    r_test_1_data <- as.matrix(r_test_1_data)
    for(i in 1:nrow(fdk_idx)) {
      r_test_1_data[fdk_idx$row[i], fdk_idx$col[i]] <- fdk_imp[i]
    }
    r_test_1_data <- as.data.frame(r_test_1_data)
    # Create ID for each entry and group of people 
    r_1_data_ID <- data_id(r_test_1_data)
    # Create dictionary of IDs, entry names and branching questions
    r_1_dictionary <- id_dictionary(r_test_1_data)
    # Format data for creation of nexus file
    r_1_data_nexus <- nexus_formatting(r_1_data_ID)
    # Save data
    write_csv(r_1_data_ID, paste0("./output/", analysis, "_", granularity, "_r1_data_", var_filt_per, "_", entry_filt_per, ".csv"))
    write_csv(r_1_dictionary, paste0("./output/", analysis, "_", granularity, "_r1_ID_dict_", var_filt_per, "_", entry_filt_per, ".csv"))
    # Write nexus data
    write_morph_nexus(r_1_data_nexus, file = paste0("./output/", analysis, "_", granularity, "_r1_", var_filt_per, "_", entry_filt_per, ".nex"), missing = "?")
  }  
  #### Robustness test 2
  # Impute I don't know answers with glrm
  # Format data for robustness test 2
  r_test_2_data <- robustness_test_format(raw_data, filtered_data, data_ID, robustness_test = "2")
  # Index I don't know values
  idk_idx <- as.data.frame(which(r_test_2_data == -2, arr.ind=TRUE)) 
  # If there are I don't know answers perform robustness test
  if(nrow(idk_idx) > 0) {
    # Extract the imputed values of I don't know answers
    idk_imp <- list()
    for(i in 1:nrow(idk_idx)) {
      idk_imp[[i]] <- data_imputed_id[idk_idx$row[i], idk_idx$col[i]]
    }
    idk_imp <- as.numeric(unlist(idk_imp))
    # Replace I don't know answers with imputed values
    r_test_2_data <- as.matrix(r_test_2_data)
    for(i in 1:nrow(idk_idx)) {
      r_test_2_data[idk_idx$row[i], idk_idx$col[i]] <- idk_imp[i]
    }
    r_test_2_data <- as.data.frame(r_test_2_data)
    # Create ID for each entry and group of people 
    r_2_data_ID <- data_id(r_test_2_data)
    # Create dictionary of IDs, entry names and branching questions
    r_2_dictionary <- id_dictionary(r_test_2_data)
    # Format data for creation of nexus file
    r_2_data_nexus <- nexus_formatting(r_2_data_ID)
    # Save data
    write_csv(r_2_data_ID, paste0("./output/", analysis, "_", granularity, "_r2_data_", var_filt_per, "_", entry_filt_per, ".csv"))
    write_csv(r_2_dictionary, paste0("./output/", analysis, "_", granularity, "_r2_ID_dict_", var_filt_per, "_", entry_filt_per, ".csv"))
    # Write nexus data
    write_morph_nexus(r_2_data_nexus, file = paste0("./output/", analysis, "_", granularity, "_r2_", var_filt_per, "_", entry_filt_per, ".nex"), missing = "?")
  }  
  #### Robustness test 3
  # Impute Field doesn't know and I don't know answers with glrm
  # Format data for robustness test 3
  r_test_3_data <- robustness_test_format(raw_data, filtered_data, data_ID, robustness_test = "3")
  # Index field doesn't know and I don't know values
  fdk_idk_idx <- as.data.frame(which(r_test_3_data == -1 | r_test_3_data == -2 , arr.ind=TRUE))
  # If there are field doesn't know and I don't know answers perform robustness test
  if(nrow(fdk_idk_idx) > 0) {
    # Extract the imputed values of I don't know answers
    fdk_idk_imp <- list()
    for(i in 1:nrow(fdk_idk_idx)) {
      fdk_idk_imp[[i]] <- data_imputed_id[fdk_idk_idx$row[i], fdk_idk_idx$col[i]]
    }
    fdk_idk_imp <- as.numeric(unlist(fdk_idk_imp))
    # Replace I don't know answers with imputed values
    r_test_3_data <- as.matrix(r_test_3_data)
    for(i in 1:nrow(fdk_idk_idx)) {
      r_test_3_data[fdk_idk_idx$row[i], fdk_idk_idx$col[i]] <- fdk_idk_imp[i]
    }
    r_test_3_data <- as.data.frame(r_test_3_data)
    # Create ID for each entry and group of people 
    r_3_data_ID <- data_id(r_test_3_data)
    # Create dictionary of IDs, entry names and branching questions
    r_3_dictionary <- id_dictionary(r_test_3_data)
    # Format data for creation of nexus file
    r_3_data_nexus <- nexus_formatting(r_3_data_ID)
    # Save data
    write_csv(r_3_data_ID, paste0("./output/", analysis, "_", granularity, "_r3_data_", var_filt_per, "_", entry_filt_per, ".csv"))
    write_csv(r_3_dictionary, paste0("./output/", analysis, "_", granularity, "_r3_ID_dict_", var_filt_per, "_", entry_filt_per, ".csv"))
    # Write nexus data
    write_morph_nexus(r_3_data_nexus, file = paste0("./output/", analysis, "_", granularity, "_r3_", var_filt_per, "_", entry_filt_per, ".nex"), missing = "?")
  }
  
  #### Robustness test 4 
  # Impute all missing values with glrm
    # Format data for robustness test 4
    r_test_4_data <- robustness_test_format(raw_data, filtered_data, data_ID, robustness_test = "4")
    # Index all unanswered, field doesn't know and I don't know answers
    missing_idx <- as.data.frame(which(r_test_4_data == -1 | r_test_4_data == -2 | is.na(r_test_4_data), arr.ind=TRUE)) 
    # If there are missing answers perform robustness test
    if(nrow(missing_idx) > 0) {
    # Extract the imputed values of field doesn't know answers
      missing_imp <- list()
      for(i in 1:nrow(missing_idx)) {
        missing_imp[[i]] <- data_imputed_id[missing_idx$row[i], missing_idx$col[i]]
      }
      missing_imp <- as.numeric(unlist(missing_imp))
      # Replace field doesn't know answers with imputed values
      r_test_4_data <- as.matrix(r_test_4_data)
      for(i in 1:nrow(missing_idx)) {
        r_test_4_data[missing_idx$row[i], missing_idx$col[i]] <- missing_imp[i]
      }
      r_test_4_data <- as.data.frame(r_test_4_data)
    # Create ID for each entry and group of people 
    r_4_data_ID <- data_id(r_test_4_data)
    # Create dictionary of IDs, entry names and branching questions
    r_4_dictionary <- id_dictionary(r_test_4_data)
    # Format data for creation of nexus file
    r_4_data_nexus <- nexus_formatting(r_4_data_ID)
    # Save data
    write_csv(r_4_data_ID, paste0("./output/", analysis, "_", granularity, "_r4_data_", var_filt_per, "_", entry_filt_per, ".csv"))
    write_csv(r_4_dictionary, paste0("./output/", analysis, "_", granularity, "_r4_ID_dict_", var_filt_per, "_", entry_filt_per, ".csv"))
    # Write nexus data
    write_morph_nexus(r_4_data_nexus, file = paste0("./output/", analysis, "_", granularity, "_r4_", var_filt_per, "_", entry_filt_per, ".nex"), missing = "?")
  }
}

# Create dictionary of IDs and metadata
id_metadata_dictionary <- function(data, raw_data, phylogeny) {
  dictionary <- data %>%
    left_join(select(raw_data, c(`Entry ID`, `Entry name`, `Entry source`, `Entry description`, `Entry tags`, `Date range`, `Region ID`, `Region name`, `Region description`, `Region tags`, Expert))) %>%
    distinct() %>%
    # Remove missing date range
    filter(!is.na(`Date range`)) %>%
    # Combine multiple experts for a single entry into a single string
    group_by_at(vars(-Expert)) %>%
    summarise(Expert = paste(unique(Expert), collapse = ", ")) %>% 
    ungroup() %>%
    mutate(Expert = if_else(Expert == "William Noseworthy, Andrea Acri", "Andrea Acri, William Noseworthy", Expert)) %>%
    # Convert date ranges to numeric
    mutate(start_date = str_extract(`Date range`, "[^-]+")) %>%
    mutate(start_year = as.numeric(gsub("([0-9]+).*$", "\\1", start_date))) %>%
    mutate(start_year = if_else(grepl("B", start_date), -start_year, start_year)) %>%
    mutate(end_date = sub(".*-", "", `Date range`)) %>%
    mutate(end_year = as.numeric(gsub("([0-9]+).*$", "\\1", end_date))) %>%
    mutate(end_year = if_else(grepl("B", end_date), -end_year, end_year)) %>%
    select(-`Date range`, -start_date, -end_date) %>%
    # If an entry has multiple date ranges, select earliest starting date and latest finishing date
    group_by_at(vars(-start_year, -end_year)) %>%
    mutate(`Start year` = min(start_year)) %>%
    mutate(`End year` = max(end_year)) %>%
    select(-start_year, -end_year) %>%
    ungroup() %>%
    distinct() %>%
    # Combine multiple regions into single strings for Region ID, Region name, Region description and Region tags
    group_by_at(vars(-`Region ID`, -`Region name`, -`Region description`, -`Region tags`)) %>%
    summarise(`Region ID` = paste(unique(`Region ID`), collapse = ", "), `Region name` = paste(unique(`Region name`), collapse = "; "), `Region description` = paste(unique(`Region description`), collapse = "; "), `Region tags` = paste(unique(`Region tags`), collapse = "; ")) %>% 
    ungroup() %>%
    distinct() %>%
    # Split groups of people into seperate columns for plotting
    mutate(elite = ifelse(grepl("E", `Branching question`), "E", NA)) %>%
    mutate(non_elite = ifelse(grepl("N", `Branching question`), "N", NA)) %>%
    mutate(religious_specialist = ifelse(grepl("R", `Branching question`), "R", NA)) %>%
    # Add tip labels
    mutate(label = phylogeny$tip.label) %>%
    select(label, everything())
}

# Format data for heatmap visualisation
heatmap_formatting <- function(data) {
  # Convert ID to row names
  data <- as.data.frame(data)
  rownames(data) <- data$ID
  data <- data %>% select(-ID)
}

# Extract phylogeny edge lengths
phylo_edge_length <- function(data){
  data <- data.frame(data$edge, edge_length=round(data$edge.length,2)) %>% 
    rename("parent" = "X1", "node" = "X2")
}

# Plot phylogeny with edge lengths
plot_phylo_edge <- function(phylogeny, phylogeny_edges){
  tree <- ggtree(phylogeny) %<+% phylogeny_edges + geom_text(aes(x = branch, label = edge_length), size = 2, hjust = -.2, vjust=-.2) 
}

# Plot phylogeny with entry/group of people labels
plot_phylo_group <- function(phylogeny, phylogeny_edges, dictionary){
  # Add edge lengths
  tree <- plot_phylo_edge(phylogeny, phylogeny_edges)
  # Add tiplabels and circles indicating which group of people(s) entry covers
  tree2 <- tree %<+% dictionary +
    geom_tiplab(aes(label = `Entry name`), size=2.5, offset=0.255) +
    xlim(0, 1) +
    geom_point(aes(x = x+0.24, color=elite), na.rm = TRUE)  + 
    geom_point(aes(x = x+0.245, color=religious_specialist), na.rm = TRUE) +
    geom_point(aes(x = x+0.25, color=non_elite), na.rm = TRUE) +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73"), labels = c("Elite", "Non-elite", "Religious Specialist")) +
    guides(color = guide_legend(title="Group of People")) 
}

# Find tag levels and frequency of tags per religious group
tag_level_freq <- function(data){
  # Shorten Buddhism of Tibetan origin.. and remove commas for splitting strings
  data <- data %>% mutate(`Entry tags` = gsub("Buddhism of Tibetan origin \\(Tibetan refugee communities in India, mostly in Bhoutan, Ladakh, Nepal and Sikkim\\)", "Buddhism of Tibetan origin", `Entry tags`)) %>%
  # Frequency of tags per entry (number of commas + 1)
  mutate(freq_tags = str_count(`Entry tags`, ',') + 1) %>%
  separate_rows(`Entry tags`, sep = ",") %>%
  # Remove whitespace
  mutate(`Entry tags` = str_trim(`Entry tags`)) %>%
  # Extract religious tag levels
  mutate(tag_level = str_count(`Entry tags`, '->'))
}
  
# Extract edge list from tag paths
edge_list <- function(x) {
  # Extract the parent tags
  x$Parent <- str_extract(x$path, "[^-]+")
  # Extract first level child tags (child 1)
  x$Child1 <- gsub("^^[^>]*>","", x$path)
  x$Child1 <- str_extract(x$Child1, "[^>]+")
  x$Child1 <- gsub(" -","", x$Child1)
  # Extract second level child tags (child 2)
  x$Child2 <- gsub("^^[^>]*>","", x$path)
  x$Child2 <- gsub("^^[^>]*>","", x$Child2)
  x$Child2 <- str_extract(x$Child2, "[^>]+")
  x$Child2 <- gsub(" -","", x$Child2)
  # Extract third level child tags (child 3)
  x$Child3 <- gsub("^^[^>]*>","", x$path)
  x$Child3 <- gsub("^^[^>]*>","", x$Child3)
  x$Child3 <- gsub("^^[^>]*>","", x$Child3)
  x$Child3 <- str_extract(x$Child3, "[^>]+")
  x$Child3 <- gsub(" -","", x$Child3)
  # Extract fourth level child tags (child 4)
  x$Child4 <- gsub("^^[^>]*>","", x$path)
  x$Child4 <- gsub("^^[^>]*>","", x$Child4)
  x$Child4 <- gsub("^^[^>]*>","", x$Child4)
  x$Child4 <- gsub("^^[^>]*>","", x$Child4)
  x$Child4 <- str_extract(x$Child4, "[^>]+")
  x$Child4 <- gsub(" -","", x$Child4)
  # Extract fourth level child tags (child 5)
  x$Child5 <- gsub("^^[^>]*>","", x$path)
  x$Child5 <- gsub("^^[^>]*>","", x$Child5)
  x$Child5 <- gsub("^^[^>]*>","", x$Child5)
  x$Child5 <- gsub("^^[^>]*>","", x$Child5)
  x$Child5 <- gsub("^^[^>]*>","", x$Child5)
  x$Child5 <- str_extract(x$Child5, "[^>]+")
  x$Child5 <- gsub(" -","", x$Child5)
  # Extract fourth level child tags (child 6)
  x$Child6 <- gsub("^^[^>]*>","", x$path)
  x$Child6 <- gsub("^^[^>]*>","", x$Child6)
  x$Child6 <- gsub("^^[^>]*>","", x$Child6)
  x$Child6 <- gsub("^^[^>]*>","", x$Child6)
  x$Child6 <- gsub("^^[^>]*>","", x$Child6)
  x$Child6 <- gsub("^^[^>]*>","", x$Child6)
  x$Child6 <- str_extract(x$Child6, "[^>]+")
  x$Child6 <- gsub(" -","", x$Child6)
  # Extract fourth level child tags (child 7)
  x$Child7 <- gsub("^^[^>]*>","", x$path)
  x$Child7 <- gsub("^^[^>]*>","", x$Child7)
  x$Child7 <- gsub("^^[^>]*>","", x$Child7)
  x$Child7 <- gsub("^^[^>]*>","", x$Child7)
  x$Child7 <- gsub("^^[^>]*>","", x$Child7)
  x$Child7 <- gsub("^^[^>]*>","", x$Child7)
  x$Child7 <- gsub("^^[^>]*>","", x$Child7)
  x$Child7 <- str_extract(x$Child7, "[^>]+")
  x$Child7 <- gsub(" -","", x$Child7)
  # Extract fourth level child tags (child 8)
  x$Child8 <- gsub("^^[^>]*>","", x$path)
  x$Child8 <- gsub("^^[^>]*>","", x$Child8)
  x$Child8 <- gsub("^^[^>]*>","", x$Child8)
  x$Child8 <- gsub("^^[^>]*>","", x$Child8)
  x$Child8 <- gsub("^^[^>]*>","", x$Child8)
  x$Child8 <- gsub("^^[^>]*>","", x$Child8)
  x$Child8 <- gsub("^^[^>]*>","", x$Child8)
  x$Child8 <- gsub("^^[^>]*>","", x$Child8)
  x$Child8 <- str_extract(x$Child8, "[^>]+")
  x$Child8 <- gsub(" -","", x$Child8)
  # Extract pairs of columns
  ParentChild1 <- x %>% select(Parent, Child1) %>% rename(SourceName = Parent, TargetName = Child1)
  Child1Child2 <- x %>% select(Child1, Child2) %>% rename(SourceName = Child1, TargetName = Child2)
  Child2Child3 <- x %>% select(Child2, Child3) %>% rename(SourceName = Child2, TargetName = Child3)
  Child3Child4 <- x %>% select(Child3, Child4) %>% rename(SourceName = Child3, TargetName = Child4)
  Child4Child5 <- x %>% select(Child4, Child5) %>% rename(SourceName = Child4, TargetName = Child5)
  Child5Child6 <- x %>% select(Child5, Child6) %>% rename(SourceName = Child5, TargetName = Child6)
  Child6Child7 <- x %>% select(Child6, Child7) %>% rename(SourceName = Child6, TargetName = Child7)
  Child7Child8 <- x %>% select(Child7, Child8) %>% rename(SourceName = Child7, TargetName = Child8)
  # Recombine pairs of columns
  ColumnPair <- rbind(ParentChild1, Child1Child2, Child2Child3, Child3Child4, Child4Child5, Child5Child6, Child6Child7, Child7Child8)
  # Remove leading/lagging whitespace
  ColumnPair$SourceName <- gsub("^\\s+|\\s+$", "", ColumnPair$SourceName)
  ColumnPair$TargetName <- gsub("^\\s+|\\s+$", "", ColumnPair$TargetName)
  # Extract only non-duplicated rows & filter rows where SourceName != TargetName
  ColumnPair <- ColumnPair %>% distinct() %>% filter(SourceName != TargetName)
}

# Set column and row names for matrix
col_row_names <- function(x,y) {
  colnames(x) <- y
  rownames(x) <- y
  x
}

# Compare expert and data derived tagging trees
tree_compare <- function(data, id_dictionary, phylogeny, output){
  # Combine dictionary with metadata
  dictionary <- id_metadata_dictionary(id_dictionary, drh, phylogeny)
  # Find tag levels and frequency of tags per religious group
  tags <- tag_level_freq(dictionary)
  # Merge question and answer data with metadata
  data_dictionary <- left_join(dictionary, data) %>%
    # Shorten Buddhism of Tibetan origin.. and remove commas for splitting strings
    mutate(`Entry tags` = gsub("Buddhism of Tibetan origin \\(Tibetan refugee communities in India, mostly in Bhoutan, Ladakh, Nepal and Sikkim\\)", "Buddhism of Tibetan origin", `Entry tags`))
  
  # Find entries with only the tag religion and no other tags
  religion_tag_only <- data_dictionary[data_dictionary$`Entry tags` == "Religious Group[8]",]
  
  # Find all tag paths for each entry and extract the longest path for each religious group
  longest_tag_path <- data_dictionary %>%   
    select(ID, `Entry ID`, `Entry name`, `Branching question`, `Entry tags`) %>%
    mutate(`Entry tags` = gsub(", ", ",", `Entry tags`)) %>% 
    mutate(`Entry tags` = gsub(" -> ", "->", `Entry tags`)) %>% 
    mutate(`Entry tags` = strsplit(`Entry tags`, ",")) %>% 
    unnest(`Entry tags`) %>%
    mutate(tag_ID = gsub('.*->\\s*','', `Entry tags`), path_length = str_count(`Entry tags`, '->')) %>%
    mutate(parent_tag = gsub('^[^->]*->', '', `Entry tags`)) %>%
    mutate(parent_tag = gsub('\\s*->.*', '', parent_tag)) %>%
    filter(tag_ID != "Religious Group[8]" | tag_ID == "Religious Group[8]" & `Entry name` %in% religion_tag_only$`Entry name`) %>%
    group_by(ID, `Entry ID`, `Entry name`, `Branching question`, parent_tag) %>%
    filter(path_length == max(path_length)) %>%
    mutate(entry_tag_ID = paste0(ID, ";", tag_ID))
  
  # Split tags into individual strings
  used_entry_tags <- unlist(strsplit(data_dictionary$`Entry tags`, '\\([^)]+,(*SKIP)(*FAIL)|,\\s*', perl=T))
  used_entry_tags <- gsub('.*->\\s*','', used_entry_tags)
  used_entry_tags <- unique(used_entry_tags)
  # Extract only used religious group tags
  used_religious_tags <- religious_tags %>% filter(tag_ID %in% used_entry_tags)
  
  # Extract edges (tag pairs)
  religious_group_edges <- edge_list(used_religious_tags)
  # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
  religious_group_graph <- igraph::simplify(igraph::graph.data.frame(religious_group_edges, directed=TRUE))
  # Create a distance matrix of tags
  religious_distance_mtx <- distances(religious_group_graph)
  # convert to a data frame of tag pairs and distances 
  tag_pairs_distance <- data.frame(tag_1=colnames(religious_distance_mtx)[col(religious_distance_mtx)], tag_2=rownames(religious_distance_mtx)[row(religious_distance_mtx)], dist=c(religious_distance_mtx)) %>%
    mutate_all(list( ~ str_replace(., "Buddhism of Tibetan origin \\(Tibetan refugee communities in India, mostly in Bhoutan, Ladakh, Nepal and Sikkim\\)", "Buddhism of Tibetan origin")))
  
  # Create dataframe of every entry and tag combination against every other entry and tag combination
  religious_tag_pairs <- expand.grid(longest_tag_path$entry_tag_ID, longest_tag_path$entry_tag_ID)  %>% 
    separate(Var1, c("entry_1","tag_1"), sep = ";") %>%
    separate(Var2, c("entry_2","tag_2"), sep = ";") %>%
    # Join entry tag pairs with distance
    left_join(tag_pairs_distance) %>%
    filter(!is.na(dist))
  
  # Extract the shortest distance between each pair of tags
  shortest_tag_distance <- religious_tag_pairs %>%
    group_by(entry_1, entry_2) %>%
    filter(dist == min(dist)) %>%
    select(entry_1, entry_2, dist) %>%
    filter(row_number()==1) %>% # filter duplicates of the same entry pair and tag distance
    ungroup() 
  
  # Extract the longest distance between each pair of tags
  longest_tag_distance <- religious_tag_pairs %>%
    group_by(entry_1, entry_2) %>%
    filter(dist == max(dist)) %>%
    select(entry_1, entry_2, dist) %>%
    filter(row_number()==1) %>% # filter duplicates of the same entry pair and tag distance
    ungroup() 
  
  # Extract expert generated tagging tree shortest distance matrix
  tag_tree_shortest <- as.data.frame(spread(shortest_tag_distance, entry_2, dist)) %>% select(-entry_1) %>%
    mutate_all(as.numeric)
  row.names(tag_tree_shortest) <- colnames(tag_tree_shortest)
  
  # Extract expert generated tagging tree longest distance matrix
  tag_tree_longest <- as.data.frame(spread(longest_tag_distance, entry_2, dist)) %>% select(-entry_1) %>%
    mutate_all(as.numeric)
  row.names(tag_tree_longest) <- colnames(tag_tree_longest)
  
  # Create distance matrix of branch lengths
  branch_length <- cophenetic(phylogeny)
  
  # Create distance matrix of the number of nodes between taxa
  n_node <- as.matrix(distTips(phylogeny, tips = "all", method = "nNodes", useC = TRUE))
  
  # Find the highest (furthest from root tag level per taxa)
  max_tag_level <- tags %>%
    group_by(ID) %>%
    filter(tag_level == max(tag_level)) %>%
    filter(row_number()==1) %>%
    mutate(ID_tag_level = paste0(ID, "_", tag_level))
  
  # Calculate kendall correlations between each pair of taxa
  tag_tree_short_long <- cor(tag_tree_shortest, tag_tree_longest, method = "kendall")
  branch_length_short_tag_tree <- cor(tag_tree_shortest, branch_length, method = "kendall")
  n_node_short_tag_tree <- cor(tag_tree_shortest, n_node, method = "kendall")
  branch_length_long_tag_tree <- cor(tag_tree_longest, branch_length, method = "kendall")
  n_node_tag_long_tree <- cor(tag_tree_longest, n_node, method = "kendall")
  branch_length_n_node <- cor(branch_length, n_node, method = "kendall")
  # List data frames
  taxa_cor_list <- list(tag_tree_short_long, branch_length_short_tag_tree, n_node_short_tag_tree, branch_length_long_tag_tree, n_node_tag_long_tree, branch_length_n_node)
  names(taxa_cor_list) <- c("shortest_vs_longest_distance_tag_tree", "branch_length_short_tag_tree", "n_node_short_tag_tree", "branch_length_long_tag_tree", "n_node_tag_long_tree", "branch_length_n_node")
  
  # Replace row and column names with highest tag level for kendall correlations between each pair of taxa
  tag_tree_short_long <- col_row_names(tag_tree_short_long, max_tag_level$ID_tag_level)
  branch_length_short_tag_tree <- col_row_names(branch_length_short_tag_tree, max_tag_level$ID_tag_level)
  n_node_short_tag_tree <- col_row_names(n_node_short_tag_tree, max_tag_level$ID_tag_level)
  branch_length_long_tag_tree <- col_row_names(branch_length_long_tag_tree, max_tag_level$ID_tag_level)
  n_node_tag_long_tree <- col_row_names(n_node_tag_long_tree, max_tag_level$ID_tag_level)
  branch_length_n_node <- col_row_names(branch_length_n_node, max_tag_level$ID_tag_level)
  # List data frames
  taxa_tag_level_cor_list <- list(tag_tree_short_long, branch_length_short_tag_tree, n_node_short_tag_tree, branch_length_long_tag_tree, n_node_tag_long_tree, branch_length_n_node)
  names(taxa_tag_level_cor_list) <- c("shortest_vs_longest_distance_tag_tree", "branch_length_short_tag_tree", "n_node_short_tag_tree", "branch_length_long_tag_tree", "n_node_tag_long_tree", "branch_length_n_node")
  
  # Calculate kendall correlations between each pair of distance matrices
  tag_tree_short_long <- cor.test(c(as.matrix(tag_tree_shortest)), c(as.matrix(tag_tree_longest)), method = "kendall", alternative = "two.sided")
  branch_length_short_tag_tree <- cor.test(c(as.matrix(tag_tree_shortest)), c(as.matrix(branch_length)), method = "kendall", alternative = "two.sided")
  n_node_short_tag_tree <- cor.test(c(as.matrix(tag_tree_shortest)), c(as.matrix(n_node)), method = "kendall", alternative = "two.sided")
  branch_length_long_tag_tree <- cor.test(c(as.matrix(tag_tree_longest)), c(as.matrix(branch_length)), method = "kendall", alternative = "two.sided")
  n_node_tag_long_tree <- cor.test(c(as.matrix(tag_tree_longest)), c(as.matrix(n_node)), method = "kendall", alternative = "two.sided")
  branch_length_n_node <- cor.test(c(as.matrix(branch_length)), c(as.matrix(n_node)), method = "kendall", alternative = "two.sided")
  # List data frames 
  mtx_cor_list <- list(tag_tree_short_long, branch_length_short_tag_tree, n_node_short_tag_tree, branch_length_long_tag_tree, n_node_tag_long_tree, branch_length_n_node)
  names(mtx_cor_list) <- c("shortest_vs_longest_distance_tag_tree", "branch_length_short_tag_tree", "n_node_short_tag_tree", "branch_length_long_tag_tree", "n_node_tag_long_tree", "branch_length_n_node")
  
  # List taxa and matrix kendall correlations 
  cor_list <- list(taxa_cor_list, mtx_cor_list, taxa_tag_level_cor_list)
  names(cor_list) <- c("taxa_correlations", "whole_matrix_correlations", "taxa_tag_level_correlations")
  # Save output
  list.save(cor_list, paste0("./output/", output, "_cor.rds"))
  }
