# Visualize data

rm(list = ls())

source("../project_support.r")

# Load data
data <- read_csv("./input/b_f_con_data_50_50.csv")
id_dictionary <- read_csv("./input/b_f_con_ID_dict_50_50.csv")
raw_data <- read_csv("./input/drh.csv")
phylogeny <- read.nexus(file = "./input/b_f_con_50_50_mcct.tree")

# Combine dictionary with metadata
dictionary <- id_metadata_dictionary(id_dictionary, raw_data, phylogeny)

# Format data for heatmap
data <- heatmap_formatting(data)

# Extract metadata for plotting
# Extract edge lengths
phylogeny_edges <- phylo_edge_length(phylogeny)

# Plot tree with heatmap of answers
# Plot tree with branch lengths, tip labels and circles indicating which group of people(s) entry covers
phylo_group <- plot_phylo_group(phylogeny, phylogeny_edges, dictionary)
# Add heatmap of answers
phylo_heatmap <- gheatmap(phylo_group, data, offset=0.012, width=0.42, colnames = FALSE, font.size=2) +
  scale_fill_manual(values = c("#91bfdb", "#fc8d59", "#ffffbf"), breaks=c("1", "0", "{01}"), labels = c("Yes", "No", "Uncertainty (Yes or No)")) +
    guides(fill = guide_legend(title="Value")) 
# Save plot
pdf("../figures/heatmap_tree.pdf", height = 15, width = 20)
plot(phylo_heatmap)
dev.off()

# Split religious group tags into separate columns
religion_tags <- dictionary %>%
  select(label, ID, `Entry ID`, `Entry name`, `Branching question`, `Entry tags`) %>%
  rename(entrytags = `Entry tags`) %>%
  separate(entrytags, c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W"), ",") %>%
  # Extract just tags without path
  mutate_at(.vars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W"), 
            .funs = gsub,
            pattern = ".*->\\s*",
            replacement = "") %>% 
  group_by(label, ID, `Entry ID`, `Entry name`, `Branching question`) %>%
  summarise(entry_tags = toString(c(A, B, C, D, E, F, G, H, I, J, K, L, M, O, P, Q, R, S, T, U, V, W))) %>%
  ungroup() %>%
  # remove NAs and spaces from strings
  mutate(entry_tags = gsub(", NA,", "", entry_tags)) %>%
  mutate(entry_tags = gsub("NA", "", entry_tags)) %>%
  mutate(entry_tags = gsub(" ,", "", entry_tags)) %>%
  mutate(entry_tags = gsub("  ", " ", entry_tags)) %>%
  # Remove tag numbers for visualisation
  mutate(entry_tags = gsub("\\[[0-9]+\\]", "", entry_tags))

# Plot tree with religious group tip labels
# Plot branch length labels
phylo_edge <- plot_phylo_edge(phylogeny, phylogeny_edges)
# Add tip labels
phylo_religious_group <- phylo_edge %<+% religion_tags +
  geom_tiplab(aes(label = entry_tags), size=2, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/religious_group_tree.pdf", height = 15, width = 22)
plot(phylo_religious_group)
dev.off()

# Plot tree with expert tip labels
phylo_expert <- phylo_edge %<+% dictionary +
  geom_tiplab(aes(label = Expert), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/expert_tree.pdf", height = 15, width = 20)
plot(phylo_expert)
dev.off()

# Split region tags into separate columns
region_tags <- dictionary %>%
  select(label, ID, `Entry ID`, `Entry name`, `Branching question`, `Region tags`) %>%
  rename(region_tags = `Region tags`) %>%
  separate(region_tags, c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"), ",") %>%
  # Extract just tags without path
  mutate_at(.vars = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"), 
            .funs = gsub,
            pattern = ".*->\\s*",
            replacement = "") %>% 
  group_by(label, ID, `Entry ID`, `Entry name`, `Branching question`) %>%
  summarise(region_tags = toString(c(A, B, C, D, E, F, G, H, I, J, K, L, M))) %>%
  ungroup() %>%
  # remove NAs and spaces from strings
  mutate(region_tags = gsub(", NA,", "", region_tags)) %>%
  mutate(region_tags = gsub("NA", "", region_tags)) %>%
  mutate(region_tags = gsub(" ,", "", region_tags)) %>%
  mutate(region_tags = gsub("  ", " ", region_tags)) %>%
  # Remove tag numbers for visualisation
  mutate(region_tags = gsub("\\[[0-9]+\\]", "", region_tags))

# Plot tree with region tip labels
phylo_region <- phylo_edge %<+% region_tags +
  geom_tiplab(aes(label = region_tags), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/region_tree.pdf", height = 15, width = 22)
plot(phylo_region)
dev.off()

# Plot tree with entry source tip labels
phylo_source <- phylo_edge %<+% dictionary +
  geom_tiplab(aes(label = `Entry source`), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/source_tree.pdf", height = 15, width = 22)
plot(phylo_source)
dev.off()
