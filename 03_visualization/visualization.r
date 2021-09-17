# Visualize data

rm(list = ls())

source("../project_support.r")

# Load data
data <- read_csv("./input/b_f_con_data_50_50.csv")
id_dictionary <- read_csv("./input/b_f_con_ID_dict_50_50.csv")
raw_data <- read_csv("./input/drh.csv")
taxonomy <- read.nexus(file = "./input/b_f_con_50_50_mcct.tree")

# Combine dictionary with metadata
dictionary <- id_metadata_dictionary(id_dictionary, raw_data, taxonomy)

# Format data for heatmap
data <- heatmap_formatting(data)

# Extract metadata for plotting
# Extract edge lengths
taxonomy_edges <- phylo_edge_length(taxonomy)

# Plot tree with heatmap of answers
# Plot tree with branch lengths, tip labels and circles indicating which group of people(s) entry covers
taxonomy_group <- plot_phylo_group(taxonomy, taxonomy_edges, dictionary)
# Add heatmap of answers
tree_heatmap <- gheatmap(taxonomy_group, data, offset=0.012, width=0.42, colnames = FALSE, font.size=2) +
  scale_fill_manual(values = c("#91bfdb", "#fc8d59", "#ffffbf"), breaks=c("1", "0", "{01}"), labels = c("Yes", "No", "Uncertainty (Yes or No)")) +
    guides(fill = guide_legend(title="Value")) 
# Save plot
pdf("../figures/heatmap_tree.pdf", height = 15, width = 20)
plot(tree_heatmap)
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
  # Remove tag numbers for visualization
  mutate(entry_tags = gsub("\\[[0-9]+\\]", "", entry_tags))

# Plot tree with religious group tip labels
# Plot branch length labels
tree_edge <- plot_phylo_edge(taxonomy, taxonomy_edges)
# Add tip labels
tree_religious_group <- tree_edge %<+% religion_tags +
  geom_tiplab(aes(label = entry_tags), size=2, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/religious_group_tree.pdf", height = 15, width = 22)
plot(tree_religious_group)
dev.off()

# Plot tree with expert tip labels
tree_expert <- tree_edge %<+% dictionary +
  geom_tiplab(aes(label = Expert), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/expert_tree.pdf", height = 15, width = 20)
plot(tree_expert)
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
  # Remove tag numbers for visualization
  mutate(region_tags = gsub("\\[[0-9]+\\]", "", region_tags))

# Plot tree with region tip labels
tree_region <- tree_edge %<+% region_tags +
  geom_tiplab(aes(label = region_tags), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/region_tree.pdf", height = 15, width = 22)
plot(tree_region)
dev.off()

# Plot tree with entry source tip labels
tree_source <- tree_edge %<+% dictionary +
  geom_tiplab(aes(label = `Entry source`), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
pdf("../figures/source_tree.pdf", height = 15, width = 22)
plot(tree_source)
dev.off()
