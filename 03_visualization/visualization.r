# Visualize data

rm(list = ls())

source("../project_support.r")

# Load data
data <- read_csv("./input/b_f_con_data_50_50.csv")
id_dictionary <- read_csv("./input/b_f_con_ID_dict_50_50.csv")
raw_data <- read_csv("./input/drh.csv")
tree <- read.nexus(file = "./input/b_f_con_50_50_mcct.tree")
clusters <- read_csv("./input/b_f_con_50_50.csv")

# Combine dictionary with metadata
dictionary <- id_metadata_dictionary(id_dictionary, raw_data, tree)

# Plot raw overall tree for paper
# Legend labels were corrected afterwards (1 = Religious Specialist, 15 = Non-elite, 17 = Elite)
tree_figure <- overall_tree_figure(tree, clusters, dictionary)
# Save figure
cairo_pdf("../figures/raw_tree_figure.pdf", height = 16, width = 13)
plot(tree_figure) 
dev.off()

# Plot tree of just cluster 1 (C1.1 and C1.2)
C1 <- prune_tree(tree, id_dictionary, metadata = clusters, cluster = c("1.1", "1.2"), entry_name_offset = 0.05, offset_1 = 0.02, offset_2 = 0.03, offset_3 = 0.04)
# Save figure
cairo_pdf("../figures/cluster_1_figure.pdf", height = 16, width = 13)
plot(C1) 
dev.off()

# Format data for heatmap
data <- heatmap_formatting(data)

# Extract metadata for plotting
# Extract edge lengths
tree_edges <- tree_edge_length(tree)

# Plot tree with heatmap of answers
# Plot tree with branch lengths, tip labels and circles indicating which group of people(s) entry covers
tree_group <- plot_tree_group(tree, tree_edges, dictionary)
# Add heatmap of answers
tree_heatmap <- gheatmap(tree_group, data, offset=0.012, width=0.42, colnames = FALSE, font.size=2) +
  scale_fill_manual(values = c("#91bfdb", "#fc8d59", "#ffffbf"), breaks=c("1", "0", "{01}"), labels = c("Yes", "No", "Uncertainty (Yes or No)")) +
    guides(fill = guide_legend(title="Value")) 
# Save plot
cairo_pdf("../figures/heatmap_tree.pdf", height = 15, width = 20)
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
tree_edge <- plot_tree_edge(tree, tree_edges)
# Add tip labels
tree_religious_group <- tree_edge %<+% religion_tags +
  geom_tiplab(aes(label = entry_tags), size=2, offset=0.01) +
  xlim(0, 1)
# save plot
cairo_pdf("../figures/religious_group_tree.pdf", height = 15, width = 22)
plot(tree_religious_group)
dev.off()

# Plot tree with expert tip labels
tree_expert <- tree_edge %<+% dictionary +
  geom_tiplab(aes(label = Expert), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
cairo_pdf("../figures/expert_tree.pdf", height = 15, width = 20)
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
cairo_pdf("../figures/region_tree.pdf", height = 15, width = 22)
plot(tree_region)
dev.off()

# Plot tree with entry source tip labels
tree_source <- tree_edge %<+% dictionary +
  geom_tiplab(aes(label = `Entry source`), size=3, offset=0.01) +
  xlim(0, 1)
# save plot
cairo_pdf("../figures/source_tree.pdf", height = 15, width = 22)
plot(tree_source)
dev.off()
