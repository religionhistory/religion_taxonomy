# Compare phylogeny to tagging tree

rm(list = ls())

source("../project_support.r")

# Read csv files into global environment
csv_files <- list.files(path = "./input", pattern = "*.csv", full.names = T)
csv_files_list <- sapply(csv_files, read_csv)
csv_files_names <- gsub(".csv", "", list.files(path = "./input", pattern = "*.csv"))
names(csv_files_list) <- csv_files_names
list2env(csv_files_list, globalenv())

# Read tree files into global environment
tree_files <- list.files(path = "./input", pattern = "*.tree", full.names = T)
tree_files_list <- lapply(tree_files, read.nexus)
tree_files_names <- gsub(".tree", "", list.files(path = "./input", pattern = "*.tree"))
tree_files_names <- gsub("mcct", "tree", tree_files_names)
names(tree_files_list) <- tree_files_names
list2env(tree_files_list, globalenv())

# Create output directory
make.dir("./output")

# Compare expert and data derived tagging trees
tree_compare(b_f_con_data_50_50, b_f_con_ID_dict_50_50, b_f_con_50_50_tree, "b_f_con_50_50")
tree_compare(b_f_r4_data_50_50, b_f_r4_ID_dict_50_50, b_f_r4_50_50_tree, "b_f_r4_50_50")

