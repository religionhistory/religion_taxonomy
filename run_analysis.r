rm(list = ls())

# Load packages and functions
source("./project_support.r")

# Make figures folder
make.dir("figures")

# Make results folder
make.dir("results")

# Visualize tree
make.dir("./03_visualization/input")
files <- c( "./data/clusters/b_f_con_50_50.csv", "./02_make_nexus/output/b_f_con_data_50_50.csv", "./02_make_nexus/output/b_f_con_ID_dict_50_50.csv", "./data/drh.csv", "./data/BEAST2/con/b_f_con_50_50_mcct.tree")
file.copy(files, "./03_visualization/input", overwrite = TRUE)
setwd("./03_visualization/")
source("visualization.r")
setwd("..")

# Compare data derived tree to tagging tree
make.dir("./04_tree_comparison/input")
dict_files <- list.files(path = "./02_make_nexus/output", pattern = "*.csv", full.names = T)
tree_files <- list.files(path = "./data/BEAST2", pattern = "mcct", full.names = T, recursive = T)
files <- c(dict_files, tree_files, "./data/religious_tags.csv", "./data/drh.csv")
file.copy(files, "./04_tree_comparison/input", overwrite = TRUE)
setwd("./04_tree_comparison/")
source("tree_comparison.r")
setwd("..")

# Find questions that discriminate between clusters
make.dir("./05_cluster_comparison/input")
files <- c("./data/drh.csv", "./data/clusters/b_f_con_50_50.csv", "./data/clusters/b_f_r4_50_50.csv")
file.copy(files, "./05_cluster_comparison/input", overwrite = TRUE)
setwd("./05_cluster_comparison/")
source("cluster_comparison.r")
setwd("..")

# Compare correlation within clusters
make.dir("./06_cluster_correlation/input")
files <- c("./04_tree_comparison/output/b_f_con_50_50_cor.rds", "./data/clusters/b_f_con_50_50.csv")
file.copy(files, "./06_cluster_correlation/input", overwrite = TRUE)
setwd("./06_cluster_correlation/")
source("cluster_correlation.r")
setwd("..")

