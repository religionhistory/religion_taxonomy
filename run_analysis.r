rm(list = ls())

# Load packages and functions
source("./project_support.r")

# Visualize phylogeny
make.dir("./03_visualization/input")
files <- c("./02_make_nexus/output/b_f_con_data_50_50.csv", "./02_make_nexus/output/b_f_con_ID_dict_50_50.csv", "./data/drh.csv", "./data/BEAST2/con/b_f_con_50_50_mcct.tree")
file.copy(files, "./03_visualization/input")
setwd("./03_visualization/")
source("visualization.r")
setwd("..")

# Compare phylogeny to tagging tree
make.dir("./04_tree_comparison/input")
dict_files <- list.files(path = "./02_make_nexus/output", pattern = "*.csv", full.names = T)
phylo_files <- list.files(path = "./data/BEAST2", pattern = "mcct", full.names = T, recursive = T)
files <- c(dict_files, phylo_files, "./data/religious_tags.csv", "./data/drh.csv")
file.copy(files, "./04_tree_comparison/input")
setwd("./04_tree_comparison/")
source("tree_comparison.r")
setwd("..")

