# Find discriminating questions between clusters

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

# Create output directory
make.dir("./output")

# Save comparisons
write_csv(C1vC2, "./output/c1vc2.csv")
write_csv(C1.1vC1.2, "./output/c1.1vc1.2.csv")
write_csv(C2.1vC2.2, "./output/c2.1vc2.2.csv")
write_csv(C2.1.1vC2.1.2, "./output/c2.1.1vc2.1.2.csv")

# Find distinguishing questions between entries
# Haroi (752_NR), Raglai - Viatemese Religions (727_N) and Cham Bani (476_ENR)
entry_hrc <- compare_entries(metadata, c("752_NR", "727_N", "476_ENR"))

# Save output
write_csv(entry_hrc, "./output/haroi_reglai_cham.csv")

