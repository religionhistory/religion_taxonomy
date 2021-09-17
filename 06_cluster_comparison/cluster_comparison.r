# Compare within cluster correlation 

rm(list = ls())

source("../project_support.r")

# Load data
correlations <- readRDS("./input/b_f_con_50_50_cor.rds")
clusters <- read_csv("./input/b_f_con_50_50.csv")

# Find the overall correlation between each comparison method
overall_correlations <- kendall_output(correlations) 
# Save results
write_csv(overall_correlations, "../results/overall_correlations.csv")

# Find the correlation between clusters by branch length vs. shortest tagging tree
# This comparison method was chosen as it has the strongest correlation
cluster_correlations <- cluster_cor(correlations, clusters, "branch_length_short_tag_tree")

# Perform Kruskal-Wallis rank sum test
kruskal.test(cor ~ Cluster, data = cluster_correlations)

# Visualize cluster correlations and save figure
box_plot <- ggplot(cluster_correlations, aes(x=Cluster, y=cor)) +
  geom_boxplot(width=0.5) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.position="none") +
  ylab("Correlation") 
cairo_pdf("../figures/additional/historian/within_cluster_correlation_box.pdf", height = 3.5, width = 3.5)
plot(box_plot)
dev.off()

ggplot(cluster_correlations, aes(x=Cluster, y=cor)) +
  geom_violin(width = 1) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.line.x = element_line(color="black", size = 0.5),
    axis.line.y = element_line(color="black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.position="none") +
  ylab("Correlation")
ggsave("../figures/additional/historian/within_cluster_correlation_violin.pdf", width = 3.5, height = 3.5)


