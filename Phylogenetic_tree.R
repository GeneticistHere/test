# Load the packages
library(treeio)
library(ggtree)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(tibble)

# Load the Newick tree file
tree <- treeio::read.newick(file = "Newick Export.nwk")

# Load the WRKY gene family information
WRKY_info <- read_xlsx(path = "WRKY_information.xlsx", col_names = TRUE)

# Reshape the WRKY gene family information
WRKY_info2 <- WRKY_info %>%
  select(ID, Group, `Length (AA)`) %>%
  pivot_longer(cols = `Length (AA)`, names_to = "Type", values_to = "Value")

# Merge the WRKY information with the tree data
tree_data <- as_tibble(tree) %>%
  left_join(WRKY_info2, by = c("label" = "ID"))

# Create the base phylogenetic tree in circular layout
p <- ggtree(tree, layout = "circular") %<+% tree_data

# Add annotations
p <- p + 
  geom_tiplab(aes(label=label, color=Group), align=TRUE, linesize=0.5, size=3, hjust=0.001, bg.color="white", bg.alpha=4) +
  geom_fruit(
    geom=geom_tile,
    aes(fill=Group),  # Use Group for discrete fill
    color="black",
    width=0.6,
    offset=0.4  # Increased offset for better spacing
  ) +
  geom_fruit(
    geom=geom_bar,
    aes(x=Value, fill=Group),
    stat="identity",
    orientation="y",
    offset=0.03  # Increased offset for better spacing
  ) +
  scale_fill_viridis_d() +  # For discrete Group variable
  scale_color_viridis_d() + # For discrete Group variable in tiplab
  theme(
    legend.position = "right",
    legend.title = element_text(size=10),
    legend.text = element_text(size=8)
  ) +
  ggtitle("Annotated Phylogenetic Tree")

# Print the tree
print(p)

# Save the plot as a PDF with increased dimensions
ggsave(filename = "phylogenetic_tree.pdf", plot = p, width = 12, height = 12)