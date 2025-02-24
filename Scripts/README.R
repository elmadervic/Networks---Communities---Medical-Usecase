# Install and load igraph if not already installed
if (!require("igraph")) install.packages("igraph")
library(igraph)

# Read the adjacency matrix from a CSV file
# Ensure that your CSV file (e.g., "adj_matrix.csv") does not include headers
adj_matrix <- as.matrix(read.csv("adj_matrix.csv", header = FALSE))

# Create an undirected graph from the adjacency matrix
# Set diag=FALSE to ignore self-loops (if any)
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# Apply the Louvain algorithm for community detection
louvain_comm <- cluster_louvain(g)

# Display the detected communities
print(louvain_comm)

# Optionally, visualize the network with communities highlighted
plot(louvain_comm, g, vertex.label = NA, main = "Louvain Community Detection")
