# Install and load required packages if needed
packages <- c("tidygraph", "igraph", "networkD3", "dplyr")
installed <- packages %in% installed.packages()[, "Package"]
if(any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)


# Read the adjacency matrix 
adj_matrix <- readRDS("Data/3.AdjacencyMatrices/Adj_Matrix_Female_CHronic_age_6.rds")
str(adj_matrix)

# Get all Chronic Conditions - 46 
# nodes in the network
node_props <- read.csv("Chronic_All.csv")
str(node_props)



# Create a graph object using igraph and convert it to a tidygraph object
g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
tg <- as_tbl_graph(g)



# Add node properties to the tidygraph object by assigning IDs via row_number()
tg <- tg %>%
  activate(nodes) %>%
  mutate(id = row_number()) %>%   # Assign sequential IDs to avoid conversion warnings
  left_join(node_props, by = "id")

# Compute communities using the Louvain algorithm via tidygraph.
tg <- tg %>% mutate(community = group_louvain())

# Create a static network plot with ggraph.
p <- ggraph(tg, layout = "fr", niter = 500) +
  geom_edge_link(alpha = 0.5, color = "gray") +
  geom_node_point(aes(color = factor(community)), size = 5) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3) +
  # Invisible layer for tooltip information (if needed for interactive plots)
  geom_node_point(aes(text = paste("Label:", label, 
                                   "<br>Class:", class, 
                                   "<br>ICD Code:", icd_code, 
                                   "<br>Cluster:", community)),
                  size = 5, alpha = 0) +
  labs(color = "Cluster") +
  theme_graph() +
  ggtitle("Louvain Community Detection")

p

# Convert the ggplot to an interactive Plotly visualization using the tooltip from the invisible layer
interactive_plot <- ggplotly(p, tooltip = "text")
interactive_plot



# --- Example networkD3 ---
  
# Extract nodes data frame from tidygraph
nodes_df <- tg %>% activate(nodes) %>% as_tibble()

# Create an informative text field that includes additional node information.
nodes_df$info <- with(nodes_df, paste("ID:", id,
                                      "\nLabel:", label,
                                      "\nCluster:", community,
                                      "\nClass:", class,
                                      "\nICD Code:", icd_code))

# networkD3 requires a column named "name" for node labels.
# Here we use the combined info so that hover text is more informative.
nodes_df$name <- nodes_df$info

# Extract links data frame from tidygraph (edges)
links_df <- tg %>% activate(edges) %>% as_tibble()
# networkD3 uses 0-indexed node numbering, so subtract 1 from 'from' and 'to' columns.
links_df <- links_df %>% mutate(source = from - 1,
                                target = to - 1)

# --- Create the Interactive Network Visualization with networkD3 ---

forceNetwork(Links = links_df, Nodes = nodes_df,
             Source = "source", Target = "target",
             NodeID = "name",         # Node label (which is also used for hover text)
             Group = "community",     # Color nodes by community (cluster)
             opacity = 0.8,
             zoom = TRUE,
             fontSize = 12,
             linkDistance = 100,
             charge = -300,
             legend = TRUE)