# statistical inference 2
# sheet 10 exercise 10.1
# Topic: Exponential Random Graph Models (ERGMs)


library(ergm)
library(network)
library(sna)

edges <- read.table("Ex_10_1_terrorist.pairs")



# ---- (a) ----

network_directed <- as.network(edges)
network_undirected <- as.network(
  as.matrix(network_directed),
  directed = FALSE
)

plot(network_undirected)

network_density <- summary(network_undirected ~ density)
network_density

degree_centrality <- sna::degree(network_undirected)
degree_centrality

degree_distribution <- degreedist(network_undirected)

plot(
  degree_distribution,
  type = "l",
  xlab = "Degree",
  ylab = "Proportion of nodes"
)

betweenness_centrality <- sna::betweenness(network_undirected)
betweenness_centrality

plot(
  density(betweenness_centrality),
  xlab = "Betweenness centrality",
  ylab = "Density",
  main = "Betweenness centrality distribution"
)



# ---- (c) ----

model_edges <- ergm(
  network_undirected ~ edges
)

summary(model_edges)

edge_coefficient <- coef(model_edges)["edges"]
edge_probability <- exp(edge_coefficient) /
  (1 + exp(edge_coefficient))

edge_coefficient
edge_probability
network_density

gof_edges <- gof(model_edges)
plot(gof_edges)



# ---- (d) ----

model_esp <- ergm(
  network_undirected ~ edges + esp(0)
)

summary(model_esp)

gof_esp <- gof(model_esp)
plot(gof_esp)

AIC(model_edges)
AIC(model_esp)
