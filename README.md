# Graph-mining

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Similarity matrix construction



```{r similarity}
require(cluster)
require(tidyverse)
require(igraph)
library(factoextra)
Final_features2 <- read_csv("C:/Users/mn852/Dropbox/SNA/network analysis and systems thinking project/Final_features3.csv")
final_features <- read_csv("C:/Users/mn852/Dropbox/SNA/network analysis and systems thinking project/Final_features3.csv")
mydata<- final_features
mydata <- mydata %>% select(followers_count:Positive)
PCAmydata <- prcomp(mydata, center = TRUE, scale = TRUE)
pdist <- daisy(PCAmydata$x, metric = "gower", stand = TRUE)
pdistn <- pdist/max(pdist)
#dist <- daisy(as.data.frame(select_if(system_mapping, is.numeric), stand= TRUE, metric = "gower"))
#dist <- dist/max(dist)
SIM = 1-as.matrix(pdistn)
diag(SIM) <- 0
graph <- graph.adjacency(SIM, mode= "undirected", weighted=T)
bgraph <- delete.edges(graph,which(E(graph)$weight<=1.1*ave(E(graph)$weight)))


mydata1 <-final_features %>% filter(organization_or_individual==1) %>% select(followers_count:Positive)
PCAmydata1 <- prcomp(mydata1, center = TRUE, scale = TRUE)
pdist1 <- daisy(PCAmydata1$x, metric = "gower", stand = TRUE)
pdistn1 <- pdist1/max(pdist1)
SIM1 = 1-as.matrix(pdistn1)
diag(SIM1) <- 0
graph1 <- graph.adjacency(SIM1, mode= "undirected", weighted=T)
bgraph1 <- delete.edges(graph1,which(E(graph1)$weight<=1.1*ave(E(graph1)$weight)))
V(bgraph1)$name <- pull(final_features %>% filter(organization_or_individual==1) %>% select(Account_id))

mydata2 <-final_features %>% filter(organization_or_individual==2) %>% select(followers_count:Positive)
PCAmydata2 <- prcomp(mydata2, center = TRUE, scale = TRUE)
pdist2 <- daisy(PCAmydata2$x, metric = "gower", stand = TRUE)
pdistn2 <- pdist2/max(pdist2)
SIM2 = 1-as.matrix(pdistn2)
diag(SIM2) <- 0
graph2 <- graph.adjacency(SIM2, mode= "undirected", weighted=T)
bgraph2 <- delete.edges(graph2,which(E(graph2)$weight<=1.1*ave(E(graph2)$weight)))
V(bgraph2)$name <- pull(final_features %>% filter(organization_or_individual==2) %>% select(Account_id))

```

## Including Plots

You can also embed plots, for example:

```{r community, echo=FALSE}
comm <- cluster_louvain(bgraph)
communities(comm)
plot(comm,bgraph)

comm1 <- cluster_louvain(bgraph1)
communities(comm1)
plot(comm1,bgraph1)

comm2 <- cluster_louvain(bgraph2)
communities(comm2)
plot(comm2,bgraph2)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
