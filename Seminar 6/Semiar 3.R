library('sna')
library('network')
library('foreign')
##library('igraph')
library('intergraph')
library('NetCluster')
library('dendextend')


data(kracknets, package = "NetData")

load("advice_data_frame.Rdata")
load("friendship_data_frame.Rdata")
load("reports_to_data_frame.Rdata")

head(advice_data_frame)
head(friendship_data_frame)
head(reports_to_data_frame)

# First, make an array of 3 the 3 data frames
krack <- list(advice_data_frame,
              friendship_data_frame,
              reports_to_data_frame)
## add names to objects in list
graphs <- c('advice','friendship','reports')
names(krack) <- graphs
## check on list
length(krack) #how many elements we have in our krack dataset


names(krack)

for (i in 1:length(krack)){ 
  krack[[i]] <- as.matrix(krack[[i]])
}

for(i in 1:3){
  krack[[i]] <- subset(krack[[i]],
                       (krack[[i]][,3] > 0 ))
}

dim(krack[[1]]) 
head(krack[[1]])
names(attributes)


library(network)

attributes[,1]

for (i in 1:3){
  krack[[i]] <- network(krack[[i]],
                        matrix.type = 'edgelist',
                        vertex.attr = list(attributes[,1], attributes[,2],
                                           attributes[,3], attributes[,4]),
                        vertex.attrnames = list("AGE","TENURE","LEVEL","DEPT"))
}
advice <- krack$advice
friendship <- krack$friendship
reports <- krack$reports
# Check networks
network.vertex(advice)
advice
friendship
reports

# Let's take a look at these data.
# First, we create a set of coordinates to run the plot in.
# Detailed explanation of what we do here is provided in the answers to the third seminar.
n<-network.size(advice)
v1<-sample((0:(n-1))/n) #create a vector of random numbers
v2<-sample(v1)
x <- n/(2 * pi) * sin(2 * pi * v1)
y <- n/(2 * pi) * cos(2 * pi * v2)
mycoord <- cbind(x,y)
# Plot networks:
par(mar=c(0,0,1,0))
par(mfrow=c(1,3))
plot(advice, edge.col='azure4', vertex.col='darkorange',
     vertex.border='azure4',vertex.cex=2,coord=mycoord,
     main ='Advice')
plot(friendship, edge.col='azure4', vertex.col='darkorange',
     vertex.border='azure4',vertex.cex=2, coord=mycoord,
     main ='Friendship')
plot(reports, edge.col='azure4', vertex.col='darkorange',
     vertex.border='azure4',vertex.cex=2, coord=mycoord,
     main='Direct Reports')
  
### ---- Assignment task 1 ---- 

library('igraph')
library('intergraph')
library('knitr')
## For the networks we’ve obtained, please calculate the following:
##  1. Dyad census
##  2. At least three types of centrality 
##  3. Triad census
##  4. Transitivity
## Having performed the calculations, please compare your results 
## for each network and make appropriate inferences.
advice.graph <- asIgraph(advice)
friendship.graph <- asIgraph(friendship)
reports.graph <- asIgraph(reports)


dyad.census(advice.graph)
dyad.census(friendship.graph) 
dyad.census(reports.graph) 

V(advice.graph)$degree

centralityA <- data.frame(row.names   = V(advice.graph),
                         degree      = degree(advice.graph),
                         closeness   = closeness(advice.graph),
                         betweenness = betweenness(advice.graph),
                         eigenvector = evcent(advice.graph)$vector)

centralityA <- centralityA[order(as.numeric(row.names(centralityA))),]


kable(centralityA, row.names=T, label="Advice Centrality")


degree(advice.graph)
degree(friendship.graph)
degree(reports.graph)
?betweenness
betweenness(advice.graph, directed=TRUE)
betweenness(friendship.graph, directed=TRUE)
betweenness(reports.graph, directed=TRUE)


evcent(advice.graph)
closeness(friendship.graph)
evcent(reports.graph)



#_______________________________________________________________________________________________________________________________________________________________________________________________________

### ---- Assignment task 2 ---- 

##  1. Experiment with k. We’ve set it to 6, but would another number make more sense?
##  2. Which of the two blockmodels appear to be more accurate to you? Why? 
##  Finally, we can make a heatmap of the two blockmodels:





#_______________________________________________________________________________________________________________________________________________________________________________________________________

### ---- Assignment task 3 ---- 
##Assignment task 3
## Try not to get lost in all the partitions! Please list all the finite 
## block-partitions that we have generated and the names of all people 
## that ended up in every block





#_______________________________________________________________________________________________________________________________________________________________________________________________________

### ---- Assignment task 4 ---- 
## Why do we remove the zero edges from networks? 
## We haven’t done it previously, why are we doing it now?
 





#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 5 ---- 

## What rationale do you have for selecting the number of clusters / 
## positions with the method above? 
## Please rely on your knowledge of cluster analysis to answer this question.




#_______________________________________________________________________________________________________________________________________________________________________________________________________

### ---- Assignment task 6 ---- 
##
##Assignment task 6
## • Plot the resulting blockmodels in any way you wish and examine them visually. 
##   What is the story you get from viewing these clusters, and their within and
##   between cluster densities on task and social interaction? What can you say about 
##   your network from this?
## • We have learned several ways to blockmodel. Which method do you find the most
##   intuitively appealing? Why?
## • What did you learn from blockmodels about your data that you could not 
##   generate from previously learned techniques?

#_______________________________________________________________________________________________________________________________________________________________________________________________________
