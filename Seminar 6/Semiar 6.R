library('sna')
library('network')
library('foreign')
##library('igraph')
library('intergraph')
library('NetCluster')
library('dendextend')


install.packages('NetCluster')
install.packages('dendextend')
#_______________________________________________________________________________________________________________________________________________________________________________________________________
data(kracknets, package = "NetData")

load("advice_data_frame.Rdata")
load("friendship_data_frame.Rdata")
load("reports_to_data_frame.Rdata")
load("reports_to_data_frame.Rdata")
load("attributes.Rdata")

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

attributes

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
#_______________________________________________________________________________________________________________________________________________________________________________________________________

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

##DYADS
d<-sapply(list(advice.graph,friendship.graph,reports.graph),dyad.census)
dyads <- data.frame(d)
colnames(dyads) <- c("advice", 'friendship', 'reports')
kable(dyads, label="Dyad Census")

##CENTRALITY
centralityA <- data.frame(row.names   = V(advice.graph),
                         degree      = degree(advice.graph),
                         betweenness = betweenness(advice.graph),
                         eigenvector = evcent(advice.graph)$vector)

centralityF <- centralityA[order(as.numeric(row.names(centralityF))),]

centralityF <- data.frame(row.names   = V(friendship.graph),
                          degree      = degree(friendship.graph),
                          betweenness = betweenness(friendship.graph),
                          eigenvector = evcent(friendship.graph)$vector)

centralityF <- centralityF[order(as.numeric(row.names(centralityF))),]

centralityR <- data.frame(row.names   = V(reports.graph),
                          degree      = degree(reports.graph),
                          betweenness = betweenness(reports.graph),
                          eigenvector = evcent(reports.graph)$vector)

centralityR <- centralityR[order(as.numeric(row.names(centralityR))),]




##TRIADS
types <-c('003','012','102','021D','021U','021C','111D','111U','030T','030C','201','120D','120U','120C', '210', '300')
t<-sapply(list(advice.graph,friendship.graph,reports.graph),triad_census)
t
triads <- data.frame(t)
rownames(triads) <- types
kable(triads, label="Triad Census")

##TRANSITIVITY

tr <-sapply(list(advice.graph,friendship.graph,reports.graph),transitivity)
transitivities <- data.frame(tr)
rownames(transitivities) <- c("advice", 'friendship', 'reports')


kable(dyads, label="Dyad Census")
kable(triads, label="Triad Census")
kable(transitivities, label="Transitivities")
 

## From those computations we can observe that advice is the most connected network 
## is the advice net, and the least is report net. Added to that, we can see that report 
## net consist only of asymmetrical and null ties, which is logical, because it is
## hard to imagine co-workers who write reports and subordinate to each other.
## With regard to friendship net, we can see that 56 people call someone a friend, but 
## it was not mutual, sometimes it happens. But 23 ties between workers might be  
## called friendship relations. The advice dyad and triad census provides some info that
## there is only 65 null ties and only 74 003 triads, which gives us a clue about well-developed
## "advice" culture in the net. We can also derive some logical conclusions form the 
## absence of some triad types. For example, there is no 030C triads in friendship 
## net: it is barely  possible that three people can form a closed triad group. 
## We have already mentioned that reports net does not have any mutual ties, but 
## but it also should be noted that the 021D triads are absent. It shows that there is 
## no such supervisor in the organization who has two workers with the report obligation.
 
kable(centralityA, row.names=T, label="Advice Centrality")
kable(centralityF, row.names=T, label="Friendship Centrality")
kable(centralityR, row.names=T, label="Reports Centrality")

##Comparing centralities we can conclude pretty much the same things: reports net
## is the least connected net. Eigen vector centralities show that each net has 
## the most "powerful" nodes: 18, 17, 14, respectively. However, if we compare
## particular nodes from the perspective of all nets, we can outline the 21th 
## node, which has considerable figures in all three nets.
detach(package:igraph)
#_______________________________________________________________________________________________________________________________________________________________________________________________________

library(network)
library(sna)

formal<-as.matrix(read.csv("formal.csv", header = TRUE, row.names=1))
roles<-read.csv("roles.csv", header=TRUE, row.names=1)
formalnet <- network(formal)
par(mar=c(0,0,2,0))
indeg <- degree(formalnet, cmode = 'indegree')
mycoord <- plot(formalnet, displaylabels=TRUE, edge.col='azure4',
                vertex.col="#E41A1C", vertex.border='azure4',
                vertex.cex = indeg + 1 , main ='Downton Abbey',
                label.cex=0.5, label.pos = 5)


plot(formalnet)

orRule <- symmetrize(formalnet, rule='weak') # "or" rule 
class(orRule) # symmetrize transformed the network into a matrix

orRule <- network(symmetrize(formalnet, rule='weak'), directed = FALSE) # 'or' rule
class(orRule) # network
andRule <- network(symmetrize(formalnet, rule='strong'), directed = FALSE) # 'and' rule

warnings() 

par(mar=c(1,1,2,1))
par(mfrow=c(1,3))
plot(formalnet, main = 'Original', coord=mycoord, vertex.cex =3,
     edge.col='azure4', vertex.col="#E41A1C", vertex.border='azure4',
     label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')
plot(orRule, main = 'Or Rule', coord=mycoord, vertex.cex =3,
     edge.col='azure4', vertex.col="#377EB8", vertex.border='azure4',
     label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')
plot(andRule, main = 'And Rule', coord=mycoord, vertex.cex =3,
     edge.col='azure4', vertex.col="#4DAF4A", vertex.border='azure4',
     label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')

snasymmformal <- orRule
aprioriformal<-blockmodel(snasymmformal, roles$commdetect,
                          block.content="density", mode="graph",
                          diag=FALSE)
# We can build what is called a heatmap, showing the relationships between blocks in color
heatmap(aprioriformal[[4]])

aprioriformal[[1]]
aprioriformal[[2]]
aprioriformal[[3]]
aprioriformal[[4]]

library('RColorBrewer')
suppressPackageStartupMessages('RColorBrewer')
par(mar=c(1,1,1,1),mfrow=c(2,3))
col5 <- brewer.pal(5, 'Set1')
cols <- ifelse(aprioriformal[[1]] == 1, col5[1],
               ifelse(aprioriformal[[1]] == 2, col5[2],
                      ifelse(aprioriformal[[1]] == 3, col5[3],
                             ifelse(aprioriformal[[1]] == 4, col5[4], col5[5]))))
par(mar=c(1,1,2,1),mfrow=c(1,1))
plot(snasymmformal, main = 'Apriori Block Model', coord=mycoord,
     vertex.cex =3, edge.col='azure4', vertex.col=cols,
     vertex.border='azure4', label=seq(1:20), label.pos=5,
     label.cex=.5, label.col='gray15')

# Create an object of distances in the "OR rule," and turn it into a vector
distformal <- dist(snasymmformal, method="euclidian", diag=FALSE)
thick <- as.vector(distformal)
# Now, let's visualize these distances as edge thickness
par(mar=c(0.5,0,2,0))
plot(snasymmformal, main = 'Euclidean Distances', coord=mycoord,
     vertex.cex =3, edge.col='azure4', vertex.col=col5[2],
     vertex.border='azure4', label=seq(1:20),label.pos=5,
     label.cex=.5,label.col='gray15', edge.lwd = thick*2)

# Cluster analysis
formalclust <- hclust(distformal, method="complete")

# And now, a blockmodel based on clustering:
exploratoryformal<-blockmodel(snasymmformal, formalclust, k=6,
                              block.content="density", mode="graph",
                              diag=FALSE)
# Plot the two blockmodels one after another for comparison:
par(mar=c(0,0,2,0))
plot.blockmodel(aprioriformal)

plot.blockmodel(exploratoryformal)

heatmap(exploratoryformal[[4]], main ='Exploratory Blockmodel')
#_______________________________________________________________________________________________________________________________________________________________________________________________________


plot(exploratoryformal, main = 'Apriori Block Model', coord=mycoord,
     vertex.cex =3, edge.col='azure4', vertex.col=cols,
     vertex.border='azure4', label=seq(1:20), label.pos=5,
     label.cex=.5, label.col='gray15')
### ---- Assignment task 2 ---- 

##  1. Experiment with k. We’ve set it to 6, but would another number make more sense?
##  2. Which of the two blockmodels appear to be more accurate to you? Why? 
##     Finally, we can make a heatmap of the two blockmodels:


exploratoryformal.01<-blockmodel(snasymmformal, formalclust, k=5,
                                 block.content="density", mode="graph",
                                 diag=FALSE)
# Plot the two blockmodels one after another for comparison:
par(mar=c(0,0,2,0))
plot.blockmodel(exploratoryformal.01)


exploratoryformal.02<-blockmodel(snasymmformal, formalclust, k=7,
                              block.content="density", mode="graph",
                              diag=FALSE)
# Plot the two blockmodels one after another for comparison:
par(mar=c(0,0,2,0))
plot.blockmodel(exploratoryformal.02)


## Answer: We have tried 5 and 7 clusters. From the picture of 5,6,and 7 we can outline
## that with 7 clusters we can separate two groups of nodes with 4 and 3 ties.





#_______________________________________________________________________________________________________________________________________________________________________________________________________

par(mar = c(1,1,4,1), mfrow = c(1,2))
heatmap(aprioriformal[[4]], main ='Apriori Blockmodel')
heatmap(exploratoryformal[[4]], main ='Exploratory Blockmodel')
connectedformal<-formal[-20,-20] # operation on the matrix 
class(connectedformal)

CONCOR <- function(mat, max.iter=1000, epsilon=1e-10){
  mat <- rbind(mat, t(mat)) # stack
  colN <- ncol(mat) # width
  X <- matrix(rep(0, times=colN*colN), nrow=colN, ncol=colN) 
  target.abs.value <- colN * colN - epsilon # convergence target 
  for (iter in 1:max.iter){
    for(i in 1:colN){
      for(j in i:colN){
        X[i,j]<-cor(mat[,i], mat[,j], method=c("pearson"))
      } # end for j
    } # end for i
    mat <- X+(t(X)-diag(diag((X))))
    if (sum(abs(mat)) > target.abs.value) { # test convergence
      #Finished before max.iter iterations
      return(mat) 
    } # end if
  } # end for iterations
  return(mat) # return matrix 
} # end function


rownames(connectedformal) <- row.names(roles)[1:19]
colnames(connectedformal) <- row.names(roles)[1:19]
## connected formal
#Now, run the matrix through the CONCOR function and show the blockmodel:
CONCORFORMAL<-CONCOR(connectedformal)
# You can look at the matrix on your own; we commented it out to save space in the documen
## print(CONCORFORMAL)
heatmap(CONCORFORMAL)

## part 1  -it's blocks from 14 to 19:
part1 <- connectedformal[14:19,14:19] 
colnames(part1) # Who are in this partition?
concor1 <- CONCOR(part1)
heatmap(concor1)

part2 <- connectedformal[1:13,1:13] # isolate the first 13 nodes
# We commented the matrix out, but you can look at it on your own
##part2
concor2 <- CONCOR(part2) # Run through CONCOR 
heatmap(concor2) # Look at the result

part3<-c(1,3,8,9,12,13) # isolate the needed nodes 
part3.1<-part2[part3,part3] # remove the isolates from partition 2 
colnames(part3.1) # Who is here?

part3.2 <- part2[-part3,-part3] # Extract remaining nodes from part2 
concor3.2 <- CONCOR(part3.2) # Run it through CONCOR 
heatmap(concor3.2)

colnames(part3.2[1:2,1:2]) # Names in the first subpart

colnames(part3.2[3:7,3:7]) # Names in the second subpart

part3.2.2 <- part3.2[3:7,3:7] # Create a partition
# Code below will choke RMarkdown, run it in CONSOLE ONLY (it's commented out here):
##concor3.2.2<-CONCOR(part3.2.2)

# Set up an example matrix.
mat <- matrix(rbinom(25, 1, 0.5), nr = 5, nc = 5)
colnames(mat) <- c('A','B','C','D','E')
rownames(mat) <- c('A','B','C','D','E')
# Stack the matrix with its transpose
mat

t(mat)

# what if this was a symmetrical matrix? Would it work? -- think about it. :-)


#_______________________________________________________________________________________________________________________________________________________________________________________________________

### ---- Assignment task 3 ---- 
##Assignment task 3
## Try not to get lost in all the partitions! Please list all the finite 
## block-partitions that we have generated and the names of all people 
## that ended up in every block


colnames(part1)
colnames(part2)
colnames(part3.1)
colnames(part3.2)
colnames(part3.2.2)

?data.frame
colN <-sapply(list(part1,part2,part3.1,part3.2,part3.2.2 ),colnames)
colnames(part1)
df1<-data.frame(sort(colnames(part1)))
df2<-data.frame(sort(colnames(part2)))
df3<-data.frame(sort(colnames(part3.1)))
df4<-data.frame(sort(colnames(part3.2)))
df5<-data.frame(sort(colnames(part3.2.2)))


colnames(df1)<-c('part1')
colnames(df2)<-c('part2')
colnames(df3)<-c('part3.1')
colnames(df4)<-c('part3.2')
colnames(df5)<-c('part3.2.2')

kable(df1)
kable(df2)
kable(df3)
kable(df4)
kable(df5)


##For markdown
install.packages('kableExtra')
library(kableExtra)

table.1985 %>%
  kable_styling(full_width = F, position = "float_left")

table.2015 %>%
  kable_styling(full_width = F, position = "right")

#_______________________________________________________________________________________________________________________________________________________________________________________________________
mat <- rbind(mat,t(mat))
# Then concor makes a square matrix of 0s of the same dimensions as mat's width
X <- matrix(rep(0, times=5*5), nrow=5, ncol=5)
X

# Then for each cell in X it puts the correlation between the stack matrix's # columm of Xrow and the colum of Xcol
X[2,4]<-cor(mat[,2], mat[,4], method=c("pearson")) ##
X

## cov(X,Y) = E[(X - MuX)*(Y - MuY)]
## and sd(X) = sqrt(E( X - MuX)
###
# The function works until it finishes filling the X matrix once for each cell in X
for(i in 1:5){ for(j in i:5){
  X[i,j]<-cor(mat[,i], mat[,j], method=c("pearson")) } # end for j
} # end for i X

##install.packages("NetData")
library(NetData)
# Pull the dataset out - same way as we did with Kracknets:
data(studentnets.M182, package = "NetData")
# Check the data we have in the dataset:
head(m182_full_data_frame)
m182_full_nonzero_edges <- subset(m182_full_data_frame,
                                  (friend_tie > 0 | social_tie > 0 | task_tie > 0))
head( m182_full_nonzero_edges) 
m182_full_nonzero_edges
suppressPackageStartupMessages(library(igraph)) 
m182_full <- graph.data.frame(m182_full_nonzero_edges) 
summary(m182_full) #check the data

m182_friend <- delete.edges(m182_full, E(m182_full)[E(m182_full)$friend_tie==0])
m182_social <- delete.edges(m182_full, E(m182_full)[E(m182_full)$social_tie==0])
m182_task <- delete.edges(m182_full, E(m182_full)[E(m182_full)$task_tie==0])
#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 4 ---- 
## Why do we remove the zero edges from networks? 
## We haven’t done it previously, why are we doing it now?
class(m182_full_data_frame)
class(m182_full_data_frame)
?network
## Because previously we have not used nets to create edge attributes, and zero 
## edges might provoke distortion in attribute figures, as missing values might
## do with classical data.
 
#_______________________________________________________________________________________________________________________________________________________________________________________________________

# This is if we want to use the edge value
task_adjacency<-get.adjacency(m182_task, attr='task_tie') # This is if we only want the tie (so it's 0 or 1) 
binary_task_adjacency<-get.adjacency(m182_task)

task_adjacency<-as.matrix(task_adjacency) #generate the matrix out of a graph # Create a nx2n matrix of directed connections 
task_matrix<-rbind(task_adjacency,t(task_adjacency))
# Same for matrix of social connections:
social_adjacency<-get.adjacency(m182_social, attr='social_tie') 
binary_social_adjacency<-get.adjacency(m182_social) #this is for later 
social_adjacency<-as.matrix(social_adjacency) 
social_matrix<-rbind(social_adjacency,t(social_adjacency))
# Because we want to analyze social and task connections together, bind matrices:
task_social_matrix <-rbind(task_matrix,social_matrix)
dim(task_social_matrix)

task_social_cors<-cor(task_social_matrix) # Correlate matrices
## task_social_cors #If you want to check the matrix, uncomment this line

dissimilarity<-1-task_social_cors #subtract matrix values from 1 
task_social_dist<-as.dist(dissimilarity) #create a distance matrix
#You can check the matrix if you wish:
##task_social_dist
task_social_dist<-dist(t(task_social_matrix))
task_social_dist
library(NetCluster) # add the library to complete the clustering 
task_social_hclust <- hclust(task_social_dist) 
plot(task_social_hclust)

cutree(task_social_hclust, k=2)
clustered_observed_cors = vector() # set it as a vector 
num_vertices = length(V(m182_task)) # get the length of the vector

clustered_observed_cors <-clustConfigurations(num_vertices,task_social_hclust,task_social_cors)
clustered_observed_cors$correlations

num_clusters = 4 # Test our number of clusters
clusters <- cutree(task_social_hclust, k = num_clusters) 
clusters

cluster_cor_mat <- clusterCorr(task_social_cors,
                               clusters)
cluster_cor_mat
gcor(cluster_cor_mat, task_social_cors)
#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 5 ---- 

## What rationale do you have for selecting the number of clusters / 
## positions with the method above? 
## Please rely on your knowledge of cluster analysis to answer this question.
## Answer: First of all, we can observe the scree plot and look for big changes
## in delta of the correlation coefficient. Also, in cluster analysis it is important
## to look at cluster stability(how often elements jump to another cluster with  
## the increase of k), but we do not have the relevant info. 

#_______________________________________________________________________________________________________________________________________________________________________________________________________

apriori = c(1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 3)
deductive_cluster_cor_mat <- generate_cluster_cor_mat(task_social_cors, apriori)
gcor(deductive_cluster_cor_mat, task_social_cors)

# Blockmodel on valued task data
task_valued_blockmodel <- blockmodel(task_adjacency, clusters)
# Blockmodel on binary task data binary_task_adjacency<-as.matrix(binary_task_adjacency) # turn graph to matrix first task_binary_blockmodel <- blockmodel(binary_task_adjacency, clusters)
# Blockmodel on valued social data
social_valued_blockmodel <- blockmodel(social_adjacency, clusters)

#Blockmodel on binary social data
binary_social_adjacency<-as.matrix(binary_social_adjacency)
social_binary_blockmodel <- blockmodel(binary_social_adjacency, clusters)


plot(social_binary_blockmodel)
# Now, look at the basic statistics:
task_mean <- mean(task_adjacency)
task_mean


task_density <- graph.density(m182_task)
task_density

social_mean <- mean(social_adjacency)
social_mean

social_density <- graph.density(m182_social)
social_density


#_______________________________________________________________________________________________________________________________________________________________________________________________________

col5 <- brewer.pal(5, 'Set1')
cols <- ifelse(aprioriformal[[1]] == 1, col5[1],
        ifelse(aprioriformal[[1]] == 2, col5[2],
        ifelse(aprioriformal[[1]] == 3, col5[3],
        ifelse(aprioriformal[[1]] == 4, col5[4], col5[5]))))
par(mar=c(1,1,2,1),mfrow=c(1,1))
plot(snasymmformal, main = 'Apriori Block Model', coord=mycoord,
     vertex.cex =3, edge.col='azure4', vertex.col=cols,
     vertex.border='azure4', label=seq(1:20), label.pos=5,
     label.cex=.5, label.col='gray15')
### ---- Assignment task 6 ---- 
##
## • Plot the resulting blockmodels in any way you wish and examine them visually. 
##   What is the story you get from viewing these clusters, and their within and
##   between cluster densities on task and social interaction? What can you say about 
##   your network from this?

exploratoryadjacency<-blockmodel(task_adjacency, clusters, k=8,
                              block.content="density", mode="graph",
                              diag=FALSE)


plot.blockmodel(task_valued_blockmodel)
plot.blockmodel(social_valued_blockmodel)
plot.blockmodel(social_binary_blockmodel)

plot.blockmodel(exploratoryadjacency)

## • We have learned several ways to blockmodel. Which method do you find the most
##   intuitively appealing? Why?
##   Answer, CONCOR method was new and interesting, but it is failing our console, so
##   we prefer good old cluster analysis, as we have known it before and it was easy 
##   to apply

## • What did you learn from blockmodels about your data that you could not 
##   generate from previously learned techniques?
##   We have equipped ourselves with skills about how to divide nets and nets data 
##   into groups. Before we have studied how to calculate the probabilities for 
##   the formation of a new tie with ergm model, but now we also can detect subgroups 
##   in the net.

#_______________________________________________________________________________________________________________________________________________________________________________________________________
