install.packages('CINNA')
install.packages('knitr')
library('RColorBrewer')                                                                                                                                                         
library('network')                                                                                                                                                                                                                                                                                                                               
library('sna')
##library('igraph')                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
library('intergraph')
library('CINNA')
library('knitr')

#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
``
detach(package:igraph)

load('flo.Rdata')

flo.marriage<- as.network(as.matrix(flo.marriage))
plot(flo.marriage)
network.size(flo.marriage)

FloColors <- c(brewer.pal(11,'RdYlBu'),brewer.pal(9,'RdYlBu')) #set a vector of colors
par(mar=c(0,0,0,0))
plot(flo.marriage,
    #vertex.cex=(get.vertex.attribute(flomarriage, 'wealth')/25 +.4), 
     displaylabels=TRUE,
     label.cex=.5,
     label.pos=0,
     vertex.col=FloColors)

network.dyadcount(flo.marriage)
dyad.census(flo.marriage)

network.edgecount(flo.marriage) 

network.density(flo.marriage)
triad.census(flo.marriage)

gtrans(flo.marriage, measure='weak')

##_________________________________________________________________________________________________________________________________________________________________________________
## ----- Assignment question 2 ----- 
#Given the explanation of transitivity provided, how do you evaluate 
# the transitivity coefficient obtained with the above command?

gtrans(flo.marriage, measure='weak')
## Let us create one random directed graph with the same amount of nodes
num_nodes <- 16
my_matrix<-matrix(round(runif(num_nodes*num_nodes)), ncol = num_nodes)
diag(my_matrix) <- 0

my_network<-as.network(x = my_matrix, directed = TRUE )
gtrans(my_network, measure='weak')

# The observed figure of transitivity is less than random figures. Thus, in Medici's 
# times, families tend to form less ties than possible. It gives us a hint that 
# something restricts them from creating possible connections. 


# ----- Assignment question 3 ----- 
# Here, we showed you how to calculate this measure and its different types,
# but the information you get will be meaningless. Why?.

grecip(flo.marriage, measure = 'dyadic') # why not 0?

grecip(flo.marriage, measure = 'dyadic.nonnull')

grecip(flo.marriage, measure = 'edgewise')

grecip(flo.marriage, measure = 'edgewise.lrr')

grecip(flo.marriage, measure = 'correlation')

# Answer: In that case, those commands are meaningless, because ties represent
# marriage and business relations which are reciprocal by definition.
?grecip

detach(package:sna)
detach(package:network)
library(igraph)
suppressPackageStartupMessages(library(intergraph))
flo.graph<-asIgraph(flo.marriage)

library(igraph) #do not forget to attach the library
indegree <- degree(flo.graph, mode="in")
outdegree<-degree(flo.graph, mode="out")
total<-degree(flo.graph, mode="total") # This is also called Freeman's degree
between<-betweenness(flo.graph)
between
par(mar=c(0,0,0,0))
plot(flo.graph)

ecentV<-evcent(flo.graph)
eigen<-ecentV$vector

node<-flo.att$flo.names # If you look at the attributes file, you'll find the names of fam #Now, create the table that we need:
table<-data.frame(node, indegree, outdegree, between, close, eigen)
#Change the header names 
names(table)<-c("Family","Indegree","Outdegree","Betweenness","Closeness","Eigenvector")
suppressPackageStartupMessages(library(knitr))
kable(table, digits=3, caption = "Florentine Families' Network Centralities")

# Set dimensions for table of correlations
cor_table<-matrix(nrow=5, ncol=5) #We have five centralities
# Apply the cor.test function to correlate measures in our table cor_table[lower.tri(cor_table)] <- apply(which(lower.tri(cor_table), arr.ind=TRUE)+1, 1, function(a) cor.test(table[,a[1]], table[,a[2]])$estimate)
# Let's look at what we got:
cor_table

# Give the same column names as to our original table
colnames(cor_table)<-c("Indegree","Outdegree","Betweenness","Closeness","Eigenvector") 
rownames(cor_table)<-c("Indegree","Outdegree","Betweenness","Closeness","Eigenvector") 
cor_table<-cor_table[2:5,1:4] # drop the extra row and column
kable(cor_table, caption="Correlations of Florentine Families' Network Centralities")

library(CINNA)
## Registered S3 method overwritten by 'GGally':
##   method from
##   +.gg   ggplot2
#First, extract components from the flo.graph network:
comps<-graph_extract_components(flo.graph) #Let's look at them:

Comp1<-comps[[1]] #We use the first component to extract centralities from 
pr_cent<-proper_centralities(Comp1)


NewCent<-calculate_centralities(Comp1, include = c(pr_cent[1:5], pr_cent[19])) 
NewCent<-as.data.frame(NewCent)
# Remove node 12 from the list of names
# Note how I have to do some acrobatics with data to accomplish this
# Otherwise, R will turn all characters into numbers
node<-c(as.character(flo.att$flo.names[1:11]),as.character(flo.att$flo.names[13:16]))
#Add a column of names to the table
# Make sure names are not converted back to numbers with stringsAsFactors = FALSE command: NewCent<-data.frame(node,NewCent, stringsAsFactors = FALSE)
names(NewCent)<-c("Family", "Alpha", "Bonacich", "Page Rank", "Average Distance", "BaryCenter", "Katz")
kable(NewCent, digits=3, caption = "Florentine Families' Additional Network Centralities")
