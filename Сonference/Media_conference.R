library('tidyverse')
library('ggplot2')
library('tidytext')
library('widyr')
library('ggraph')
library('igraph')

library("ggplot2")
library("foreign")
library("readxl")
library(intergraph)



covid <- read_excel("fake_new_dataset.xlsx")
View(covid)
covid_true <- subset(covid, subset = covid$label == "1")
covid_fake <- subset(covid, subset = covid$label == "0")

#---- FAKE NEWS ANALYSIS ----
covid_fake_new <- covid_fake %>%
  filter(!is.na(title)) %>%
  transmute(post_id = row_number(), title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(word != "de",
         str_detect(word, "[a-z]"))
  



  

install.packages("extrafont")
library(extrafont) 

loadfonts(device = "win")

windowsFonts(Times=windowsFont("TT Times New Roman"))
#install.packages("gridExtra")
library(gridExtra)


covid_fake_new %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n))+
  geom_col(width = 0.75,
           col = "white",
           fill = "#99d8c9")+
  coord_flip()+
  labs(title = "Common words in fake news titles")+
  theme_classic()+
  theme(text = element_text(family = "Times", face = "bold", size = 14))

 
covid_fake_filtered <- covid_fake_new %>%
  add_count(word) %>%
  filter(n >= 10)

covid_fake_all <- covid_fake_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) 

names(covid_fake_all)[names(covid_fake_all) == 'item1'] <- 'word'
names(covid_fake_all)[names(covid_fake_all) == 'item2'] <- 'word2'
covid_fake_all

Graph_all_F <- graph_from_data_frame(covid_fake_all)
png("mygraph.png", heigh=4000, width=6000)
plot(Graph_all_F, layout= layout_nicely(Graph_fake_all), vertex.size = 10, label.cex = 100)
dev.off()



top_word_covid_fake <- covid_fake_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) %>%
  head(100)



#---- GRAPHS FAKE NEWS ANALYSIS ----
set.seed(2020)


?ggraph
ggraph(Graph_fake_all, layout = layout_nicely(graph_from_data_frame(covid_fake_new_all)))+
  geom_edge_arc(aes(colour=correlation, alpha=correlation))+
  coord_fixed()+
  geom_node_text(aes(label = name), repel = TRUE)+
  geom_node_point()+
  theme_void()+
  scale_edge_color_continuous(low="red", high="green")+
  theme(legend.position = "none")

covid_fake_new_all %>%
  graph_from_data_frame() %>%
  ggraph(layout = layout_nicely(graph_from_data_frame(covid_fake_new_all)))+
  geom_edge_arc(aes(colour=correlation, alpha=correlation))+
  coord_fixed()+
  geom_node_text(aes(label = name), repel = TRUE)+
  geom_node_point()+
  theme_void()+
  scale_edge_color_continuous(low="red", high="green")+
  theme(legend.position = "none")

Graph_top_F <- graph_from_data_frame(top_word_covid_fake)

top_word_covid_fake %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'linear', circular = TRUE)+
  geom_edge_arc(aes(colour=correlation, alpha=correlation))+
  coord_fixed()+
  geom_node_text(aes(label = name), repel = TRUE)+
  geom_node_point()+
  theme_void()+
  scale_edge_color_continuous(low="red", high="green")+
  theme(legend.position = "none")

top_fake<-top_word_covid_fake %>% 
  # Count how many word per value
  inner_join(afinn, "word") %>% 
  group_by(value) %>% 
  count(value, sort=T)  %>% 
  


Graph_emotions_F

# Count how many word per value


plot(Graph_top_F)
#---- TRUE NEWS ANALYSIS ----
covid_true_new <- covid_true %>%
  filter(!is.na(title)) %>%
  transmute(post_id = row_number(), title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(word != "de",
         str_detect(word, "[a-z]"))

covid_true_new %>%
  count(word, sort = TRUE) %>%
  mutate(word = fct_reorder(word, n)) %>%
  head(20) %>%
  ggplot(aes(word, n))+
  geom_col(width = 0.75,
           col = "white",
           fill = "#99d8c9")+
  coord_flip()+
  labs(title = "Common words in true news titles")+
  theme_classic()+
  theme(text = element_text(family = "Times", face = "bold", size = 14))

covid_true_filtered <- covid_true_new %>%
  add_count(word) %>%
  filter(n >= 10)

top_word_covid_true <- covid_true_filtered %>%
  select(post_id, word) %>%
  pairwise_cor(word, post_id, sort = TRUE) %>%
  head(100)

set.seed(2020)
top_word_covid_true %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'linear', circular = TRUE)+
  geom_edge_arc(aes(colour=correlation, alpha=correlation))+
  coord_fixed()+
  geom_node_text(aes(label = name), repel = TRUE)+
  geom_node_point()+
  theme_void()+
  scale_edge_color_continuous(low="red", high="green")+
  theme(legend.position = "none")



#----   FAKE SENTIMENTAL ANALYSIS ----
library('reshape2')
library(wordcloud)    
library(wordcloud2)

afinn <- read_csv("Afinn.csv",
                  col_types = cols(word = col_character(), value = col_double()))
bing <- read_csv("Bing.csv",
                 col_types = cols(word = col_character(), sentiment = col_character()))
nrc <- read_csv("NRC.csv",
                col_types = cols(word = col_character(), sentiment = col_character()))



library(RColorBrewer)
options(repr.plot.width=15, repr.plot.height=15)
colsR_B<- brewer.pal(4,name = 'RdBu')


display.brewer.pal(4,name = 'RdBu' )
# ----- WORDCLOUDS FAKE----
covid_fake_new %>% 
  inner_join(bing, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud(colors=colsR_B[c(1, 4)], max.words = 400, title.size = 2,
                   scale = c(3,.5))

# ----- CONTRIBUTION FAKE----
options(repr.plot.width=15, repr.plot.height=9)

names(top_word_covid_fake)[names(top_word_covid_fake) == 'item1'] <- 'word'
names(top_word_covid_fake)[names(top_word_covid_fake) == 'item2'] <- 'word2'
names(top_word_covid_true)[names(top_word_covid_true) == 'item1'] <- 'word'
names(top_word_covid_true)[names(top_word_covid_true) == 'item2'] <- 'word2'
top_word_covid_fake

top_word_covid_fake %>% 
  # Count how many word per value
  inner_join(afinn, "word") %>% 
  group_by(value) %>% 
  count(value, sort=T)  %>% 
  
  # Plot
  ggplot(aes(x=value, y=n)) +
  geom_bar(stat="identity", show.legend = F, width = 0.5, fill = colsR_B[4]) +
  geom_label(aes(label=format(n, big.mark = ",")), size=5) +
  scale_x_continuous(breaks=seq(-5, 5, 1)) +
  labs(x="Score", y="Frequency", title="Word count distribution over intensity of sentiment: Neg - Pos") +
  my_theme + theme(axis.text.y = element_blank())





options(repr.plot.width=15, repr.plot.height=9)

unnest_tweets %>% 
  # by word and value count number of occurences
  inner_join(afinn, "word") %>% 
  count(word, value, sort=T) %>% 
  mutate(contribution = n * value,
         sentiment = ifelse(contribution<=0, "Negative", "Positive")) %>% #another variable
  arrange(desc(abs(contribution))) %>% 
  head(20)  %>% 
  
  # plot
  ggplot(aes(x=reorder(word, contribution), y=contribution, fill=sentiment)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  labs(x="Word", y="Contribution", title="Words with biggest contributions in positive/negative sentiments") +
  coord_flip() +
  scale_fill_manual(values=my_colors[c(3, 2)]) + 
  my_theme

#----TRUE SENTIMENTAL ANALYSIS ----

# ----- WORDCLOUDS TRUE----
covid_true_new %>% 
  inner_join(bing, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud(colors=colsR_B[c(1, 4)], max.words = 400, title.size = 2,
                   scale = c(3,.5))

# ----- CONTRIBUTION TRUE----

# ----- RADAR CHART ----
library('radarchart')

covid_fake_new1<-covid_fake_new  
covid_true_new1<-covid_true_new

FAKE <-c()
for (i in 1:length(covid_fake_new1$post_id)){
  FAKE<- append(FAKE,'FAKE')
}
FAKE


RIGHT <-c()
for (i in 1:length(covid_true_new1$post_id)){
  RIGHT<- append(RIGHT,'TRUE')
}
RIGHT

covid_fake_new1$status <- FAKE 
covid_true_new1$status<-RIGHT

covid_new<-rbind(covid_fake_new1, covid_true_new1)

char_sentiment <- covid_new %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  group_by(status, sentiment) %>% 
  count(status, sentiment) %>% 
  select(status, sentiment, char_sentiment_count=n)

# Total Count of sentiments per countries
total_char <- covid_new %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(status) %>% 
  select(status, total=n)

# Radar Chart:
plt <- char_sentiment %>% 
  inner_join(total_char, by="status") %>% 
  mutate(percent = char_sentiment_count / total * 100 ) %>% 
  select(-char_sentiment_count, -total) %>% 
  spread(status, percent)  %>% 
  chartJSRadar(showToolTipLabel = T, main="Fake and True News Emotion", maxScale=25, responsive=T,
               addDots = T, 
               colMatrix = grDevices::col2rgb(colsR_B[c(1, 4)]),
               lineAlpha = 0.7, polyAlpha = 0.2)
plt
colsR_B[c(1, 4)]
# saveWidget(plt, "plt.html")


# ----- ERGM FAKE----

top_fake_nrc <- inner_join(top_word_covid_fake, nrc, "word")

top_fake_bing<-inner_join(top_word_covid_fake, bing, by="word")

top_fake_afinn<-inner_join(top_word_covid_fake, afinn, by="word")

top_word_covid_true

all_fake_nrc <- inner_join(covid_fake_all, nrc, "word")

all_fake_bing<-inner_join(covid_fake_all, bing, by="word")

all_fake_afinn<-inner_join(covid_fake_all, afinn, by="word")

top_true_nrc <- inner_join(top_word_covid_true, nrc, "word")

top_true_bing<-inner_join(top_word_covid_true, bing, by="word")

top_true_afinn<-inner_join(top_word_covid_true, afinn, by="word")

detach(package:igraph)
detach(package:ergm)
detach(package:network)
detach(package:sna)
library(network)
library(sna)
library(ergm)
library(igraph)

Graph_emotion_F<-graph_from_data_frame(top_fake_nrc)
set.vertex.attribute(Graph_emotion_F, emotion, value = c(top_fake_nrc))

Graph_all_bing_F<-graph_from_data_frame(all_fake_bing)
emotion_vector<-vector()

for(i in 1:49){ # this is our set of all network nodes
  for(j in 1:11){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(Graph_all_bing_F)$name[i]==attributes_fake_bing$word[j]) {
      #if we match, we add the attribute to a vector
      emotion_vector[i]<-attributes_fake_bing$sentiment[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{emotion_vector[i]<-0}
  }
}
emotion_vector

emotion_vector1<-vector()

for(i in 1:38){ # this is our set of all network nodes
  for(j in 1:10){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(Graph_top_F)$name[i]==attributes_fake_bing$word[j]) {
      #if we match, we add the attribute to a vector
      emotion_vector1[i]<-attributes_fake_bing$sentiment[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{emotion_vector1[i]<-0}
  }
}
emotion_vector1

set.vertex.attribute(Graph_all_bing_F, "emotion", value = emotion_vector)
plot(Graph_all_bing_F)

Graph_top_F <- graph_from_data_frame(top_word_covid_fake)
Graph_top_T <- graph_from_data_frame(top_word_covid_true)

set.edge.attribute(Graph_top_F, cor, value = top_word_covid_fake$correlation)

Graph_all_NRC_F <- graph_from_data_frame(all_fake_nrc)

Graph_all_Bing_F <- graph_from_data_frame(all_fake_bing)

Graph_all_AFINN_F <- graph_from_data_frame(all_fake_afinn)



vector_fake_nrc <-data.frame(word = V(Graph_all_NRC_F)$name)
vector_fake_bing<-data.frame(word = V(Graph_all_Bing_F)$name)
vector_fake_afinn<-data.frame(word = V(Graph_all_AFINN_F)$name)

attributes_fake_nrc <-inner_join(vector_fake_nrc,nrc, by="word")
attributes_fake_bing<-inner_join(vector_fake_bing,bing, by="word")
attributes_fake_afinn<-inner_join(vector_fake_afinn,afinn, by="word")

##creating Attrinbutes


set.vertex.attribute(Graph_all_Bing_F, 'sentiment', value = attributes_fake_bing$sentiment)





Net_top_F<-asNetwork(Graph_top_F)
Net_top_T<-asNetwork(Graph_top_T)

Net_all_F <-asNetwork(Graph_all_F)
Net_emotion_F <-asNetwork(Graph_emotion_F)
Net_emotion_F



test_model<-ergm(Net_top_F~edges)
summary(test_model)

test_model_02<-ergm(Net_top_F~edges+mutual)
summary(test_model_02)
mcmc.diagnostics(test_model_02)

invlogit <- function(x) {1/(1 + exp(-x))} 

invlogit(test_model$coefficients[1])


test_model2.0<-ergm(Net_all_F~edges)
summary(test_model2.0)

test_model2.0<-ergm(Net_top_F~edges+nodefactor('sentiment'))
summary(test_model2.0)

test_model_02<-ergm(Net_top_F~edges+gwesp(0.1,fixed=T)+nodeofactor('sentiment'))
summary(test_model_02)
mcmc.diagnostics(test_model_02)


Net__all_Bing_F<-asNetwork(Graph_all_bing_F)
set.vertex.attribute(Net__all_Bing_F, "sentiment", emotion_vector, v=seq_len(network.size(Net__all_Bing_F)))
Net_top_F
set.vertex.attribute(Net_top_F, 'sentiment', emotion_vector1, v=seq_len(network.size(Net_top_F)))

Net__all_Bing_F %v% "sentiment" <- emotion_vector
Net__all_Bing_F

test_model3.0<-ergm(Net__all_Bing_F~edges+asymmetric+ gwesp(0.8,fixed=T))
summary(test_model3.0)

test_model3.1<-ergm(Net__all_Bing_F~edges+nodematch('sentiment'))
summary(test_model3.1)
delete.vertex.attribute(Net__all_Bing_F, "sentiment")
 

# ----- CENSUS FAKE----
library(knitr)
library('gridExtra')
pdf("trade.pdf", height=11, width=8.5)
grid.table(df)
dev.off()
centralitiesF <- data.frame(row.names   = V(Graph_top_F)$name,
                          degree      = degree(Graph_top_F),
                          betweenness = betweenness(Graph_top_F),
                          closseness = closeness(Graph_top_F),
                          eigenvector = evcent(Graph_top_F)$vector, 
                          pagerank = page_rank(Graph_top_F)$vector)
pdf("centralities_fake.pdf", height=11, width=8.5)
grid.table(centralitiesF)
dev.off()

kable(centralities)

centralitiesT <- data.frame(row.names   = V(Graph_top_T)$name,
                           degree      = degree(Graph_top_T),
                           betweenness = betweenness(Graph_top_T),
                           #closseness = closeness(Graph_top_T),
                           eigenvector = evcent(Graph_top_T)$vector, 
                           pagerank = page_rank(Graph_top_T)$vector)
pdf("centralities_true.pdf", height=40, width=8.5)
grid.table(centralitiesT)
dev.off()

kable(centralities)




types <-c('003','012','102','021D','021U','021C','111D','111U','030T','030C','201','120D','120U','120C', '210', '300')
t<-sapply(list(Graph_top_F, Graph_top_T),triad_census)
colnames(t) <-c('FAKE', 'TRUE')
triads <- data.frame(t)
rownames(triads) <- types

pdf("triads.pdf", height=11, width=8.5)
grid.table(triads)
dev.off()
kable(triads, label="Triad Census")

tr <-sapply(list(Graph_top_F, Graph_top_T),transitivity)
transitivities <- data.frame(transitivity=tr)
rownames(transitivities) <- c("FAKE", 'TRUE')
pdf("transitivity.pdf", height=11, width=8.5)
grid.table(transitivities)
dev.off()

d<-sapply(list(Graph_top_F, Graph_top_T),dyad.census)
dyads <- data.frame(d)
colnames(dyads) <- c("FAKE", 'TRUE')
rownames(dyads) <- c("mut", 'asym', 'null')
pdf("dyads.pdf", height=1100, width=8.5)
grid.table(dyads, rows=c("mut", 'asym', 'null'))
dev.off()

den <-sapply(list(Graph_top_F, Graph_top_T),edge_density)
density <- data.frame(density=den)
rownames(density) <- c("FAKE", 'TRUE')
pdf("density.pdf", height=11, width=8.5)
grid.table(density)
dev.off()

kable(dyads, label="Dyad Census")

