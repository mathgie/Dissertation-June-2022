###Note: this code is not a script that runs from beginning to end, article 1 and 
##2 are both included, so the individual sections must be run for the analysis 
##of interest

##Load Libraries
options(java.parameters = "- Xmx2048m")

library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(readxl)
library(xlsx)
library(writexl)
library(topicmodels)
library(tidytext)
library(tidyr)
library(textmineR)
library(reshape2)
library(syuzhet)
library(lda)
library(ldatuning)
library(igraph)
library(ggraph)
library(stringr)

##Load in data 
maggie_data_v6_drops <- read_excel("C:/Users/Maggie/Downloads/maggie_data_v7_collapsed domains.xlsx")
View(maggie_data_v6_drops)



##Start Bag of Words Code

##Loading in data and exploring how it looks
Test_Items <- maggie_data_v6_drops

View(Test_Items)

items <- Test_Items
items

##removing the 8 items that get cleaned to nothing

corp_8 <- items %>%
  filter(!id %in% c(335, 3227, 3122, 3202, 4129, 3691, 2416, 2505))


nrow(corp_8)

qstems <- items$completeq


head(qstems)

stems_source <- VectorSource(qstems)

stems_corpus <- VCorpus(stems_source)

stems_corpus



###Article 2
##Code for a stand-alone bigrams analysis - with cleaned words
qstems_bigrams <- items %>%
  unnest_tokens(bigram, completeq, token = "ngrams", n = 2)
qstems_bigrams

qstems_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- qstems_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered

bigrams_filtered2 <- bigrams_filtered %>%
  filter(!str_detect(word1, "\\d"), 
         !str_detect(word2, "\\d"))

# new bigram counts
bigram_counts <- bigrams_filtered2 %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

View(bigram_counts)


##Code for a stand-alone trigrams analysis - with cleaned words
qstems_trigrams <- items %>%
  unnest_tokens(trigram, completeq, token = "ngrams", n = 3)
qstems_trigrams

qstems_trigrams %>%
  count(trigram, sort = TRUE)

trigrams3 <-qstems_trigrams %>%
  count(trigram, sort = TRUE)

trigrams_separated <- qstems_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)
trigrams_filtered

trigrams_filtered2 <- trigrams_filtered %>%
  filter(!str_detect(word1, "\\d"), 
         !str_detect(word2, "\\d"),
         !str_detect(word3, "\\d"))

# new triigram counts:
trigram_counts <- trigrams_filtered2 %>% 
  count(word1, word2, word3,  sort = TRUE)

trigram_counts

View(trigram_counts)



##Code for a stand-alone bigrams analysis - with cleaned words
qstems_bigrams <- items %>%
  unnest_tokens(bigram, completeq, token = "ngrams", n = 2)
qstems_bigrams

qstems_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- qstems_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered

bigrams_filtered2 <- bigrams_filtered %>%
  filter(!str_detect(word1, "\\d"), 
         !str_detect(word2, "\\d"))

# new bigram counts:
bigram_counts <- bigrams_filtered2 %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

View(bigram_counts)

library(stringr)

bigram_graph <- bigram_counts %>%
  filter(n > 40,) %>%
  graph_from_data_frame()

bigram_graph

#visualize the pairs - only works with VERY small datasets or very powerful processors
library(igraph)
set.seed(2017)
ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)








##Initial Document Term Matrix before cleaning for stop words
fullcorpus_dtm <-DocumentTermMatrix(stems_corpus)
fullcorpus_dtm


##Calculation for initial overall word count before cleaning = word count per question
##includes stop words
full_m <- as.matrix(fullcorpus_dtm)

full_frequency <- rowSums(full_m)
full_frequency

typeof(full_frequency)

View(full_frequency)

summary(full_frequency)

mean(full_frequency)
sd(full_frequency)





##Function to clean the corpus
##add in custom stop words here, as needed - add after the parentheses close
##for function corpus4. Can put in any words that make sense
clean_corpus <- function(stems_corpus){
  corpus <- tm_map(stems_corpus, stripWhitespace)
  corpus2 <- tm_map(corpus, removePunctuation)
  corpus3 <- tm_map(corpus2, content_transformer(tolower))
  corpus4 <- tm_map(corpus3, removeWords, c(stopwords("en")))
  corpus5 <- tm_map(corpus4, removeNumbers)
  return(corpus5)
}

#in case you need to see which words are in the  originla stopwords list. 
stopwords("en")

##Checking to make sure it was cleaned
clean_corp <- clean_corpus(stems_corpus)

clean_corp[[20]][1]

clean_corp


##Making a DTM with the cleaned data
stems_dtm <- DocumentTermMatrix(clean_corp)
stems_dtm
head(stems_dtm)

stems_m <- as.matrix(stems_dtm)
dim(stems_m)

##Checking a portion of the DTM
stems_m[14:16, 100:105]



##Calculating overall word count per question now it's clean = overall count 
##for significant words per question
stems_frequency <- rowSums(stems_m)
stems_frequency

for(p in stems_frequency) {
  print(p)
}

summary(stems_frequency)

##To check for rows that have been cleaned to 0 for removal (can run before this)

rowTotals <- apply(stems_dtm , 1, sum)
dtmZeros  <- stems_dtm[rowTotals=0, ]
loop_index_1 <- 0 

for(p in stems_frequency) {
  loop_index_1 <- loop_index_1 + 1
   if(p == 0 ) {
    print(loop_index_1)  
    }
}





##Code to Make a Term Document Matrix (needed to make the bar plot)
stems_tdm <-TermDocumentMatrix(clean_corp)
stems_tdm


stems2_m <- as.matrix(stems_tdm)

dim(stems2_m)

stems2_m[100:105, 14:16]


##Generate barplot for overall term frequency
term_frequency <- rowSums(stems2_m)
term_frequency <- sort(term_frequency, decreasing = T)
term_frequency[1:50]
barplot(term_frequency[1:20], col = "blue", las = 3)


##Making a word cloud
head(term_frequency)


term_word_freqs <- data.frame(
  term = names(term_frequency),
  num = term_frequency
)

head(term_word_freqs)


wordcloud(term_word_freqs$term, term_word_freqs$num,
          max.words = 120, colors = "cadetblue")

##associations code - the correlation you put in is a <= value, it is the minimum
##threshold you will accept
findAssocs(stems_tdm, 'number', 0.15)
findAssocs(stems_tdm, 'total', 0.15)
findAssocs(stems_tdm, 'square', 0.15)
findAssocs(stems_tdm, 'shows', 0.15)
findAssocs(stems_tdm, 'feet', 0.15)
findAssocs(stems_tdm, 'value', 0.15)


m2 <- as.matrix(stems_tdm)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward")
plot(fit)
rect.hclust(fit, k=10)





##### Begin Sentiment Analysis

##Code to unnest the tokens from the larger corpus, breaks each line of text into composite words
try_1 <- items %>%
  unnest_tokens(word, completeq)

try_1

##to see the different sentiment lexicons
sentiments

get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")


###Using Bing Library for sentiment analysis
bing_word_counts <- try_1 %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_word_counts




##Make the visual for top positive and negative words with the Bing lexicon

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)



##Making a Comparative Word Cloud - again choose a small value
try_1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("indianred3","lightsteelblue3"),
                   max.words = 100)


##Digging into specific kinds of sentiments with NRC

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

try_1 %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

try_1 %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)


nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

try_1 %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)


nrc_trust <- get_sentiments("nrc") %>% 
  filter(sentiment == "trust")

trust1 <- try_1 %>%
  inner_join(nrc_trust) %>%
  count(word, sort = TRUE)

trust1

nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")

anticipation1 <- try_1 %>%
  inner_join(nrc_anticipation) %>%
  count(word, sort = TRUE)

anticipation1


##Deeper Sentiment Analysis
##Taken from https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/


# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(try_1, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)


##Bing score
bing_vector <- get_sentiment(try_1, method="bing")
head(bing_vector)
summary(bing_vector)
#affin score
afinn_vector <- get_sentiment(try_1, method="afinn")
head(afinn_vector)
summary(afinn_vector)


#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)


# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d <- get_nrc_sentiment(qstems)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)


##Bar graph for overall counts of each kind of emotion
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:5674]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, 
          ylab="count")+ggtitle("Survey sentiments")


##Bar plots to show what percentage of the meaningful words is each emotion
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.5, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)




####Begin LDA Code ##


###LdaTuning to find best number of topics
##Taken from https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

result1 <- FindTopicsNumber(
  stems_dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)


##print table of values for a more precise analysis
result1


##Code to make plot if the table is a bit hard to read
FindTopicsNumber_plot(result1)







##Basic code just to specify the model, change k for the number of topics, 
##once you know from ldatuning
stems_lda <- LDA(stems_dtm, k = 9, control = list(seed = 1234))
stems_lda


##Gives the probability of each word/number appearing in each topic
stems_topics <- tidy(stems_lda, matrix = "beta")
stems_topics




##Makes the most common 10 terms and gives a plot - 
stems_top_terms <- stems_topics %>% 
  group_by(topic) %>%
  top_n(10, beta) %>% 
  ungroup () %>%
  arrange(topic, -beta)

stems_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + 
  coord_flip()



##Code for finding the greatest differences between two similar-seeming topics
##This takes awhile to run, but usually works

beta_spread <- stems_topics %>% 
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))

beta_spread


##Code for Document-Topic Probabilities

stems_docfreq <- tidy(stems_lda, matrix = "gamma")
stems_docfreq

stems_topics2 <- stems_docfreq %>%
  arrange(desc(gamma))

View(stems_topics2)




#Boxplot by individual document
g <- ggplot(stems_topics2, aes(document, gamma))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="Gamma Plotted Versus Document",
       x="Document",
       y="Gamma")

#Boxplot by topic
g <- ggplot(stems_topics2, aes(topic, gamma), group = topic)
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="Gamma Plotted Versus Topic",
       x="Topic",
       y="Gamma")



##dot plot per topic

ggplot(data = stems_topics2)+
  geom_point(mapping = aes(x = document, y = gamma)) +
  facet_wrap(~topic, nrow = 2)


ggplot(data = stems_topics2) + 
  geom_bar(
    mapping = aes (x = topic, fill = document),
    position = "fill"
  )






##LDAvis code

##a visualizer for examining the clusters

library(LDAvis)
help(createJSON, package = "LDAvis")

vignette("details", package = "LDAvis")

##http://datacm.blogspot.com/2017/03/lda-visualization-with-r-topicmodels.html 
#' Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#' for use with LDAvis
#'
#' @param fitted Output from a topicmodels \code{LDA} model.
#' @param doc_term The document term matrix used in the \code{LDA}
#' model. This should have been created with the tm package's
#' \code{DocumentTermMatrix} function.
#'
#' @seealso \link{LDAvis}.
#' @export

topicmodels_json_ldavis <- function(stems_lda, stems_dtm){
  require(LDAvis)
  require(slam)
  
  # Find required quantities
  phi <- as.matrix(topicmodels::posterior(stems_lda)$terms)
  theta <- as.matrix(topicmodels::posterior(stems_lda)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(stems_dtm)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = as.vector(table(stems_dtm$i)),
                                 term.frequency = term_freq)
  
  return(json_lda)
}


json_res <- topicmodels_json_ldavis(stems_lda, stems_dtm)

serVis(json_res)




