library(dplyr)
library(tidytext)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(igraph)
library(ggraph)
library(tm) # for text input (not working with tidytext )
library(widyr)

# http://tidytextmining.com/ngrams.html

# data input is not well implemented sofar

#docs <- VCorpus(DirSource("~/uni/askDarwin/dat/")) # all
#docs <- VCorpus(DirSource("~/uni/askDarwin/dat/", pattern="origin"))   

txt <- read_lines("~/uni/askDarwin/dat/origin1.txt")
tdoc <- data_frame(book="origin", text = txt)

tidy_text <- tdoc %>%
  unnest_tokens(word, text)

data("stop_words")
tidy_text <- tidy_text %>%
  anti_join(stop_words)

tidy_text %>%
  count(word, sort = TRUE)


#
# 1) ngrams
#
bigrams <- tdoc %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

#Analyzing bigrams
bigrams_filtered %>%
  filter(word2 == "species") %>%
  count(word1, sort = TRUE)



#
# graph
#
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#
# 2) Correlation
# correlation among words (how often they appear together relative to how often they appear separately)

section_words <- tdoc %>%
  mutate(section = row_number() %/% 80) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

word_cors <- section_words %>%
  group_by(word) %>%
  #filter(n() >= 20) %>%
  filter(n() >= 200) %>% # magic number
  pairwise_cor(word, section, sort = TRUE)
word_cors

word_cors %>%
  filter(item1 == "species")

word_cors %>%
  filter(correlation > .2) %>% # magic number
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = FALSE) +
  theme_void()
