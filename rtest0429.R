library(multilinguer)
library(textclean)
library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(tidytext)
library(KoNLP)
library(widyr)
library(tidyr)
library(tidygraph)
library(ggraph)
library(showtext)
comment <- read_csv("movie.csv")
comment


comment <- comment %>% unnest_tokens(input = review, output = review, token = "words") # ´Ü¾îº°·Î ÅäÅ«È­ 
comment



comment_space <- comment %>% count(review, sort = T) # ºóµµ ³ôÀº ´Ü¾î ÃßÃâ
comment_space

comment_space = comment_space %>% filter(str_count(review)>1) # ÇÑ±ÛÀÚÂ¥¸® ´Ù¾ø¾Ö°í ºóµµ º¸¿©ÁÜ
comment_space


top20 <- comment_space %>% head(20)
top20


ggplot(top20, aes(x = reorder(review, n), y = n)) + geom_col() + coord_flip() +
  geom_text(aes(label = n), hjust = -0.3 ) + labs(title = "¿µÈ­ ¸®ºä ºóµµ", x =NULL, y = NULL) +
  theme(title = element_text(size = 20))
#¹æÇâ Á¶ÀýÇÏ°í ±×·¡ÇÁ ÃßÃâ °Å¸®Á¶ÀýÇÏ°í xÃà°ú yÃà ¾ø¾Ú ¸¶Áö¸·À¸·Î Á¦¸ñ±Û¾¾Å©±â



comment10 <- read_csv("movie.csv") #³ÊÀÇÀÌ¸§Àº¸®ºä
comment10


comment1 <- read_csv("movie2.csv") # °°Àº °¨µ¶¿¡ ³»¿ëÀÌ ÀÌ¾îÁö´Â ³¯¾¾ÀÇ¾ÆÀÌ
comment1


weather <- comment1 %>% mutate(movie = "weather") # ³¯¾¾ÀÇ¾ÆÀÌ id ¸¦ ºÙ¿©ÁÜ
weather


name <- comment10 %>% mutate(movie = "name") # ³ÊÀÇÀÌ¸§Àº id ¸¦ ºÙ¿©ÁÜ
name


bind_speeches <- bind_rows(name, weather) %>% select(movie, review) #bind_rowÇÔ¼ö ½á¼­ µÎ°³ ÇÕÃÄÁÖ°í select ½á¼­ À§Ä¡ ¹Ù²ãÁÜ
bind_speeches

comment_space <- bind_speeches %>% unnest_tokens(input = review, output = review, token = "words") 
comment_space # ¶Ù¾î¾²±â ±âÁØÀ¸·Î ÅäÅ«È­


comment_space = comment_space %>% filter(str_count(review)>1) # ÇÑ±ÛÀÚÂ¥¸® ´Ù¾ø¾Ö°í ºóµµ º¸¿©ÁÜ
comment_space



freq <- comment_space %>% count(movie, review) %>% filter(str_count(review)>1) # ÇÑ¹®Àå•û°í ÃßÃâ
freq <- freq %>% group_by(movie) %>% slice_max(n, n=10) # ºóµµ ³ôÀº ´Ü¾î ÃßÃâ
freq


top10 <- freq %>% group_by(movie) %>% slice_max(n, n=10, with_ties=F) # ºóµµ µ¿Àû Á¦¿ÜÈÄ ÃßÃâ 
top10

# ÅÚ¸¶ 3¹ø º¸¸é µÊ

df_wide <- top10 %>% pivot_wider(names_from = movie, values_from = n) # 
df_wide

df_wide <- top10 %>% pivot_wider(names_from = movie, values_from = n, values_fill = list(n=0))
df_wide

freq_wide <- freq %>% pivot_wider(names_from = movie, values_from = n, values_fill = list(n=0))
freq_wide

freq_wide <- freq_wide %>% mutate(ratio_name = ((name)/(sum(name))), ratio_weather = ((weather)/(sum(weather))))
freq_wide

freq_wide <- freq_wide %>% mutate(ratio_name = ((name)/(sum(name))), ratio_weather = ((weather+1)/(sum(weather+1))))
freq_wide

freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_name/ratio_weather)
freq_wide

freq_wide %>% arrange(-odds_ratio)
freq_wide %>% arrange(odds_ratio)
freq_wide %>% arrange(abs(1-odds_ratio))

#==============================¿ÀÁîºñ

#-----------------------------------------4 ±îÁö
comment2 <- read_csv("movie2.csv")
comment2


comment2 <- comment2 %>% mutate(id =row_number(), review = str_squish(replace_html(review))) #°íÀ¯¹øÈ£ ¸¸µé°í html Æ¯¼ö¹®ÀÚ Á¦°Å
glimpse(comment2)

word_comment <- comment2 %>% unnest_tokens(input =review, output = word, token = "words", drop = F) # ´Ü¾î±âÁØ ÅäÅ«È­
word_comment %>% select(word, review) #´Ü¾î±âÁØÀ¸·Î ÅäÅ«È­ µÈ°Í°ú review³»¿ë Ãâ·Â

dic <- read_csv("knu_sentiment_lexicon.csv") 
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity)) # °¨Á¤Á¡¼ö ºÎ¿©
word_comment %>% select(word, polarity)


word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
# °¨Á¤ ºÐ·ùÇÏ±â 
new_word_count <- word_comment %>% count(sentiment)
new_word_count

top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
# neg, pos µî ±×Áß »óÀ§ 10°³¸¦ º¸¿©ÁÖ¸ç °¹¼ö¸¦ »õ¾îÁÜ 
top10_sentiment


ggplot(top10_sentiment, aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_col()+coord_flip() + geom_text(aes(label =n), hjust = -0.5) + facet_wrap(~sentiment, scales ="free")+scale_y_continuous(expand = expansion(mult = c(0.03,0.11))) + labs(x = NULL)


score_comment <- word_comment %>% group_by(id, review) %>% summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select(score, review)



score_comment %>% select(score, review) %>% arrange(-score) #±àÁ¤
score_comment %>% select(score, review) %>% arrange(score) #ºÎÁ¤

score_comment %>% count(score) %>% print(n =Inf) # °¨Á¤ Á¡¼ö ºóµµ ±¸ÇÏ±â±â


score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
score_comment

freq_score <-score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
freq_score

ggplot(freq_score, aes(x = sentiment, y = n, fill = sentiment)) + geom_col()+ geom_text(aes(label = n), vjust = -0.3) + scale_x_discrete(limits = c("pos","neu","neg"))

# -----------------------------»õ °¨Á¤ ³Ö¾î¼­ ºñ±³ ÇØº¸´Â°Í 
score_comment %>% filter(str_detect(review, "¹ÌÄ£³ð")) %>% select(review)
score_comment %>% filter(str_detect(review, "±×´Ú")) %>% select(review)

new_word <- tibble(word = c("¹ÌÄ£³ð","±×´Ú"),polarity = c(-2,-2)) #¹ÌÄ£³ð°ú ¼³Ä¡´ÂÀÌ¶ó´Â ´Ü¾î´Â °¨Á¤»çÀü¿¡ ¾ø±â¶§¹®¿¡
# »õ·Î¿î ´Ü¾î¸¦ Ãß°¡ÇØÁÜ

new_word_dic <- bind_rows(dic, new_word) # Ãß°¡ÇØÁÖ°í 

tail(new_word_dic) # º¸¿©ÁÜ

word_comment <- comment2 %>% unnest_tokens(input =review, output = word, token = "words", drop = F) # ´Ü¾î±âÁØ ÅäÅ«È­
word_comment %>% select(word, review) #´Ü¾î±âÁØÀ¸·Î ÅäÅ«È­ µÈ°Í°ú review³»¿ë Ãâ·Â


new_word_comment <- word_comment %>% left_join(new_word_dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity)) # »õ °¨Á¤Á¡¼ö ºÎ¿©
new_word_comment %>% select(word, polarity)

new_word_comment <- new_word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
# °¨Á¤ ºÐ·ùÇÏ±â 
new_word_count <- new_word_comment %>% count(sentiment)
new_word_count


top10_s <- new_word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
# neg, pos µî ±×Áß »óÀ§ 10°³¸¦ º¸¿©ÁÖ¸ç °¹¼ö¸¦ »õ¾îÁÜ 
top10_s


new_score_comment <- word_comment %>% group_by(id, review) %>% summarise(score = sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, review)



new_score_comment %>% select(score, review) %>% arrange(-score) #±àÁ¤
new_score_comment %>% select(score, review) %>% arrange(score) #ºÎÁ¤

new_score_comment %>% count(score) %>% print(n =Inf) # °¨Á¤ Á¡¼ö ºóµµ ±¸ÇÏ±â±â


new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 2, "pos", ifelse(score <= -2, "neg", "neu")))
new_score_comment


new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100) # ¼öÁ¤µÈ »çÀü



#--------------------------------------------------------5¹ø

comment2 <- read_csv("movie2.csv")
comment2

news_comment <- comment2 %>% select(review) %>% mutate(review = str_replace_all(review, "[^°¡-ÆR]", " ") , review = str_squish(review), id = row_number())
comment_pos <- news_comment %>% unnest_tokens(input = review, output = word, token = SimplePos22, drop = F)
comment_pos %>% select(word, review)
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
comment_pos %>% select(word, review)
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word, "/.*$"))
noun %>% select(word, review)
noun
# ¾î¶² ¸í»ç°¡ ¸¹ÀÌ ¾²¿´´ÂÁö È®ÀÎ
noun %>% count(word, sort = T)
pvpa <- comment_pos %>% filter(str_detect(word, "/pa|/pv")) %>% mutate(word = str_replace(word, "/.*$","´Ù"))
pvpa %>% select(word, review)
pvpa %>% count(word, sort = T)

comment <- bind_rows(noun, pvpa)%>% filter(str_count(word)>=2)%>% arrange(id)
comment %>% select(word, review)

pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
pair

pair %>% filter(item1 == "½ºÅä¸®")
pair %>% filter(item1 == "¿µ»ó¹Ì")

graph_comment <- pair %>% filter(n>=2) %>% as_tbl_graph()
graph_comment


ggraph(graph_comment) + geom_node_point() + geom_edge_link() + geom_node_text(aes(label = name))



font_add_google(name = "Nanum Gothic", family = "ng")
showtext_auto()

set.seed(1234)
ggraph(graph_comment, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + geom_node_point(color = "lightcoral", size = 5) + geom_node_text(aes(label = name), repel = T, size = 5, family = "ng") + theme_graph()


word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50", alpha = 0.5) +
    geom_node_point(color = "lightcoral", size = 5) +
    geom_node_text(aes(label = name), repel = T, size = 5, family = "ng") +
    theme_graph()
}

set.seed(1234)
word_network(graph_comment)


graph_comment <- pair %>% filter(n>=2) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))
graph_comment

set.seed(1234)
ggraph(graph_comment, layout = "fr") +
  geom_edge_link(color = "gray50", alpha = 0.5) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  
  scale_size(range = c(5, 15)) +
  
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "ng") +
  
  
  theme_graph()
#================================================Ã³À½ ³×Æ®¿öÅ© ¹ÙÀÌ±×·¥




word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 3) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors

word_cors %>% filter(item1 == "»ç¶û")
word_cors %>% filter(item1 == "¸ð½À")


word_list <- c("»ç¶û","¸ð½À","°¨µ¿","¸ôÀÔ","½ºÅä¸®","¿µ»ó¹Ì")

top8_cors <- word_cors %>% 
  filter(item1 %in% word_list) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8)

top8_cors$item1 <- factor(top8_cors$item1, levels = word_list)

ggplot(top8_cors, aes(x = reorder_within(item2, correlation, item1),
                      y = correlation,
                      fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL)


set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.3) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))


set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,
                     edge_width = correlation),
                 show.legend = F) +
  scale_edge_width(range = c(0,2)) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5,10)) +
  geom_node_text(aes(label = name), repel = T, size = 5) +
  theme_graph()



#=================================¿©±â±îÁö ÆÄÀÌ°è¼ö




