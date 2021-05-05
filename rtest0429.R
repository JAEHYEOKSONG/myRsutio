library(multilinguer)
library(textclean)
library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(KoNLP)
library(widyr)
library(tidyr)
library(tidygraph)
library(ggraph)
library(showtext)
comment <- read_csv("movie.csv") # 리뷰 파일 불러옴
comment

# 공백을 제거해주고 tibble구조로 변경해준후 단어별로 토큰화를 시켜줍니다.
comment <- comment %>% str_replace_all("[^가-힣]", " ") %>% str_squish()%>% as_tibble() 
comment <- comment %>% unnest_tokens(input = review, output = review, token = "words") 
comment


# 빈도 높은 단어 추출하여 빈도가 높은 단어를 확인합니다.
comment_space <- comment %>% count(review, sort = T) 
comment_space



# 한글자인 단어를 없애고 빈도를 보여줍니다.
comment_space = comment_space %>% filter(str_count(review)>1) 
comment_space

# 빈도가 높은 단어들을 20개 추출한것을 넣어 그래프를 그리기위해 top20 에넣어줍니다.
top20 <- comment_space %>% head(20)
top20


ggplot(top20, aes(x = reorder(review, n), y = n, fill="review_word")) + geom_col() + coord_flip() +
  geom_text(aes(label = n), hjust = -0.3 ) + labs(title = "영화 리뷰 빈도", x =NULL, y = NULL, fill = NULL) +
  theme(title = element_text(size = 20))
# top 20 단어들을 그래프로 나타내고 방향 조절하고 aes로 거리조절하고 x축과 y축 fill 보여주는것을 
# NULL값을 넣어 없애주며  텍스트 크기는 20으로 하여 보여줍니다.


comment10 <- read_csv("movie.csv") #너의이름은리뷰
comment10


comment1 <- read_csv("movie2.csv") # 같은 감독에 내용이 이어지는 날씨의아이
comment1


weather <- comment1 %>% mutate(movie = "weather") # 날씨의아이 id 를 붙여줌
weather


name <- comment10 %>% mutate(movie = "name") # 너의이름은 id 를 붙여줌
name


bind_speeches <- bind_rows(name, weather) %>% select(movie, review) #bind_row함수 써서 두개 합쳐주고 select 써서 위치 바꿔줌
bind_speeches

comment_space <- bind_speeches %>% unnest_tokens(input = review, output = review, token = "words") 
comment_space # 뛰어쓰기 기준으로 토큰화


comment_space = comment_space %>% filter(str_count(review)>1) # 한글자짜리 다없애고 빈도 보여줌
comment_space



freq <- comment_space %>% count(movie, review) %>% filter(str_count(review)>1) # 한문장馨 추출
freq <- freq %>% group_by(movie) %>% slice_max(n, n=10) # 빈도 높은 단어 추출
freq


top10 <- freq %>% group_by(movie) %>% slice_max(n, n=10, with_ties=F) # 빈도 동적 제외후 추출 
top10


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

#==============================오즈비

#-----------------------------------------4 까지
comment2 <- read_csv("movie2.csv")
comment2


comment2 <- comment2 %>% mutate(id =row_number(), review = str_squish(replace_html(review))) #고유번호 만들고 html 특수문자 제거
glimpse(comment2)

word_comment <- comment2 %>% unnest_tokens(input =review, output = word, token = "words", drop = F) # 단어기준 토큰화
word_comment %>% select(word, review) #단어기준으로 토큰화 된것과 review내용 출력

dic <- read_csv("knu_sentiment_lexicon.csv") 
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity)) # 감정점수 부여
word_comment %>% select(word, polarity)


word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
# 감정 분류하기 
new_word_count <- word_comment %>% count(sentiment)
new_word_count

top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
# neg, pos 등 그중 상위 10개를 보여주며 갯수를 새어줌 
top10_sentiment


ggplot(top10_sentiment, aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_col()+coord_flip() + geom_text(aes(label =n), hjust = -0.5) + facet_wrap(~sentiment, scales ="free")+scale_y_continuous(expand = expansion(mult = c(0.03,0.11))) + labs(x = NULL)


score_comment <- word_comment %>% group_by(id, review) %>% summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select(score, review)



score_comment %>% select(score, review) %>% arrange(-score) #긍정
score_comment %>% select(score, review) %>% arrange(score) #부정

score_comment %>% count(score) %>% print(n =Inf) # 감정 점수 빈도 구하기기


score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
score_comment

freq_score <-score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
freq_score

ggplot(freq_score, aes(x = sentiment, y = n, fill = sentiment)) + geom_col()+ geom_text(aes(label = n), vjust = -0.3) + scale_x_discrete(limits = c("pos","neu","neg"))

# -----------------------------새 감정 넣어서 비교 해보는것 
score_comment %>% filter(str_detect(review, "미친놈")) %>% select(review)
score_comment %>% filter(str_detect(review, "그닥")) %>% select(review)

new_word <- tibble(word = c("미친놈","그닥"),polarity = c(-2,-2)) #미친놈과 설치는이라는 단어는 감정사전에 없기때문에
# 새로운 단어를 추가해줌

new_word_dic <- bind_rows(dic, new_word) # 추가해주고 

tail(new_word_dic) # 보여줌

word_comment <- comment2 %>% unnest_tokens(input =review, output = word, token = "words", drop = F) # 단어기준 토큰화
word_comment %>% select(word, review) #단어기준으로 토큰화 된것과 review내용 출력


new_word_comment <- word_comment %>% left_join(new_word_dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity)) # 새 감정점수 부여
new_word_comment %>% select(word, polarity)

new_word_comment <- new_word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
# 감정 분류하기 
new_word_count <- new_word_comment %>% count(sentiment)
new_word_count


top10_s <- new_word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
# neg, pos 등 그중 상위 10개를 보여주며 갯수를 새어줌 
top10_s


new_score_comment <- word_comment %>% group_by(id, review) %>% summarise(score = sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, review)



new_score_comment %>% select(score, review) %>% arrange(-score) #긍정
new_score_comment %>% select(score, review) %>% arrange(score) #부정

new_score_comment %>% count(score) %>% print(n =Inf) # 감정 점수 빈도 구하기기


new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 2, "pos", ifelse(score <= -2, "neg", "neu")))
new_score_comment


new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100) # 수정된 사전



#--------------------------------------------------------5번

comment2 <- read_csv("movie2.csv")
comment2

news_comment <- comment2 %>% select(review) %>% mutate(review = str_replace_all(review, "[^가-R]", " ") , review = str_squish(review), id = row_number())
comment_pos <- news_comment %>% unnest_tokens(input = review, output = word, token = SimplePos22, drop = F)
comment_pos %>% select(word, review)
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
comment_pos %>% select(word, review)
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word, "/.*$"))
noun %>% select(word, review)
noun
# 어떤 명사가 많이 쓰였는지 확인
noun %>% count(word, sort = T)
pvpa <- comment_pos %>% filter(str_detect(word, "/pa|/pv")) %>% mutate(word = str_replace(word, "/.*$","다"))
pvpa %>% select(word, review)
pvpa %>% count(word, sort = T)

comment <- bind_rows(noun, pvpa)%>% filter(str_count(word)>=2)%>% arrange(id)
comment %>% select(word, review)

pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
pair

pair %>% filter(item1 == "스토리")
pair %>% filter(item1 == "영상미")

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
#================================================처음 네트워크 바이그램




word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 3) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors

word_cors %>% filter(item1 == "사랑")
word_cors %>% filter(item1 == "모습")


word_list <- c("사랑","모습","감동","몰입","스토리","영상미")

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



#=================================여기까지 파이계수




