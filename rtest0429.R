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

comment_name <- read_csv("movie.csv") #너의이름은리뷰
comment_name


comment_weather <- read_csv("movie2.csv") # 같은 감독에 내용이 이어지는 날씨의아이
comment_weather


weather <- comment_weather %>% mutate(movie = "weather") # 날씨의아이 id 를 붙여줍니다.
weather


name <- comment_name %>% mutate(movie = "name") # 너의이름은 id 를 붙여줍니다.
name


bind_speeches <- bind_rows(name, weather) %>% select(movie, review) #bind_row함수 써서 두개 합쳐주고 select 써서 순서 바꿔줍니다.
bind_speeches
# 단어기준으로 토큰화합니다.
comment_space <- bind_speeches %>% unnest_tokens(input = review, output = review, token = "words") 
comment_space 

freq <- comment_space %>% count(movie, review) %>% filter(str_count(review)>1) # 한글자 단어 뺴고 추출해줍니다.
freq <- freq %>% group_by(movie) %>% slice_max(n, n=10) # group_by로 영화 두개를 묶어주고 빈도 높은 단어 추출합니다.
freq


top10 <- freq %>% group_by(movie) %>% slice_max(n, n=10, with_ties=F) # 빈도 동적 제외후 추출 
top10

# pivot_wider을 이용하여 데이터를 재구조 하여 두개의 데이터에 있는 단어와 없는단어를 구분하여 없으면 NA값이 출력되게 합니다.
df_wide <- top10 %>% pivot_wider(names_from = movie, values_from = n) 
df_wide
# 그후 NA값은 계산이 불가함으로 전부 0으로 바꿔줍니다.
df_wide <- top10 %>% pivot_wider(names_from = movie, values_from = n, values_fill = list(n=0))
df_wide
# 리뷰 빈도를 전부 wide form로 바꾸어줍니다.
freq_wide <- freq %>% pivot_wider(names_from = movie, values_from = n, values_fill = list(n=0))
freq_wide
# 각 단어가 리뷰에 차지하는 오즈비를 구해줍니다. (각단어의 빈도/ 모든 단어의 빈도)
freq_wide <- freq_wide %>% mutate(ratio_name = ((name)/(sum(name))), ratio_weather = ((weather)/(sum(weather))))
freq_wide
# 빈도 0이나오게되면 0을 나누게 되는 경우가 발생함으로 1을 더해줍니다.
freq_wide <- freq_wide %>% mutate(ratio_name = ((name)/(sum(name))), ratio_weather = ((weather+1)/(sum(weather+1))))
freq_wide
# 그후 텍스트 비중을 다른 텍스트의 단어 비중으로 나누어 줍니다.
freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_name/ratio_weather)
freq_wide
# 오즈비 구하는 식대로 대입하여 오즈비를 구합니다.
freq_wide <- freq_wide %>% mutate(odds_ratio = ((name+1)/sum(weather+1))/((weather+1)/(sum(weather+1))))
freq_wide


#==============================오즈비

comment2 <- read_csv("movie2.csv")
comment2


comment2 <- comment2 %>% mutate(id =row_number(), review = str_squish(replace_html(review))) #고유번호 만들고 html 특수문자 제거

word_comment <- comment2 %>% unnest_tokens(input =review, output = word, token = "words", drop = F) # 단어기준 토큰화
word_comment %>% select(word, review) #단어기준으로 토큰화 된것과 review내용 출력

dic <- read_csv("knu_sentiment_lexicon.csv") # 감정사전을 불러온다.
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity)) # 감정점수 부여
word_comment %>% select(word, polarity)

# 감정 분류하고 감정 갯수 확인
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
new_word_count <- word_comment %>% count(sentiment)
new_word_count


# neg, pos 등 그중 상위 10개를 보여주며 갯수를 새어줌
top10_sentiment <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
top10_sentiment

# 자주 사용된 감정단어를 시각적으로 그래프로 나타내어줌
ggplot(top10_sentiment, aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_col()+coord_flip() + geom_text(aes(label =n), hjust = -0.5) + facet_wrap(~sentiment, scales ="free")+scale_y_continuous(expand = expansion(mult = c(0.03,0.11))) + labs(x = NULL)

# 댓글별 감정 점수를 계산해줍니다. id 값 번호 매긴후 group_by로 묶고 summarise로 여러 요약한것을 보여주고 
# ungroup함수로 묶인 데이터를 그룹해체후  score review위치 변경후 보여줍니다. 
score_comment <- word_comment %>% group_by(id, review) %>% summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select(score, review)

score_comment %>% select(score, review) %>% arrange(-score) #긍정댓글 보여주고
score_comment %>% select(score, review) %>% arrange(score) #부정댓글 보여줍니다.

score_comment %>% count(score) %>% print(n =Inf) # 감정 점수 빈도 구해줍니다.

# 감정을 분류한뒤 긍정적인 댓글과 부정적인 댓글이 많은지 감정 경향을 살펴봅니다.
score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu"))) 
freq_score <-score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
freq_score
# 그후 감정 경향을 시각적으로 ggplot 을 통하여 그래프로 보여줍니다.
ggplot(freq_score, aes(x = sentiment, y = n, fill = sentiment)) + geom_col()+ geom_text(aes(label = n), vjust = -0.3) + scale_x_discrete(limits = c("pos","neu","neg"))




# -----------------------------새 감정 넣어서 비교 해보는것 수정 감정사전으로 비교
# 댓글에 보면 비속어나 별로 보여주고 싶지 않은 감정의 댓글이있습니다.
score_comment %>% filter(str_detect(review, "미친놈")) %>% select(review)
score_comment %>% filter(str_detect(review, "그닥")) %>% select(review)

new_word <- tibble(word = c("미친놈","그닥"),polarity = c(-2,-2)) #미친놈과 그닥 이라는 단어는 감정사전에 없기때문에 새로운 단어를 추가해줍니다.

new_word_dic <- bind_rows(dic, new_word) # 새로운 감정사전을 만들어 추가해주고 

tail(new_word_dic) # 잘 적용되었는지 보여줍니다.

word_comment <- comment2 %>% unnest_tokens(input =review, output = word, token = "words", drop = F) # 단어기준 토큰화
word_comment %>% select(word, review) #단어기준으로 토큰화 된것과 review내용 출력

# 새 감정점수 부여
new_word_comment <- word_comment %>% left_join(new_word_dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity),0,polarity))
new_word_comment %>% select(word, polarity)

new_word_comment <- new_word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
# 감정 분류하기 
new_word_count <- new_word_comment %>% count(sentiment)
new_word_count


top10_s <- new_word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n=10)
# neg, pos 등 그중 상위 10개를 보여주며 갯수를 새어줌 
top10_s


# 댓글별 감정 점수를 계산해줍니다. id 값 번호 매긴후 group_by로 묶고 summarise로 여러 요약한것을 보여주고 
# ungroup함수로 묶인 데이터를 그룹해체후  score review위치 변경후 보여줍니다. 
new_score_comment <- new_word_comment %>% group_by(id, review) %>% summarise(score = sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, review)

new_score_comment %>% select(score, review) %>% arrange(-score) #긍정댓글 보여주고
new_score_comment %>% select(score, review) %>% arrange(score) #부정댓글 보여줍니다.

new_score_comment %>% count(score) %>% print(n =Inf) # 새 감정 점수 빈도 구하기기


new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 2, "pos", ifelse(score <= -2, "neg", "neu")))
new_score_comment


new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100) # 수정된 사전



#--------------------------------------------------------5번
comment2 <- read_csv("movie2.csv")
comment2
# 띄어쓰기 제거하고 
movie_comment <- comment2 %>% select(review) %>% mutate(review = str_replace_all(review, "[^가-힣]", " ") , review = str_squish(review), id = row_number())

comment_pos <- movie_comment %>% unnest_tokens(input = review, output = word, token = SimplePos22, drop = F)
comment_pos %>% select(word, review)
# separate_rows 함수를 사용하여 sep = [+]가 등장할때마다 행을 나눠줌.
comment_pos <- comment_pos %>% separate_rows(word, sep = "[+]")
# word, review로 위치 바꿔서 보여줌
comment_pos %>% select(word, review)
# filter함수 str_remove 함수를 사용하여 /n와 특수문자들을 제거해줌 그후 다시 mutate함수를 사용하여 word 변수생성하여 넣어줌
noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word = str_remove(word, "/.*$"))
# word, review로 위치 바꿔서 보여줌
noun %>% select(word, review)
noun
# 어떤 명사가 많이 쓰였는지 확인
noun %>% count(word, sort = T)
#동사, 형용사추출,동사 /pv, 형용사 /pa 붙어있는단어추출
pvpa <- comment_pos %>% filter(str_detect(word, "/pa|/pv")) %>% mutate(word = str_replace(word, "/.*$","다"))
# 어떤 동사가 많이 쓰였는지 확인 
pvpa %>% select(word, review)
# 동사 쓰인 갯수 확인 
pvpa %>% count(word, sort = T)
# 명사, 동사 두글자 이상의 단어만 남기고 출력해줌
comment <- bind_rows(noun, pvpa)%>% filter(str_count(word)>=2)%>% arrange(id)
comment %>% select(word, review)
#pairwise_count함수를 사용하여 그룹 단위 내에서 단어가 동시에 출현한 횟수를 세어줌
pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
pair
# 많이 나온 특정 단어별 조회하기 
pair %>% filter(item1 == "스토리")
pair %>% filter(item1 == "영상미")
# n이 1보다 큰경우의 특정 단어들을 골라서  as_tbl_graph() 함수를 사용하여 그래프형식으로 바꿔줌
graph_comment <- pair %>% filter(n>=2) %>% as_tbl_graph()
graph_comment

# geom_node_point()(노드),geom_edge_link()(엣지)를 통해 네트워크 그래프를 만들어봄
ggraph(graph_comment) + geom_node_point() + geom_edge_link() + geom_node_text(aes(label = name))


# 폰트설정하고 
font_add_google(name = "Nanum Gothic", family = "ng")
showtext_auto()
# 시드값을 1234로 해줍니다.
set.seed(1234)
#위에 동시출현 명사와 동사 네트워크 그래프를 좀더 확실히 보이게 엣지와 
# 노드색깔 크기 텍스트 위치 설정을 통해 다듬어서 보여줍니다. 
ggraph(graph_comment, layout = "fr") + geom_edge_link(color = "gray50", alpha = 0.5) + geom_node_point(color = "lightcoral", size = 5) + geom_node_text(aes(label = name), repel = T, size = 5, family = "ng") + theme_graph()
# 노드 엣지 색깔과 크기 , 사이즈크기를 좀더 다듬어서 좀더 강조된 내용이 높은것을 시각적으로 잘 보이게 
# 네트워크 그래프를 나타내줍니다.
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

# 다음은 파이계수를 통해 알아보기위해 pairwise_cor함수를 사용하여 두 단어의 파이계수를 구해줍니다.
word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 3) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors
# 상대적으로 어떤단어가 관련성이 크고 자주 사용되었는지 확인함
word_cors %>% filter(item1 == "사랑")
word_cors %>% filter(item1 == "모습")

# 관심단어 목록추가 하고 
word_list <- c("사랑","모습","감동","몰입","스토리","영상미")
# 파이계수 추출을 5개씩 함 
top5_cors <- word_cors %>% 
  filter(item1 %in% word_list) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 5)
# 그후 factor 함수를 사용하여 범주형 자료를 표현하여 
top5_cors$item1 <- factor(top5_cors$item1, levels = word_list)
# 변수의 항목별로 그래프를 나누어서 보여줍니다.
ggplot(top5_cors, aes(x = reorder_within(item2, correlation, item1),
                      y = correlation,
                      fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL)


set.seed(1234)
# 파이 계수로 네트워크 그래프를 보여줍니다.
# 명암 두께 엣지 단어 사용 된것을 시각적으로 보여주는 scale_size 함수 사용하여 크기 조절하고 
# 글자 크기등 조절후 네트워크 형식으로 나타내줍니다.
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
