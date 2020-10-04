setwd('C:/Users/jiho0/OneDrive/바탕 화면/주제분석/data')
library(data.table)
library(dplyr)
library(tidyr)
#data를 읽어와보자.
#magazine <- fread('magazines.csv', encoding = 'UTF-8')
metadata <- fread('metadata.csv', encoding = 'UTF-8')
read_data <- fread('read.csv', encoding = 'UTF-8')
user <- fread('users.csv', encoding = 'UTF-8')

#metadata와 magazine을 merge시켜주자.
#magazine <- magazine %>% select(-num)
#metadata <- left_join(metadata, magazine, by = c('magazine_id' = 'magazine_id'))
#metadata <- metadata %>% select(article_number, article_keyword, magazine_tag_list, title, sub_title, author_id,
#                                article_id, magazine_id, display_url, reg_ts)
#write.csv(metadata, 'metadata.csv', row.names = F)

#사람이 읽은 것에 대한 것을 뽑아보자.
ad_read_data <- read_data %>% select(user_id, article_id)
range <- c(seq(1, 3507097, 300000), nrow(ad_read_data))
for(i in 1:(length(range)-1)) { 
  assign(paste0('data',i),(ad_read_data[c(range[i]:range[i+1]-1),] %>% separate_rows(article_id, sep=" ")))
  print(i)
}
ad_read_data <- rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)
rm(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)

#메타 데이터와 머지해서 article의 키워드를 붙여주자.
merged_read_data <- left_join(ad_read_data, metadata, by = c('article_id'='article_number') )
merged_read_data <- merged_read_data %>% select(user_id, article_keyword, magazine_tag_list)

#이제 독자 한명이 읽은 글의 키워드를 모두 모아보자. - 특수문자 제거. 
library(tm)
merged_read_data$article_keyword  <-  tm::removePunctuation(merged_read_data$article_keyword)
merged_read_data$magazine_tag_list  <-  tm::removePunctuation(merged_read_data$magazine_tag_list)

#이제 독자 한명이 읽은 글의 키워드를 모두 모아보자. - group by
user_tag <- merged_read_data %>% 
  group_by(user_id) %>% 
  summarize(article_keyword = paste(article_keyword, collapse = " "), 
            magazine_keyword = paste(magazine_tag_list, collapse = " "))
write.csv(user_tag, 'user_tag.csv', row.names = F)

#44줄부터 돌려라.
setwd('C:/Users/jiho0/OneDrive/바탕 화면/주제분석/data')
library(dplyr)
library(data.table)
library(tidyr)
library(readr)
library(tm)
x <- fread('user_tag.csv', encoding = 'UTF-8')

#article과 magazine을 떼어주자.
user_article <- x %>% select(user_id, article_keyword)
user_magazine <- x %>% select(user_id, magazine_keyword)

#--------------------article에 대해서
#기본전처리
user_article <- user_article %>% na.omit()
user_article$article_keyword <- user_article$article_keyword %>% gsub(pattern = 'NA', replacement = '') 

#복제해주자.
user_article <- user_article %>% separate_rows(article_keyword, sep = ' ')
user_article <- user_article[!(user_article$article_keyword == ""),] #데이터가 빈 곳을 없애주자. 

#count를 해주자. 
user_article_cnt <- user_article %>% group_by(user_id, article_keyword) %>% summarize(count = n())
write_csv(user_article_cnt, 'user_count_article_전체.csv')
#user_article_cnt[(user_article_cnt$count >= 5),] %>% nrow 
write_csv(user_article_cnt[(user_article_cnt$count >= 5),], 'user_count_article_5번이상읽은글.csv')

#---------------------magazine에 대해서.
#기본전처리
user_magazine <- user_magazine %>% na.omit()
user_magazine$magazine_keyword <- user_magazine$magazine_keyword %>% gsub(pattern = 'NA', replacement = '') 

#복제해주자.
user_magazine <- user_magazine %>% separate_rows(magazine_keyword, sep = ' ')
user_magazine <- user_magazine[!(user_magazine$magazine_keyword == ""),] #데이터가 빈 곳을 없애주자. 

#count를 해주자. 
user_magazine_cnt <- user_magazine %>% group_by(user_id, magazine_keyword) %>% summarize(count = n())
write_csv(user_magazine_cnt, 'user_count_magazine_전체.csv')
#user_magazine_cnt[(user_magazine_cnt$count >= 10),] %>% nrow 
write_csv(user_magazine_cnt[(user_magazine_cnt$count >= 10),],
          'user_count_magazine_10번이상읽은.csv')

##################################################################################################
##################################################################################################
##----------------------------------유사어집합으로 키워드를 줄여주는 과정을 거쳐야 한다. 
##################################################################################################
##################################################################################################
rm(list = ls())
#유사어집합 - magazine
setwd('C:/Users/jiho0/OneDrive/바탕 화면/주제분석/data')
library(dplyr)
library(data.table)
library(tidyr)
library(readr)
library(tm)
library(stringr)
library(moonBook)
library(Hmisc)
magazine_dic <- fread('magazine_keyword_유사어집합.csv', encoding = 'UTF-8', header= T)
user_data_magazine <- fread('user_count_magazine_전체.csv', encoding = 'UTF-8')
colnames(magazine_dic) <- c('V1', 'keyword', 'dic')
magazine_dic$dic <- magazine_dic$dic %>% tm::removePunctuation()
magazine_dic <- magazine_dic %>% separate_rows(dic, sep = ' ')
magazine_dic <- magazine_dic %>% select('keyword', 'dic')
unique_magazine <-  magazine_dic[-which(duplicated(magazine_dic$dic)),] #중복 없이 유늬크하게 만들어준다.

#지금 dic에 NA too much. 
merged_data <- left_join(user_data_magazine, unique_magazine, by = c('magazine_keyword' = 'dic'))

#데이터가 준비되었다.
merged_data <- merged_data %>% na.omit() #NA인 행을 삭제해주고
magazine_large_keyword <- merged_data %>% select(-magazine_keyword) #큰 범주만 남겨준다.
str(magazine_large_keyword)
new_count <- magazine_large_keyword %>% group_by(user_id, keyword) %>% dplyr::summarize(new_sum = sum(count))

#읽은 횟수가 적은 글들은 어떻게 할까
new_count$new_sum %>% summary
new_count <- new_count %>% arrange(desc(new_sum))
boxplot(new_count$new_sum)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#1.000    1.000    2.000    4.657    4.000 2468.000 
#위와 같으므로 1번 읽은 글들은 모두 제거해줄까?

#한번 읽은 태그들은 제거해주자. 
new_count[(new_count$new_sum > 1),] %>% nrow #한번 이상 읽은 
new_count[(new_count$new_sum > 2),] %>% nrow
reduced_data <- new_count[(new_count$new_sum >= 5),] #다섯번 이상 읽은 유의미한 친구들을 뽑는다.
reduced_data$user_id %>% as.factor() %>% levels %>% length


#이걸 매트릭스로 만들어주자.
library(tidyr)
itemmat_magazine <- reduced_data %>% spread(keyword, new_sum)

#NA를 0으로 만들어주자.
itemmat_magazine[is.na(itemmat_magazine)] <- 0

write_csv(itemmat_magazine, 'item_mat_magazine.csv')

library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(anytime)
library(lubridate)
setwd('C:/Users/jiho0/OneDrive/바탕 화면/주제분석/data')

read_data <- fread('read.csv')
####________________[1]데이터 준비 및 전처리
#필요없는 열 삭제
read_data %>% colnames()
read_data <- read_data %>% select(dt, user_id, article_id)

#나눠서 separate를 진행해주자. 
range <- c(seq(1, 3507097, 300000), nrow(read_data))
for(i in 1:(length(range)-1)) { 
  assign(paste0('data',i),(read_data[c(range[i]:range[i+1]-1),] %>% separate_rows(article_id, sep=" ")))
  print(i)
}
read_data <- rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)
rm(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)


####________________[2]데이터 trim
#[2-1] article의 조회수를 봐보자. 
read_data_count <- read_data %>% group_by(article_id) %>% summarize(count = n())
read_data_count<- read_data_count[-1,]
read_data_nobrunch <- read_data_count[!(read_data_count$article_id %>% substr(1,8) == "@brunch_"),]
read_data_nobrunch$count %>% density() %>% plot
#######결과적으로 독자들이 읽은 글들을 모두 모았을때, 이 글들의 개수는 505841임.

#[2-2] 조회수 자체가 1인 글을 삭제한다.
read_data_nobrunch <- read_data_nobrunch %>% arrange(desc(count))
#read_data_nobrunch[read_data_nobrunch$count >=1000,] %>% nrow
read_data_count <- read_data_count[read_data_count$count != 1,] 

#[2-3] 조회수 자체가 1인 글을 삭제해주는 늑김
read_data_cut <- left_join(read_data, read_data_count, by = c('article_id' = 'article_id'))
read_data_cut <- read_data_cut %>% na.omit 

#[2-4] 한 유저가 읽은 글이 1인거면 모두 삭제한닷
read_data_cut <- read_data_cut %>% select(-count)
read_data_cut2 <- read_data_cut %>% group_by(user_id, article_id) %>% summarize(count = n())
read_data_cut2 <- read_data_cut2[read_data_cut2$count > 2, ]
######
read_data_cut <- left_join(read_data_cut, read_data_cut2, by = c('user_id'= 'user_id', 
                                                                 'article_id' = 'article_id'))
read_data_cut <- read_data_cut %>% na.omit
read_data_cut  <- read_data_cut %>% select(-count)

#[3] 이제 계통 추출을 해보잩
#[3-1]하기 전에 유저별 읽은 글 전체의 데이터가 필요하다.
a <- read_data_cut2 %>% group_by(user_id) %>% summarize(count = sum(count))

#[3-2]  몇명을 추출해야 하는거?
clt_store <- rep(NA, 50000)
sample_size <- 10000
set.seed(2000)
for (k in seq_along(clt_store)){
  x <- sample(a$count, size = sample_size)
  clt_store[k] <- mean(x) # x bar
  if (k %% 20000 == 0) cat(k, "\n")
}
sd_a<- clt_store %>% var
N <- nrow(a)
sigma <- var(a$count)
n <- N*sigma / (N*sd_a + sigma)

#[3-3] 그럼 만명의 obs만 뽑아보자. 
k <- N/n #k = 12
set.seed(2000)
sample(1:12, 1)
sampled_read <- a[(seq(from = 3, to = nrow(a), by = 11)),]

sampled_data <- right_join(sampled_read, read_data_cut, by = c('user_id' = 'user_id'))
sampled_data <- sampled_data %>% na.omit
sampled_data <- sampled_data %>% select(-count)

#[3-4] 뽑힌 샘플을 확인해보자.
sampled_data$article_id %>% unique() %>% length #글은 70946개.
sampled_data$user_id %>% unique() %>% length #유저는 10284개.


#[4] 샘플을 train test로 나눠야 한다. 
########[4]뽑힌 샘플과 read데이터를 합쳐준다.
sample_train <- sampled_data %>% filter(dt <20190201)
sample_test <- sampled_data %>% filter(dt >= 20190201 & dt <= 20190301)

#metadata 정제하기.
metadata <- fread('metadata.csv', encoding = 'UTF-8')
metadata <- metadata[(metadata$author_id != '@brunch'),] #브런치는 공지글이니 삭제한다.
metadata <- metadata %>% mutate(unixtime=substr(as.character(reg_ts),1,10)) %>% 
  mutate(time=anytime(as.integer(unixtime)))#유닉스 시간 바꿔주기.
metadata <- metadata %>% select(-unixtime)

#2월부터 만들어진 글은 삭제해준다. 
meatadata <- metadata[!(metadata$time > '2019-01-31'),]
metadata <- metadata %>% select(article_number, article_keyword)

#train data 만들기
train_merged <- left_join(sample_train, metadata, by = c('article_id' =  'article_number'))
library(tm)
train_merged$article_keyword <- train_merged$article_keyword %>% tm::removePunctuation()
train_merged <- train_merged[!(train_merged$article_keyword == ''),] #빈칸 삭제
train_merged <- train_merged %>% na.omit
write.csv(train_merged, 'train.csv', row.names = F)

#test data 만들기
test_merged <- left_join(sample_test, metadata, by = c('article_id' =  'article_number'))
test_merged$article_keyword <- test_merged$article_keyword %>% tm::removePunctuation()
test_merged <- test_merged[!(test_merged$article_keyword == ''),] #빈칸 삭제
test_merged <- test_merged %>% na.omit

#test에 쓰인 article은 모두 train에 있어야 한다.
(test_merged$article_id %>% unique) %in% (train_merged$article_id %>% unique) %>% sum
unique_test <- (test_merged$article_id %>% unique)[(test_merged$article_id %>% unique) %in%
                                                     (train_merged$article_id %>% unique)] %>% as.data.frame()
unique_test$d <- 0L
test_merged <- left_join(test_merged, unique_test, by = c('article_id' = '.'))
test_merged <- test_merged %>% na.omit
test_merged <- test_merged %>% select(-d)
write.csv(test_merged, 'test.csv', row.names = F)

library(devtools)
library(dplyr)
library(libFMexe)
library(data.table)
library(tm)
library(caret)
library(ModelMetrics)
#devtools::install_github("andland/libFMexe", force = TRUE)

#그리고 이 파일을 저장해줘야 한다. http://www.libfm.org/#download 에 들어가서 다운로드 한 뒤 
#C:\\libFM라는 폴더를 만들어 저장해주자. 이렇게 하는 이유는 밑의 코드에서 exe_loc = "C:\\libFM"
#의 디렉토리에서 불러오기 때문!
setwd('C:/Users/jiho0/OneDrive/바탕 화면/주제분석/data')
#fangtest <- fread('fangtest.csv', encoding= 'UTF-8')

#train set을 재 범주화 해줘야 한다.
bumju <- fread('train을바꿔준것.csv', encoding = 'UTF-8')
bumju$key <- bumju$key %>% tm::removePunctuation()
bumju %>% colnames
bumju <- bumju %>% select(article_id, key)
bumju$article_id %>% unique %>% length
bumju <-  bumju[-which(duplicated(bumju$article_id)),]
trainset <- fread('small_train.csv')
trainset <- left_join(trainset, bumju, by = c('article_id'= 'article_id'))
trainset <- trainset %>% select(-dt, -article_keyword)

#테스트 셋도 재 범주화 해주자.
real_test <- fread('small_test.csv')
testset <- left_join(real_test, bumju, by = c('article_id'= 'article_id'))
testset <- testset %>% select(-dt, -article_keyword)

#score을 추출해줘야 한다.
scoreset <- trainset %>% group_by(user_id, article_id) %>% summarize(count = n(), key = unique(key))
colnames(scoreset) <- c('user_id', 'article_id', 'count', 'article_keyword')
scoreset_test <- testset %>% group_by(user_id, article_id) %>% summarize(count = n(), key = unique(key))
colnames(scoreset_test) <- c('user_id', 'article_id', 'count', 'article_keyword')

#칼럼들을 factor화 시켜주자.
scoreset$user_id <- scoreset$user_id %>% as.factor()
scoreset$article_id <- scoreset$article_id %>% as.factor()
scoreset$article_keyword <- scoreset$article_keyword %>% as.factor()
scoreset <- scoreset %>% as.data.frame()
scoreset_test$user_id <- scoreset_test$user_id %>% as.factor()
scoreset_test$article_id <- scoreset_test$article_id %>% as.factor()
scoreset_test$article_keyword <- scoreset_test$article_keyword %>% as.factor()
scoreset_test <- scoreset_test %>% as.data.frame()

#과적합을 막기 위해 test의 값을 따로 빼줘야겠다.
answer <- scoreset_test$count
scoreset_test$count <- 0

#cv로 최적의 dim을 찾을 수 있다.
#set.seed(2007)
#cvFM_sgd <-  cv_libFM(scoreset, formula = count ~ user_id + article_id + article_keyword,
#                task = "r", method = 'sgd', dims = 0:10, cv_verbosity = 1, exe_loc = "C:\\libFM")
#set.seed(2007)
#cvFM_als <-  cv_libFM(scoreset, formula = count ~ user_id + article_id + article_keyword,
#                      task = "r", method = 'als', dims = 0:15, cv_verbosity = 1, exe_loc = "C:\\libFM")
#cvFM_mcmc <-  cv_libFM(scoreset, formula = count ~ user_id + article_id + article_keyword,
#                      task = "r", method = 'mcmc', dims = 0:15, cv_verbosity = 1, exe_loc = "C:\\libFM")
#cvFM_mcmc
#cvFM_sgd
#cvFM_als

#예측하는 모델을 만들어서 RMSE를 확인해보자.
predFM_sgd <- libFM(train = scoreset, test = scoreset_test, formula = count ~ user_id + article_id + article_keyword,
                    task = 'r', dim = 0, iter = 500, method = 'sgd', exe_loc = "C:\\libFM")
rmse(answer,predFM_sgd)
mae(answer,predFM_sgd)

predFM_sgd2 <- libFM(train = scoreset, test = scoreset_test, formula = count ~ user_id + article_id + article_keyword,
                     task = 'r', dim = 0, iter = 500, method = 'sgd', exe_loc = "C:\\libFM")
rmse(answer,predFM_sgd2)
mae(answer,predFM_sgd2)

predFM_als <- libFM(train = scoreset, test = scoreset_test, formula = count ~ user_id + article_id + article_keyword,
                    task = 'r', dim = 19, iter = 500, method = 'als', exe_loc = "C:\\libFM")
rmse(answer,predFM_als)
mae(answer,predFM_als)

predFM_als1 <- libFM(train = scoreset, test = scoreset_test, formula = count ~ user_id + article_id + article_keyword,
                     task = 'r', dim = 0, iter = 500, method = 'als', exe_loc = "C:\\libFM")
rmse(answer,predFM_als1)
mae(answer,predFM_als1)

predFM_mcmc <- libFM(train = scoreset, test = scoreset_test, formula = count ~ user_id + article_id + article_keyword,
                     task = 'r', dim = 5, iter = 500, method = 'mcmc', exe_loc = "C:\\libFM")
rmse(answer, predFM_mcmc)
mae(answer, predFM_mcmc)


#전체 사람에 대해서 뽑을 수 있고,
test <-  data.frame(user_id = factor(levels = levels(scoreset$user_id),
                                     rep(levels(scoreset$user_id), each = length(levels(scoreset$article_id)))), 
                    article_id = factor(levels = levels(scoreset$article_id),
                                        rep(levels(scoreset$article_id), length(levels(scoreset$user_id)))),
                    Rating = 0L)
test_test <- left_join(test, bumju, by = ('article_id' = 'article_id'))
test_test$user_id <- test_test$user_id %>% as.factor()
test_test$article_id <- test_test$article_id %>% as.factor()
test_test$key <- test_test$key %>% as.factor()
colnames(test_test) <- scoreset %>% colnames()
predFM_test <- libFM(train = scoreset, test = test_test, formula = count ~ user_id + article_id + article_keyword,
                     task = 'r', dim = 5, iter = 500, method = 'mcmc', exe_loc = "C:\\libFM")
test_test$pred_count <- predFM_test
testuser_real <-scoreset[scoreset$user_id == '#fd466b976c4da76dc3a7abe3b95a1c75',]
kkk <- test_test %>% filter(test_test$user_id == '#fd466b976c4da76dc3a7abe3b95a1c75') %>% arrange(desc(pred_count))

kkk