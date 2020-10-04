##################한쿸경제 뉴스기사 크롤링#########################################
library('rvest')
library('dplyr')
library('stringr')
library('httr')
setwd('C:/Users/jiho0/OneDrive/바탕 화면/뉴스크롤링')

#한국경제 크롤링 - economy 
sector <- c(seq(767, 14585, 1000), 14586) 
error_vec <- c()
#for(j in 6 : (length(sector) - 1)){ 
for(j in c(14)){ 
  d_article <- c()
  for (i in sector[j]:(sector[j+1] - 1)){
    url_a <- paste0('https://www.hankyung.com/economy?hkonly=true&page=',sep = "",i)
    url_ad <- read_html(url_a,encoding="euc_kr")
    newsbase1 <- html_nodes(url_ad, "ul.list_basic")
    title <- html_nodes(newsbase1,"h3.tit") %>% html_text()
    day <- html_nodes(newsbase1,"span.time")  %>% html_text()
    article <- cbind(title, day)
    d_article <- rbind(d_article, article)
    Sys.sleep(1)
    if (i %% 10 == 0 ){
      print(i)
    }
    rm(url_a)
    rm(url_ad)
  }
  write.csv(d_article, paste0('article_economy', j, '.csv'))  
  rm(d_article)
}
write.csv(error_vec, 'economy_error.csv')
rm(list = ls())


#한국경제 크롤링 - society (지호 전체 다시 해야함.)
sector <- c(seq(442, 10487, 1000), 10488) 
error_vec <- c()
#for(j in 1 : (length(sector) - 1)){ 
for(j in 9 : (length(sector) - 1)){ 
  d_article <- c()
  for (i in sector[j]:(sector[j+1] - 1)){
    url_a <- paste0('https://www.hankyung.com/society?hkonly=true&page=',sep = "",i)
    url_ad <- read_html(url_a,encoding="euc_kr")
    newsbase1 <- html_nodes(url_ad, "ul.list_basic")
    title <- html_nodes(newsbase1,"h3.tit") %>% html_text()
    day <- html_nodes(newsbase1,"span.time")  %>% html_text()
    article <- cbind(title, day)
    d_article <- rbind(d_article, article)
    Sys.sleep(1)
    if (i %% 10 == 0 ){
      print(i)
    }
    rm(url_a)
    rm(url_ad)
  }
  write.csv(d_article, paste0('article_society', j, '.csv'))  
  rm(d_article)
  
}
write.csv(error_vec, 'society_error.csv')
rm(list = ls())



#한국경제 크롤링 - politics 
sector <- c(seq(189, 3778, 1000), 3779) 
error_vec <- c()
for(j in 1 : (length(sector) - 1)){
  d_article <- c()
  for (i in sector[j]:(sector[j+1] - 1)) {
    url_a <- paste0('https://www.hankyung.com/politics?hkonly=true&page=',sep = "",i)
    url_ad <- read_html(url_a,encoding="euc_kr")
    newsbase1 <- html_nodes(url_ad, "ul.list_basic")
    title <- html_nodes(newsbase1,"h3.tit") %>% html_text()
    day <- html_nodes(newsbase1,"span.time")  %>% html_text()
    article <- cbind(title, day)
    d_article <- rbind(d_article, article)
    Sys.sleep(1)
    if (i %% 10 == 0 ){
      print(i)
    }
  }
  write.csv(d_article, paste0('article_politics', j, '.csv'))  
  rm(d_article)
}
write.csv(error_vec, 'politics_error.csv')
rm(list = ls())



#한국경제 크롤링 - finance
sector <- c(seq(261, 8071, 1000), 8072) 
error_vec <- c()
#for(j in 6 : (length(sector) - 1)){ 
j <- 7
d_article <- c()
for (i in sector[j]:(sector[j+1] - 1)) {
  url_a <- paste0('https://www.hankyung.com/finance?hkonly=true&page=',sep = "",i)
  url_ad <- read_html(url_a,encoding="euc_kr")
  newsbase1 <- html_nodes(url_ad, "ul.list_basic")
  title <- html_nodes(newsbase1,"h3.tit") %>% html_text()
  day <- html_nodes(newsbase1,"span.time")  %>% html_text()
  article <- cbind(title, day)
  d_article <- rbind(d_article, article)
  Sys.sleep(1)
  if (i %% 10 == 0 ){
    print(i)
  }
  rm(url_a)
  rm(url_ad) 
}
write.csv(d_article, paste0('article_finance', j, '.csv'))  
rm(d_article)
#}
write.csv(error_vec, 'finance_error.csv')
rm(list = ls())


#한국경제 크롤링 - realestate (현지 - done)
sector <- c(seq(155, 3926, 1000), 3927) 
error_vec <- c()
for(j in 1 : (length(sector) - 1)){ 
  d_article <- c()
  for (i in sector[j]:(sector[j+1] - 1)) {
    url_a <- paste0('https://www.hankyung.com/realestate?hkonly=true&page=',sep = "",i)
    url_ad <- read_html(url_a,encoding="euc_kr")
    newsbase1 <- html_nodes(url_ad, "ul.list_basic")
    title <- html_nodes(newsbase1,"h3.tit") %>% html_text()
    day <- html_nodes(newsbase1,"span.time")  %>% html_text()
    article <- cbind(title, day)
    d_article <- rbind(d_article, article)
    Sys.sleep(1)
    if (i %% 10 == 0 ){
      print(i)
    }
    rm(url_a)
    rm(url_ad)
  }
  write.csv(d_article, paste0('article_realestate', j, '.csv'))  
  rm(d_article)
}
write.csv(error_vec, 'realestate_error.csv')
rm(list = ls())


#한국경제 크롤링 - international 
sector <- c(seq(170, 2130, 1000), 2131) 
error_vec <- c()
for(j in c(1,2)){
  d_article <- c()
  for (i in sector[j]:(sector[j+1] - 1)) {
    url_a <- paste0('https://www.hankyung.com/international?hkonly=true&page=',sep = "",i)
    url_ad <- read_html(url_a,encoding="euc_kr")
    newsbase1 <- html_nodes(url_ad, "ul.list_basic")
    title <- html_nodes(newsbase1,"h3.tit") %>% html_text()
    day <- html_nodes(newsbase1,"span.time")  %>% html_text()
    article <- cbind(title, day)
    d_article <- rbind(d_article, article)
    Sys.sleep(1)
    if (i %% 10 == 0 ){
      print(i)
    }
    rm(url_a)
    rm(url_ad)
  }
  write.csv(d_article, paste0('article_international', j, '.csv'))  
  rm(d_article)
}
write.csv(error_vec, 'international_error.csv')
rm(list = ls())


#한국경제 크롤링 - it 
sector <- c(seq(150, 2307, 1000), 2308) 
error_vec <- c()
for(j in 1 : (length(sector) - 1)){ 
  d_article <- c()
  for (i in sector[j]:(sector[j+1] - 1)) tryCatch({
    url_a <- paste0('https://www.hankyung.com/it?hkonly=true&page=',sep = "",i)
    url_ad <- read_html(url_a,encoding="euc_kr")
    newsbase1 <- html_nodes(url_ad, "ul.list_basic")
    title <- html_nodes(newsbase1,"h3.tit") %>% html_text()
    day <- html_nodes(newsbase1,"span.time")  %>% html_text()
    article <- cbind(title, day)
    d_article <- rbind(d_article, article)
    Sys.sleep(1)
    if (i %% 10 == 0 ){
      print(i)
    }
  }, error=function(e){append(error_vec, i)}
  )
  write.csv(d_article, paste0('article_it', j, '.csv'))  
  rm(d_article)
}
write.csv(error_vec, 'it_error.csv')


#결과적으로 각 여섯 세션의 데이터를 rbind하면 총 뉴스데이터를 얻을 수 있다.



################################################################################################################
################################################################################################################
#########산업군 크롤링 파트
################################################################################################################
################################################################################################################


library('rvest')
library('dplyr')
library('stringr')
library('httr')
library(KoNLP)
#산업군 크롤링.
#contents > div.table_style01 > table > tbody > tr:nth-child(8) > td.text_l > a
#[1]인코딩을 파악한다. 
a<- GET('http://consensus.hankyung.com/apps.analysis/analysis.list?&sdate=2003-01-01&edate=2019-11-17&report_type=CO&pagenum=80&order_type=&now_page=206')
guess_encoding(a) #인코딩은 ISO-8859-1 ??
guess_encoding(texting) 

#[2]크롤링을 시작한다.
for(j in c(14)){ #contents > div.table_style01 > table > tbody > tr:nth-child(1) > td.text_l
  d_article <- c()
  for (i in 206:3336){
    url_a <- paste0('http://consensus.hankyung.com/apps.analysis/analysis.list?&sdate=2003-01-01&edate=2019-11-17&report_type=CO&pagenum=80&order_type=&now_page=',i)
    url_ad <- read_html(url_a, encoding = 'ISO-8859-1')
    date <- html_nodes(url_ad, "tbody") %>% html_nodes('td.first.txt_number') %>% html_text
    texting <- html_nodes(url_ad, "tbody")  %>% html_nodes('td.text_l') %>% 
      html_nodes('a') %>% html_text()
    article <- cbind(title, day)
    d_article <- rbind(d_article, article)
    Sys.sleep(1)
    if (i %% 10 == 0 ){
      print(i)
    }
    rm(url_a)
    rm(url_ad)
  }
  write.csv(d_article, paste0('article_economy', j, '.csv'))  
  rm(d_article)
}
write.csv(error_vec, 'economy_error.csv')
rm(list = ls())



#[1]cd C:\r_selenium 입력
#[2]java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445 입력.
library(RSelenium)
library(rvest)
remDr<-remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
remDr$open()
i <- 207
remDr$navigate(paste0('http://consensus.hankyung.com/apps.analysis/analysis.list?&sdate=2003-01-01&edate=2019-11-17&report_type=CO&pagenum=80&order_type=&now_page=',i))

#페이지 소스 읽어오기
a <- data.frame()
for(i in 206:3336) {
  remDr$navigate(paste0('http://consensus.hankyung.com/apps.analysis/analysis.list?&sdate=2003-01-01&edate=2019-11-17&report_type=CO&pagenum=80&order_type=&now_page=',i))
  url_item<-remDr$getPageSource()[[1]] #페이지 소스 읽어오기
  url_base<-read_html(url_item, encoding="UTF-8")
  date <- url_base %>% html_nodes("td.first.txt_number") %>% html_text()
  texting <- url_base  %>%
    html_nodes(xpath = '//*[@id="contents"]/div[2]/table/tbody/tr/td[2]') %>% html_text
  price <- url_base %>% html_nodes("td.text_r.txt_number")  %>% html_text()
  opinion <- url_base  %>% 
    html_nodes(xpath = '//*[@id="contents"]/div[2]/table/tbody/tr/td[4]') %>% html_text
  source <- url_base  %>% 
    html_nodes(xpath = '//*[@id="contents"]/div[2]/table/tbody/tr/td[6]') %>% html_text
  a <- rbind(a, cbind(date, texting, price, opinion, source) %>% as.data.frame())
  rm(date, texting, price, opinion, source)
  if(i %% 10 == 0){
    print(i)
  }
}


library(rvest);library(tidyverse);library(magrittr)
library(RSelenium); library(data.table)
#[1]cd C:\r_selenium 입력
#[2]java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445 입력.
remDr<-remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
remDr$open()


#//*[@id="contents"]/div[1]/div[2]/ul/li[2]/div[1]/a/strong
a <- data.frame()
#1:1861
for(i in 1:930) {
  remDr$navigate(paste0('http://www.paxnet.co.kr/stock/report/report?menuCode=2222&reportId=0&searchKey=stock&currentPageNo=',i))
  url_item<-remDr$getPageSource()[[1]] #페이지 소스 읽어오기
  url_base<-read_html(url_item, encoding="UTF-8")
  date <- url_base %>%
    html_nodes(xpath='//*[@id="contents"]/div[1]/div[2]/ul/li[*]/div[6]') %>% html_text()
  company <- url_base %>%
    html_nodes(xpath='//*[@id="contents"]/div[1]/div[2]/ul/li[*]/div[1]') %>% html_text()
  texting <- url_base  %>%
    html_nodes(xpath = '//*[@id="contents"]/div[1]/div[2]/ul/li[*]/div[2]/p/a') %>% html_text
  price <- url_base %>%
    html_nodes(xpath='//*[@id="contents"]/div[1]/div[2]/ul/li[*]/div[3]')  %>% html_text() %>%
    str_replace_all('적정가격','') %>% trimws()
  opinion <- url_base  %>% 
    html_nodes(xpath = '//*[@id="contents"]/div[1]/div[2]/ul/li[*]/div[4]/text()') %>% html_text
  source <- url_base  %>% 
    html_nodes(xpath = '//*[@id="contents"]/div[1]/div[2]/ul/li[*]/div[5]') %>% html_text
  a <- rbind(a, cbind(data=date[-1], company[-1], texting, price=price[-1], opinion=opinion[-1], source=source[-1]) %>% as.data.frame())
  rm(date, texting, price, opinion, source)
  if(i %% 10 == 0){
    print(i)
  }
}
a$opinion <- a$opinion %>%  str_trim(side=c("both"))
a$opinion %>% table
hollowdata <- a %>% filter(opinion == '-')
write.csv(a, '1-930crawling.csv', row.names = F)
write.csv(hollowdata, 'hollowdata.csv', row.names = F)

###################################감성사전 구축########################################################
#각 기사들을 합쳐줘야한다.
library(data.table)
library(dplyr)
library(lubridate)
library(readr)
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터/뉴스크롤링')

#여섯 섹션의 뉴스를 하나로 합치자.
art_eco <- fread('article_economy.csv', encoding = 'UTF-8')
art_so <- fread('article_society.csv', encoding = 'UTF-8')
art_it <- fread('article_it.csv', encoding =  'UTF-8')
art_pol <- fread('article_politics.csv', encoding =  'UTF-8')
art_inter <- fread('article_international.csv', encoding =  'UTF-8')
art_fin <- fread('article_finance.csv', encoding = 'UTF-8')
article <- rbind(art_eco, art_fin, art_inter, art_it, art_pol, art_so)
analysis <- fread('증권사 리포트_감성사전 추가.csv')
analysis$time <- 14L
colnames(analysis) <- c('day', 'title', 'time')
analysis <- analysis[,c(2,1,3)]
article <- rbind(article, analysis)
article %>% str
article$day <- article$day %>% lubridate::ymd()
article$day[1] + 1
i <- 1

ifelse((article$time >= 15) == TRUE, article$day <- article$day + 1, article$day)

for(i in 1:nrow(article)) { 
  if(article$time[i] >= 15) { 
    article$day[i] <- article$day[i] + 1
  }
  if(i %% 10 ==0) {
    print(i)
  }
}

article$date2 <- article$day %>% wday(label = T)
article <- article %>% filter(date2 !='일', date2 !='토')

#2019년을 없애주고, 주말을 제거해준다. 
article <- article %>% arrange(desc(day))
article <- article[229:nrow(article),]

#이제 필요없는 열을 없애주자. 
colnames(article)
article <- article %>% select(title, day)
write.csv(article, 'final_article_v02.csv', row.names = F)

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#이제 전처리의 시작이다.
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터/뉴스크롤링')
rm(list = ls())
library(data.table)
library(dplyr)
article <- fread('final_article_v02.csv')
article <- article %>% rename(article = title, date = day)

#날짜를 팩터처리 해주자.
article$date <- article$date %>% as.factor()

#날짜 하나당 신문을 모두 모아주자.
article_test <- article %>% group_by(date) %>% summarize(article2 = paste(article, collapse = "  "))
write.csv(article_test, 'article_merged.csv', row.names = F)

#전처리를 해주자.
library(tm)
library(rJava)
library(KoNLP)
library(stringr)
library(reshape2)

#조사를 없애버리자.
article_test$article <- article_test$article2 %>% tm::removePunctuation()
article_test <- article_test %>% select(-article2)
useNIADic()

#실전이다.
x <- article_test[,2]
mp <- x %>% unlist %>% SimplePos09() %>% reshape2::melt()
mp_df <- mp %>% mutate( fwords = (str_replace_all(value, '([:alpha:]+)/J', '') %>% 
                                    str_match('([A-z]+)/F'))[,2],
                        noun = (str_replace_all(value, '([:alpha:]+)/J', '') %>% 
                                  str_match('([:alnum:]+)/N'))[,2])
mp_df %>% head
mp_df[(mp_df[,4] %>% is.na),][,4] <- ''
mp_df[(mp_df[,5] %>% is.na),][,5] <- ''
mp_df <- mp_df %>% mutate(words = paste0(fwords, noun))
mp_df <- mp_df %>% select(L1, words)
mp_df$words <- mp_df$words %>% gsub(pattern = 'NA', replacement = NA)
mp_df <- mp_df %>% na.omit() 
write.csv(mp_df, 'HyengTaeSo.csv', row.names = F)

#################################################################################################
#################################################################################################
##이제 TF-IDF
################################################################################################
################################################################################################
rm(list= ls())
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터/뉴스크롤링')

library(readr) # 파일작성에 필요한 패키지
library(data.table)
library(dplyr)
library(tm)
library(rJava)
library(KoNLP)
library(stringr)
library(reshape2)
library(KoNLP)

#문서를 불러오고 날짜별로 넣어주자. 
article <- fread('HyengTaeSo.csv')
article$L1 <-  gsub(article$L1,pattern = "article",replacement =  '') %>% as.numeric
article <- article %>% arrange(L1) 
article$L1 <- article$L1 %>% as.factor()
article_merged <- article %>% group_by(L1) %>% summarize(sum = paste(words, collapse = "  "))
article <- fread('article_merged.csv')
article_merged$date <- article$date #날짜를 붙여주는 것.
article_merged <- article_merged %>% select(-L1) #필요없는 팩터행을 삭제해주자.
trading <- fread('KOSPI_trading data by groups_v05.csv')
colnames(trading) <- c('date', 'institution', 'others', 'individual', 'foreign')
article_merged <- dplyr::left_join(article_merged, trading , by = c('date' = 'date'))
which(article_merged$date == '2016-12-30')
article_for_train <- article_merged$sum[1:which(article_merged$date == '2016-12-30')]
kkk <- article_merged[1:which(article_merged$date == '2016-12-30'),] 

#적합은 우선 1일 전의 데이터를 가지고 해야한다. 
article_for_train <- article_for_train[c(1:4144)]  

#회귀를 돌려서 감성사전을 만들자.
corpus1<- Corpus(VectorSource(article_for_train))
dtm <- DocumentTermMatrix(corpus1,
                          control=list(removePunctuation=F,
                                       removeNumbers=F,
                                       weighting=weightTfIdf))
kwdDTM_TF <- removeSparseTerms(dtm, as.numeric(0.99)) #removeSparseTerms 수행
Encoding(kwdDTM_TF$dimnames$Terms) = 'euc_kr'
X <- as.matrix(kwdDTM_TF)

x_day0 <- X[c(1:4144),]
x_day1 <- X[c(1:4143),]
x_day2 <- X[c(1:4142),]
x_day3 <- X[c(1:4141),]
x_day4 <- X[c(1:4140),]
x_day5 <- X[c(1:4139),]

y1_day0 <- article_merged$individual[1:4144]
y1_day1 <- article_merged$individual[2:4144]
y1_day2 <- article_merged$individual[3:4144]
y1_day3 <- article_merged$individual[4:4144]
y1_day4 <- article_merged$individual[5:4144]
y1_day5 <- article_merged$individual[6:4144]

y2_day0 <- article_merged$foreign[1:4144]
y2_day1 <- article_merged$foreign[2:4144]
y2_day2 <- article_merged$foreign[3:4144]
y2_day3 <- article_merged$foreign[4:4144]
y2_day4 <- article_merged$foreign[5:4144]
y2_day5 <- article_merged$foreign[6:4144]

y3_day0 <- article_merged$institution[1:4144]
y3_day1 <- article_merged$institution[2:4144]
y3_day2 <- article_merged$institution[3:4144]
y3_day3 <- article_merged$institution[4:4144]
y3_day4 <- article_merged$institution[5:4144]
y3_day5 <- article_merged$institution[6:4144]

y4_day0 <- article_merged$others[1:4144]
y4_day1 <- article_merged$others[2:4144]
y4_day2 <- article_merged$others[3:4144]
y4_day3 <- article_merged$others[4:4144]
y4_day4 <- article_merged$others[5:4144]
y4_day5 <- article_merged$others[6:4144]
rm(corpus1, dtm, kwdDTM_TF, article)

#############################################################################
#-------------------------이제 회귀식을... 돌려볼까 - individual
#ridge - individual_day0
#당일 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_individual_scale_day0 <- x_day0 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y1_scale_day0 <- y1_day0 - mean(y1_day0) 
dataset_individual_scale_day0 <- cbind(y1_day0, X_individual_scale_day0)
ridge_individual_day0 <- glmnet(x = X_individual_scale_day0, y = y1_scale_day0, alpha = 0)
#plot(ridge_individual_day0, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day0 <- cv.glmnet(x = X_individual_scale_day0, y = y1_scale_day0, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)

#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day0$lambda.min  
ridge_tuned <- glmnet(x = X_individual_scale_day0, y = y1_scale_day0, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_individual_day0 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_individual_scale_day0,y1_scale_day0,dataset_individual_scale_day0,ridge_individual_day0,
   cv_ridge_day0,opt_lambda,ridge_tuned)

#ridge - individual_day1
#1일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_individual_scale_day1 <- x_day1 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y1_scale_day1 <- y1_day1 - mean(y1_day1) 
dataset_individual_scale_day1 <- cbind(y1_day1, X_individual_scale_day1)
ridge_individual_day1 <- glmnet(x = X_individual_scale_day1, y = y1_scale_day1, alpha = 0)
#plot(ridge_individual_day1, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day1 <- cv.glmnet(x = X_individual_scale_day1, y = y1_scale_day1, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day1$lambda.min  
ridge_tuned <- glmnet(x = X_individual_scale_day1, y = y1_scale_day1, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_individual_day1 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_individual_scale_day1,y1_scale_day1,dataset_individual_scale_day1,ridge_individual_day1,
   cv_ridge_day1,opt_lambda,ridge_tuned)

#ridge - individual_day2
#2일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
dataset_individual_day2 <- cbind(y1_day2, x_day2) %>% as.data.frame()
X_individual_scale_day2 <- x_day2 %>% scale
y1_scale_day2 <- y1_day2 - mean(y1_day2)
dataset_individual_scale_day2 <- cbind(y1_day2, X_individual_scale_day2)
ridge_individual_day2 <- glmnet(x = X_individual_scale_day2, y = y1_scale_day2, alpha = 0)
set.seed(1234)
cv_ridge_day2 <- cv.glmnet(x = X_individual_scale_day2, y = y1_scale_day2, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day2$lambda.min  
ridge_tuned <- glmnet(x = X_individual_scale_day2, y = y1_scale_day2, alpha = 0, lambda = opt_lambda)
coef_ridge_individual_day2 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_individual_scale_day2,y1_scale_day2,dataset_individual_scale_day2,ridge_individual_day2,
   cv_ridge_day2,opt_lambda,ridge_tuned)

#ridge - individual_day3
#3일 뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_individual_day3 <- cbind(y1_day3, x_day3) %>% as.data.frame()
X_individual_scale_day3 <- x_day3 %>% scale
y1_scale_day3 <- y1_day3 - mean(y1_day3)
dataset_individual_scale_day3 <- cbind(y1_day3, X_individual_scale_day3)
ridge_individual_day3 <- glmnet(x = X_individual_scale_day3, y = y1_scale_day3, alpha = 0)
set.seed(1234)
cv_ridge_day3 <- cv.glmnet(x = X_individual_scale_day3, y = y1_scale_day3, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day3$lambda.min  
ridge_tuned <- glmnet(x = X_individual_scale_day3, y = y1_scale_day3, alpha = 0, lambda = opt_lambda)
coef_ridge_individual_day3 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_individual_scale_day3,y1_scale_day3,dataset_individual_scale_day3,ridge_individual_day3,
   cv_ridge_day3,opt_lambda,ridge_tuned)

#ridge - individual_day4
#4일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_individual_day4 <- cbind(y1_day4, x_day4) %>% as.data.frame()
X_individual_scale_day4 <- x_day4 %>% scale
y1_scale_day4 <- y1_day4 - mean(y1_day4)
dataset_individual_scale_day4 <- cbind(y1_day4, X_individual_scale_day4)
ridge_individual_day4 <- glmnet(x = X_individual_scale_day4, y = y1_scale_day4, alpha = 0)
set.seed(1234)
cv_ridge_day4 <- cv.glmnet(x = X_individual_scale_day4, y = y1_scale_day4, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day4$lambda.min  
ridge_tuned <- glmnet(x = X_individual_scale_day4, y = y1_scale_day4, alpha = 0, lambda = opt_lambda)
coef_ridge_individual_day4 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_individual_scale_day4,y1_scale_day4,dataset_individual_scale_day4,ridge_individual_day4,
   cv_ridge_day4,opt_lambda,ridge_tuned)

#ridge - individual_day5
#5일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_individual_day5 <- cbind(y1_day5, x_day5) %>% as.data.frame()
X_individual_scale_day5 <- x_day5 %>% scale
y1_scale_day5 <- y1_day5 - mean(y1_day5)
dataset_individual_scale_day5 <- cbind(y1_day5, X_individual_scale_day5)
ridge_individual_day5 <- glmnet(x = X_individual_scale_day5, y = y1_scale_day5, alpha = 0)
set.seed(1234)
cv_ridge_day5 <- cv.glmnet(x = X_individual_scale_day5, y = y1_scale_day5, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day5$lambda.min  
ridge_tuned <- glmnet(x = X_individual_scale_day5, y = y1_scale_day5, alpha = 0, lambda = opt_lambda)
coef_ridge_individual_day5 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_individual_scale_day5,y1_scale_day5,dataset_individual_scale_day5,ridge_individual_day5,
   cv_ridge_day5,opt_lambda,ridge_tuned)

#망각곡선 고려 안 했을때 - individual
coef_ridge_individual_noforget <- (coef_ridge_individual_day0 + coef_ridge_individual_day1 + 
                                     coef_ridge_individual_day2 + coef_ridge_individual_day3 + 
                                     coef_ridge_individual_day4 + coef_ridge_individual_day5)
pos_ridge_individual_noforget <- coef_ridge_individual_noforget[coef_ridge_individual_noforget > 0]
neg_ridge_individual_noforget <- coef_ridge_individual_noforget[coef_ridge_individual_noforget < 0]
write.csv(pos_ridge_individual_noforget, 'pos_ridge_individual_noforget.csv', row.names = T)
write.csv(neg_ridge_individual_noforget, 'neg_ridge_individual_noforget.csv', row.names = T)
rm(coef_ridge_individual_noforget,pos_ridge_individual_noforget,neg_ridge_individual_noforget)

#individual_forget curve
forgetting <- function(x) {
  1.84 / (log10(x)^1.25 + 1.84)
}
totalsum<- sum(forgetting(1),forgetting(1*60*24*1),forgetting(1*60*24*2),forgetting(1*60*24*3),
               forgetting(1*60*24*4))
#total sum을 통해서 그 합이 1이 되게 해준다. 
coef_ridge_individual <- (coef_ridge_individual_day0 *forgetting(1) + 
                            coef_ridge_individual_day1 * forgetting(1*60*24*1)  + coef_ridge_individual_day2 * forgetting(1*60*24*2)  + 
                            coef_ridge_individual_day3 * forgetting(1*60*24*3)  + 
                            coef_ridge_individual_day4 * forgetting(1*60*24*4)  + 
                            coef_ridge_individual_day5 * forgetting(1*60*24*5))

#망각 곡선을 곱해주어 시간에 대한 지속성을 고려한 회귀계수를 도출한다.
pos_ridge_individual <- coef_ridge_individual[coef_ridge_individual > 0]
#회귀계수가 0 이상이면 긍정적인 단어로!
neg_ridge_individual <- coef_ridge_individual[coef_ridge_individual < 0]
#회귀계수가 0보다 작으면 부정적인 단어로!
write.csv(pos_ridge_individual, 'pos_ridge_individual.csv', row.names = T)
write.csv(neg_ridge_individual, 'neg_ridge_individual.csv', row.names = T)
#결과적으로 개인 투자자의 순 거래대금에 대한 긍정 사전과 부정 사전을 
#만들어낼수 있었다.
rm(coef_ridge_individual, coef_ridge_individual_day0, coef_ridge_individual_day1, coef_ridge_individual_day2, 
   coef_ridge_individual_day3, coef_ridge_individual_day4, coef_ridge_individual_day5,y1_day1, 
   y1_day2, y1_day3,y1_day4,y1_day5)

#이와 같은 과정을 회국인, 기관, 기타법인 투자자에 대해 같은 과정을 
#반복해준다.
#############################################################################
#-------------------------이제 회귀식을... 돌려볼까 - foreign

#ridge - foreign_day0
#당일 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_foreign_scale_day0 <- x_day0 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y2_scale_day0 <- y2_day0 - mean(y2_day0) 
dataset_foreign_scale_day0 <- cbind(y2_day0, X_foreign_scale_day0)
ridge_foreign_day0 <- glmnet(x = X_foreign_scale_day0, y = y2_scale_day0, alpha = 0)
#plot(ridge_foreign_day0, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day0 <- cv.glmnet(x = X_foreign_scale_day0, y = y2_scale_day0, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)

#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day0$lambda.min  
ridge_tuned <- glmnet(x = X_foreign_scale_day0, y = y2_scale_day0, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_foreign_day0 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_foreign_scale_day0,y2_scale_day0,dataset_foreign_scale_day0,ridge_foreign_day0,
   cv_ridge_day0,opt_lambda,ridge_tuned)

#ridge - foreign_day2
#1일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_foreign_scale_day2 <- x_day2 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y2_scale_day2 <- y2_day2 - mean(y2_day2) 
dataset_foreign_scale_day2 <- cbind(y2_day2, X_foreign_scale_day2)
ridge_foreign_day2 <- glmnet(x = X_foreign_scale_day2, y = y2_scale_day2, alpha = 0)
#plot(ridge_foreign_day2, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day2 <- cv.glmnet(x = X_foreign_scale_day2, y = y2_scale_day2, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day2$lambda.min  
ridge_tuned <- glmnet(x = X_foreign_scale_day2, y = y2_scale_day2, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_foreign_day2 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_foreign_scale_day2,y2_scale_day2,dataset_foreign_scale_day2,ridge_foreign_day2,
   cv_ridge_day2,opt_lambda,ridge_tuned)

#ridge - foreign_day2
#2일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
dataset_foreign_day2 <- cbind(y2_day2, x_day2) %>% as.data.frame()
X_foreign_scale_day2 <- x_day2 %>% scale
y2_scale_day2 <- y2_day2 - mean(y2_day2)
dataset_foreign_scale_day2 <- cbind(y2_day2, X_foreign_scale_day2)
ridge_foreign_day2 <- glmnet(x = X_foreign_scale_day2, y = y2_scale_day2, alpha = 0)
set.seed(1234)
cv_ridge_day2 <- cv.glmnet(x = X_foreign_scale_day2, y = y2_scale_day2, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day2$lambda.min  
ridge_tuned <- glmnet(x = X_foreign_scale_day2, y = y2_scale_day2, alpha = 0, lambda = opt_lambda)
coef_ridge_foreign_day2 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_foreign_scale_day2,y2_scale_day2,dataset_foreign_scale_day2,ridge_foreign_day2,
   cv_ridge_day2,opt_lambda,ridge_tuned)

#ridge - foreign_day3
#3일 뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_foreign_day3 <- cbind(y2_day3, x_day3) %>% as.data.frame()
X_foreign_scale_day3 <- x_day3 %>% scale
y2_scale_day3 <- y2_day3 - mean(y2_day3)
dataset_foreign_scale_day3 <- cbind(y2_day3, X_foreign_scale_day3)
ridge_foreign_day3 <- glmnet(x = X_foreign_scale_day3, y = y2_scale_day3, alpha = 0)
set.seed(1234)
cv_ridge_day3 <- cv.glmnet(x = X_foreign_scale_day3, y = y2_scale_day3, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day3$lambda.min  
ridge_tuned <- glmnet(x = X_foreign_scale_day3, y = y2_scale_day3, alpha = 0, lambda = opt_lambda)
coef_ridge_foreign_day3 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_foreign_scale_day3,y2_scale_day3,dataset_foreign_scale_day3,ridge_foreign_day3,
   cv_ridge_day3,opt_lambda,ridge_tuned)

#ridge - foreign_day4
#4일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_foreign_day4 <- cbind(y2_day4, x_day4) %>% as.data.frame()
X_foreign_scale_day4 <- x_day4 %>% scale
y2_scale_day4 <- y2_day4 - mean(y2_day4)
dataset_foreign_scale_day4 <- cbind(y2_day4, X_foreign_scale_day4)
ridge_foreign_day4 <- glmnet(x = X_foreign_scale_day4, y = y2_scale_day4, alpha = 0)
set.seed(1234)
cv_ridge_day4 <- cv.glmnet(x = X_foreign_scale_day4, y = y2_scale_day4, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day4$lambda.min  
ridge_tuned <- glmnet(x = X_foreign_scale_day4, y = y2_scale_day4, alpha = 0, lambda = opt_lambda)
coef_ridge_foreign_day4 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_foreign_scale_day4,y2_scale_day4,dataset_foreign_scale_day4,ridge_foreign_day4,
   cv_ridge_day4,opt_lambda,ridge_tuned)

#ridge - foreign_day5
#5일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_foreign_day5 <- cbind(y2_day5, x_day5) %>% as.data.frame()
X_foreign_scale_day5 <- x_day5 %>% scale
y2_scale_day5 <- y2_day5 - mean(y2_day5)
dataset_foreign_scale_day5 <- cbind(y2_day5, X_foreign_scale_day5)
ridge_foreign_day5 <- glmnet(x = X_foreign_scale_day5, y = y2_scale_day5, alpha = 0)
set.seed(1234)
cv_ridge_day5 <- cv.glmnet(x = X_foreign_scale_day5, y = y2_scale_day5, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day5$lambda.min  
ridge_tuned <- glmnet(x = X_foreign_scale_day5, y = y2_scale_day5, alpha = 0, lambda = opt_lambda)
coef_ridge_foreign_day5 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_foreign_scale_day5,y2_scale_day5,dataset_foreign_scale_day5,ridge_foreign_day5,
   cv_ridge_day5,opt_lambda,ridge_tuned)

#망각곡선 고려 안 했을때 - foreign
coef_ridge_foreign_noforget <- (coef_ridge_foreign_day0 + coef_ridge_foreign_day2 + 
                                  coef_ridge_foreign_day2 + coef_ridge_foreign_day3 + 
                                  coef_ridge_foreign_day4 + coef_ridge_foreign_day5)
pos_ridge_foreign_noforget <- coef_ridge_foreign_noforget[coef_ridge_foreign_noforget > 0]
neg_ridge_foreign_noforget <- coef_ridge_foreign_noforget[coef_ridge_foreign_noforget < 0]
write.csv(pos_ridge_foreign_noforget, 'pos_ridge_foreign_noforget.csv', row.names = T)
write.csv(neg_ridge_foreign_noforget, 'neg_ridge_foreign_noforget.csv', row.names = T)
rm(coef_ridge_foreign_noforget,pos_ridge_foreign_noforget,neg_ridge_foreign_noforget)

#foreign_forget curve
forgetting <- function(x) {
  1.84 / (log10(x)^1.25 + 1.84)
}
totalsum<- sum(forgetting(1),forgetting(1*60*24*1),forgetting(1*60*24*2),forgetting(1*60*24*3),
               forgetting(1*60*24*4))
#total sum을 통해서 그 합이 1이 되게 해준다. 
coef_ridge_foreign <- (coef_ridge_foreign_day0 *forgetting(1) + 
                         coef_ridge_foreign_day2 * forgetting(1*60*24*1)  + coef_ridge_foreign_day2 * forgetting(1*60*24*2)  + 
                         coef_ridge_foreign_day3 * forgetting(1*60*24*3)  + 
                         coef_ridge_foreign_day4 * forgetting(1*60*24*4)  + 
                         coef_ridge_foreign_day5 * forgetting(1*60*24*5))

#망각 곡선을 곱해주어 시간에 대한 지속성을 고려한 회귀계수를 도출한다.
pos_ridge_foreign <- coef_ridge_foreign[coef_ridge_foreign > 0]
#회귀계수가 0 이상이면 긍정적인 단어로!
neg_ridge_foreign <- coef_ridge_foreign[coef_ridge_foreign < 0]
#회귀계수가 0보다 작으면 부정적인 단어로!
write.csv(pos_ridge_foreign, 'pos_ridge_foreign.csv', row.names = T)
write.csv(neg_ridge_foreign, 'neg_ridge_foreign.csv', row.names = T)
#결과적으로 개인 투자자의 순 거래대금에 대한 긍정 사전과 부정 사전을 
#만들어낼수 있었다.
rm(coef_ridge_foreign, coef_ridge_foreign_day0,coef_ridge_foreign_day2, coef_ridge_foreign_day2, 
   coef_ridge_foreign_day3,coef_ridge_foreign_day4, 
   coef_ridge_foreign_day5,y2_day2, y2_day2, y2_day3,y2_day4,y2_day5)



#############################################################################
#-------------------------이제 회귀식을... 돌려볼까 - institution
#ridge - institution_day0
#당일 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_institution_scale_day0 <- x_day0 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y3_scale_day0 <- y3_day0 - mean(y3_day0) 
dataset_institution_scale_day0 <- cbind(y3_day0, X_institution_scale_day0)
ridge_institution_day0 <- glmnet(x = X_institution_scale_day0, y = y3_scale_day0, alpha = 0)
#plot(ridge_institution_day0, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day0 <- cv.glmnet(x = X_institution_scale_day0, y = y3_scale_day0, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)

#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day0$lambda.min  
ridge_tuned <- glmnet(x = X_institution_scale_day0, y = y3_scale_day0, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_institution_day0 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_institution_scale_day0,y3_scale_day0,dataset_institution_scale_day0,ridge_institution_day0,
   cv_ridge_day0,opt_lambda,ridge_tuned)

#ridge - institution_day3
#1일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_institution_scale_day3 <- x_day3 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y3_scale_day3 <- y3_day3 - mean(y3_day3) 
dataset_institution_scale_day3 <- cbind(y3_day3, X_institution_scale_day3)
ridge_institution_day3 <- glmnet(x = X_institution_scale_day3, y = y3_scale_day3, alpha = 0)
#plot(ridge_institution_day3, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day3 <- cv.glmnet(x = X_institution_scale_day3, y = y3_scale_day3, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day3$lambda.min  
ridge_tuned <- glmnet(x = X_institution_scale_day3, y = y3_scale_day3, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_institution_day3 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_institution_scale_day3,y3_scale_day3,dataset_institution_scale_day3,ridge_institution_day3,
   cv_ridge_day3,opt_lambda,ridge_tuned)

#ridge - institution_day2
#2일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
dataset_institution_day2 <- cbind(y3_day2, x_day2) %>% as.data.frame()
X_institution_scale_day2 <- x_day2 %>% scale
y3_scale_day2 <- y3_day2 - mean(y3_day2)
dataset_institution_scale_day2 <- cbind(y3_day2, X_institution_scale_day2)
ridge_institution_day2 <- glmnet(x = X_institution_scale_day2, y = y3_scale_day2, alpha = 0)
set.seed(1234)
cv_ridge_day2 <- cv.glmnet(x = X_institution_scale_day2, y = y3_scale_day2, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day2$lambda.min  
ridge_tuned <- glmnet(x = X_institution_scale_day2, y = y3_scale_day2, alpha = 0, lambda = opt_lambda)
coef_ridge_institution_day2 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_institution_scale_day2,y3_scale_day2,dataset_institution_scale_day2,ridge_institution_day2,
   cv_ridge_day2,opt_lambda,ridge_tuned)

#ridge - institution_day3
#3일 뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_institution_day3 <- cbind(y3_day3, x_day3) %>% as.data.frame()
X_institution_scale_day3 <- x_day3 %>% scale
y3_scale_day3 <- y3_day3 - mean(y3_day3)
dataset_institution_scale_day3 <- cbind(y3_day3, X_institution_scale_day3)
ridge_institution_day3 <- glmnet(x = X_institution_scale_day3, y = y3_scale_day3, alpha = 0)
set.seed(1234)
cv_ridge_day3 <- cv.glmnet(x = X_institution_scale_day3, y = y3_scale_day3, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day3$lambda.min  
ridge_tuned <- glmnet(x = X_institution_scale_day3, y = y3_scale_day3, alpha = 0, lambda = opt_lambda)
coef_ridge_institution_day3 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_institution_scale_day3,y3_scale_day3,dataset_institution_scale_day3,ridge_institution_day3,
   cv_ridge_day3,opt_lambda,ridge_tuned)

#ridge - institution_day4
#4일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_institution_day4 <- cbind(y3_day4, x_day4) %>% as.data.frame()
X_institution_scale_day4 <- x_day4 %>% scale
y3_scale_day4 <- y3_day4 - mean(y3_day4)
dataset_institution_scale_day4 <- cbind(y3_day4, X_institution_scale_day4)
ridge_institution_day4 <- glmnet(x = X_institution_scale_day4, y = y3_scale_day4, alpha = 0)
set.seed(1234)
cv_ridge_day4 <- cv.glmnet(x = X_institution_scale_day4, y = y3_scale_day4, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day4$lambda.min  
ridge_tuned <- glmnet(x = X_institution_scale_day4, y = y3_scale_day4, alpha = 0, lambda = opt_lambda)
coef_ridge_institution_day4 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_institution_scale_day4,y3_scale_day4,dataset_institution_scale_day4,ridge_institution_day4,
   cv_ridge_day4,opt_lambda,ridge_tuned)

#ridge - institution_day5
#5일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_institution_day5 <- cbind(y3_day5, x_day5) %>% as.data.frame()
X_institution_scale_day5 <- x_day5 %>% scale
y3_scale_day5 <- y3_day5 - mean(y3_day5)
dataset_institution_scale_day5 <- cbind(y3_day5, X_institution_scale_day5)
ridge_institution_day5 <- glmnet(x = X_institution_scale_day5, y = y3_scale_day5, alpha = 0)
set.seed(1234)
cv_ridge_day5 <- cv.glmnet(x = X_institution_scale_day5, y = y3_scale_day5, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day5$lambda.min  
ridge_tuned <- glmnet(x = X_institution_scale_day5, y = y3_scale_day5, alpha = 0, lambda = opt_lambda)
coef_ridge_institution_day5 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_institution_scale_day5,y3_scale_day5,dataset_institution_scale_day5,ridge_institution_day5,
   cv_ridge_day5,opt_lambda,ridge_tuned)

#망각곡선 고려 안 했을때 - institution
coef_ridge_institution_noforget <- (coef_ridge_institution_day0 + coef_ridge_institution_day3 + 
                                      coef_ridge_institution_day2 + coef_ridge_institution_day3 + 
                                      coef_ridge_institution_day4 + coef_ridge_institution_day5)
pos_ridge_institution_noforget <- coef_ridge_institution_noforget[coef_ridge_institution_noforget > 0]
neg_ridge_institution_noforget <- coef_ridge_institution_noforget[coef_ridge_institution_noforget < 0]
write.csv(pos_ridge_institution_noforget, 'pos_ridge_institution_noforget.csv', row.names = T)
write.csv(neg_ridge_institution_noforget, 'neg_ridge_institution_noforget.csv', row.names = T)
rm(coef_ridge_institution_noforget,pos_ridge_institution_noforget,neg_ridge_institution_noforget)

#institution_forget curve
forgetting <- function(x) {
  1.84 / (log10(x)^1.25 + 1.84)
}
totalsum<- sum(forgetting(1),forgetting(1*60*24*1),forgetting(1*60*24*2),forgetting(1*60*24*3),
               forgetting(1*60*24*4))
#total sum을 통해서 그 합이 1이 되게 해준다. 
coef_ridge_institution <- (coef_ridge_institution_day0 *forgetting(1) + 
                             coef_ridge_institution_day3 * forgetting(1*60*24*1)  + coef_ridge_institution_day2 * forgetting(1*60*24*2)  + 
                             coef_ridge_institution_day3 * forgetting(1*60*24*3)  + 
                             coef_ridge_institution_day4 * forgetting(1*60*24*4)  + 
                             coef_ridge_institution_day5 * forgetting(1*60*24*5))

#망각 곡선을 곱해주어 시간에 대한 지속성을 고려한 회귀계수를 도출한다.
pos_ridge_institution <- coef_ridge_institution[coef_ridge_institution > 0]
#회귀계수가 0 이상이면 긍정적인 단어로!
neg_ridge_institution <- coef_ridge_institution[coef_ridge_institution < 0]
#회귀계수가 0보다 작으면 부정적인 단어로!
write.csv(pos_ridge_institution, 'pos_ridge_institution.csv', row.names = T)
write.csv(neg_ridge_institution, 'neg_ridge_institution.csv', row.names = T)
#결과적으로 개인 투자자의 순 거래대금에 대한 긍정 사전과 부정 사전을 
#만들어낼수 있었다.
rm(coef_ridge_institution, coef_ridge_institution_day0, coef_ridge_institution_day3, 
   coef_ridge_institution_day2, coef_ridge_institution_day3, coef_ridge_institution_day4,
   coef_ridge_institution_day5,y3_day3, y3_day2, y3_day3,y3_day4,y3_day5)



#############################################################################
#-------------------------이제 회귀식을... 돌려볼까 - others
#ridge - others_day0
#당일 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_others_scale_day0 <- x_day0 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y4_scale_day0 <- y4_day0 - mean(y4_day0) 
dataset_others_scale_day0 <- cbind(y4_day0, X_others_scale_day0)
ridge_others_day0 <- glmnet(x = X_others_scale_day0, y = y4_scale_day0, alpha = 0)
plot(ridge_others_day0, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day0 <- cv.glmnet(x = X_others_scale_day0, y = y4_scale_day0, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)

#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day0$lambda.min  
ridge_tuned <- glmnet(x = X_others_scale_day0, y = y4_scale_day0, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_others_day0 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_others_scale_day0,y4_scale_day0,dataset_others_scale_day0,ridge_others_day0,
   cv_ridge_day0,opt_lambda,ridge_tuned)

#ridge - others_day4
#1일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
library(glmnet)
X_others_scale_day4 <- x_day4 %>% scale #Ridge regression에서는 scaling을 해줘야 한다.
y4_scale_day4 <- y4_day4 - mean(y4_day4) 
dataset_others_scale_day4 <- cbind(y4_day4, X_others_scale_day4)
ridge_others_day4 <- glmnet(x = X_others_scale_day4, y = y4_scale_day4, alpha = 0)
#plot(ridge_others_day4, xvar = "lambda", label = T)
set.seed(1234)
cv_ridge_day4 <- cv.glmnet(x = X_others_scale_day4, y = y4_scale_day4, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
#CV방법을 이용하여 최적의 람다값을 찾는다.
opt_lambda <- cv_ridge_day4$lambda.min  
ridge_tuned <- glmnet(x = X_others_scale_day4, y = y4_scale_day4, alpha = 0, lambda = opt_lambda)
#최적 람다로 릿지 회귀식을 적합한다.
coef_ridge_others_day4 <- coef(ridge_tuned, s = "lambda.min")[,1]
#이때의 회귀계수와 단어를 저장해준다.
rm(X_others_scale_day4,y4_scale_day4,dataset_others_scale_day4,ridge_others_day4,
   cv_ridge_day4,opt_lambda,ridge_tuned)

#ridge - others_day2
#2일 뒤의 순거래대금과 적합을 시키는 회귀식을 도출.
dataset_others_day2 <- cbind(y4_day2, x_day2) %>% as.data.frame()
X_others_scale_day2 <- x_day2 %>% scale
y4_scale_day2 <- y4_day2 - mean(y4_day2)
dataset_others_scale_day2 <- cbind(y4_day2, X_others_scale_day2)
ridge_others_day2 <- glmnet(x = X_others_scale_day2, y = y4_scale_day2, alpha = 0)
set.seed(1234)
cv_ridge_day2 <- cv.glmnet(x = X_others_scale_day2, y = y4_scale_day2, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day2$lambda.min  
ridge_tuned <- glmnet(x = X_others_scale_day2, y = y4_scale_day2, alpha = 0, lambda = opt_lambda)
coef_ridge_others_day2 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_others_scale_day2,y4_scale_day2,dataset_others_scale_day2,ridge_others_day2,
   cv_ridge_day2,opt_lambda,ridge_tuned)

#ridge - others_day3
#3일 뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_others_day3 <- cbind(y4_day3, x_day3) %>% as.data.frame()
X_others_scale_day3 <- x_day3 %>% scale
y4_scale_day3 <- y4_day3 - mean(y4_day3)
dataset_others_scale_day3 <- cbind(y4_day3, X_others_scale_day3)
ridge_others_day3 <- glmnet(x = X_others_scale_day3, y = y4_scale_day3, alpha = 0)
set.seed(1234)
cv_ridge_day3 <- cv.glmnet(x = X_others_scale_day3, y = y4_scale_day3, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day3$lambda.min  
ridge_tuned <- glmnet(x = X_others_scale_day3, y = y4_scale_day3, alpha = 0, lambda = opt_lambda)
coef_ridge_others_day3 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_others_scale_day3,y4_scale_day3,dataset_others_scale_day3,ridge_others_day3,
   cv_ridge_day3,opt_lambda,ridge_tuned)

#ridge - others_day4
#4일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_others_day4 <- cbind(y4_day4, x_day4) %>% as.data.frame()
X_others_scale_day4 <- x_day4 %>% scale
y4_scale_day4 <- y4_day4 - mean(y4_day4)
dataset_others_scale_day4 <- cbind(y4_day4, X_others_scale_day4)
ridge_others_day4 <- glmnet(x = X_others_scale_day4, y = y4_scale_day4, alpha = 0)
set.seed(1234)
cv_ridge_day4 <- cv.glmnet(x = X_others_scale_day4, y = y4_scale_day4, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day4$lambda.min  
ridge_tuned <- glmnet(x = X_others_scale_day4, y = y4_scale_day4, alpha = 0, lambda = opt_lambda)
coef_ridge_others_day4 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_others_scale_day4,y4_scale_day4,dataset_others_scale_day4,ridge_others_day4,
   cv_ridge_day4,opt_lambda,ridge_tuned)

#ridge - others_day5
#5일뒤의 순 거래대금과 적합을 시키는 회귀식을 도출.
dataset_others_day5 <- cbind(y4_day5, x_day5) %>% as.data.frame()
X_others_scale_day5 <- x_day5 %>% scale
y4_scale_day5 <- y4_day5 - mean(y4_day5)
dataset_others_scale_day5 <- cbind(y4_day5, X_others_scale_day5)
ridge_others_day5 <- glmnet(x = X_others_scale_day5, y = y4_scale_day5, alpha = 0)
set.seed(1234)
cv_ridge_day5 <- cv.glmnet(x = X_others_scale_day5, y = y4_scale_day5, alpha = 0, 
                           nfolds = 5, type.measure = 'mse', intercept = F, grouped = FALSE)
opt_lambda <- cv_ridge_day5$lambda.min  
ridge_tuned <- glmnet(x = X_others_scale_day5, y = y4_scale_day5, alpha = 0, lambda = opt_lambda)
coef_ridge_others_day5 <- coef(ridge_tuned, s = "lambda.min")[,1]
rm(X_others_scale_day5,y4_scale_day5,dataset_others_scale_day5,ridge_others_day5,
   cv_ridge_day5,opt_lambda,ridge_tuned)

#망각곡선 고려 안 했을때 - others
coef_ridge_others_noforget <- (coef_ridge_others_day0 + coef_ridge_others_day4 + 
                                 coef_ridge_others_day2 + coef_ridge_others_day3 + 
                                 coef_ridge_others_day4 + coef_ridge_others_day5)
pos_ridge_others_noforget <- coef_ridge_others_noforget[coef_ridge_others_noforget > 0]
neg_ridge_others_noforget <- coef_ridge_others_noforget[coef_ridge_others_noforget < 0]
write.csv(pos_ridge_others_noforget, 'pos_ridge_others_noforget.csv', row.names = T)
write.csv(neg_ridge_others_noforget, 'neg_ridge_others_noforget.csv', row.names = T)
rm(coef_ridge_others_noforget,pos_ridge_others_noforget,neg_ridge_others_noforget)

#others_forget curve
forgetting <- function(x) {
  1.84 / (log10(x)^1.25 + 1.84)
}
totalsum<- sum(forgetting(1),forgetting(1*60*24*1),forgetting(1*60*24*2),forgetting(1*60*24*3),
               forgetting(1*60*24*4))
#total sum을 통해서 그 합이 1이 되게 해준다. 
coef_ridge_others <- (coef_ridge_others_day0 *forgetting(1) + 
                        coef_ridge_others_day4 * forgetting(1*60*24*1)  + coef_ridge_others_day2 * forgetting(1*60*24*2)  + 
                        coef_ridge_others_day3 * forgetting(1*60*24*3)  + 
                        coef_ridge_others_day4 * forgetting(1*60*24*4)  + 
                        coef_ridge_others_day5 * forgetting(1*60*24*5))

#망각 곡선을 곱해주어 시간에 대한 지속성을 고려한 회귀계수를 도출한다.
pos_ridge_others <- coef_ridge_others[coef_ridge_others > 0]
#회귀계수가 0 이상이면 긍정적인 단어로!
neg_ridge_others <- coef_ridge_others[coef_ridge_others < 0]
#회귀계수가 0보다 작으면 부정적인 단어로!
write.csv(pos_ridge_others, 'pos_ridge_others.csv', row.names = T)
write.csv(neg_ridge_others, 'neg_ridge_others.csv', row.names = T)
#결과적으로 개인 투자자의 순 거래대금에 대한 긍정 사전과 부정 사전을 
#만들어낼수 있었다.
rm(coef_ridge_others, coef_ridge_others_day0,coef_ridge_others_day4, coef_ridge_others_day2,
   coef_ridge_others_day3, coef_ridge_others_day4, coef_ridge_others_day5,y4_day4, y4_day2, y4_day3,y4_day4,y4_day5)

library(readr) 
library(data.table)
library(dplyr)
library(tm)
library(rJava)
library(KoNLP)
library(stringr)
library(reshape2)
library(KoNLP)
useSystemDic()
library(tm.plugin.sentiment)
library(devtools)

#우선 데이터를 불러오자.
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터/뉴스크롤링')
rm(list = ls())
datedata <- fread('article_merged.csv')
data_article <- datedata$date
rm(datedata)
basedata <- fread('HyengTaeSo.csv')
basedata$L1 <-  gsub(basedata$L1,pattern = "article",replacement =  '') %>% as.numeric
basedata <- basedata %>% arrange(L1) 
basedata$L1 <- basedata$L1 %>% as.factor()

#individual의 감성을 분석해보자.
pos_individual <- fread('pos_ridge_individual.csv')
neg_individual <- fread('neg_ridge_individual.csv')
#만들어 둔 개인 투자자에 대한 감성 사전을가져온다.
individual_score <- basedata
individual_dic <- rbind(pos_individual, neg_individual)
m_individual <- match(individual_score$words, individual_dic$V1)
#해당 날짜의 뉴스 기사에 사전의 단어가 있다면 사전에서의 점수를
#넣어주고,
individual_score$score_individual <- individual_dic$x[m_individual]
#그 날에 감성사전에 있으면 이것을 score로 뽑아주고 
individual_score$score_individual[is.na(individual_score$score_individual)] <- 0
individual_senti <- individual_score %>% group_by(L1) %>% summarize(score = sum(score_individual))
#그 날에 추출된 단어에 대한 사전의 점수를 모두 더해주면 
#개인 감성사전에 기반한 그 날의 신문 감성점수를 추출할 수 있다.

#위와 같은 과정을 외국인, 기관, 기타법인에 대해 반복해주면
#각 투자자별로 뉴스에 반응한 주식 감성점수를 추출할 수 있다.

#foreign의 감성을 분석해보자.
pos_foreign <- fread('pos_ridge_foreign.csv')
neg_foreign <- fread('neg_ridge_foreign.csv')
foreign_score <- basedata
foreign_dic <- rbind(pos_foreign, neg_foreign)
m_foreign <- match(foreign_score$words, foreign_dic$V1)
foreign_score$score_foreign <- foreign_dic$x[m_foreign]
foreign_score$score_foreign[is.na(foreign_score$score_foreign)] <- 0
foreign_senti <- foreign_score %>% group_by(L1) %>% summarize(score = sum(score_foreign))

#institution의 감성을 분석해보자.
pos_institution <- fread('pos_ridge_institution.csv')
neg_institution <- fread('neg_ridge_institution.csv')
institution_score <- basedata
institution_dic <- rbind(pos_institution, neg_institution)
m_institution <- match(institution_score$words, institution_dic$V1)
institution_score$score_institution <- institution_dic$x[m_institution]
institution_score$score_institution[is.na(institution_score$score_institution)] <- 0
institution_senti <- institution_score %>% group_by(L1) %>% summarize(score = sum(score_institution))

#others의 감성을 분석해보자.
pos_others <- fread('pos_ridge_others.csv')
neg_others <- fread('neg_ridge_others.csv')
others_score <- basedata
others_dic <- rbind(pos_others, neg_others)
m_others <- match(others_score$words, others_dic$V1)
others_score$score_others <- others_dic$x[m_others]
others_score$score_others[is.na(others_score$score_others)] <- 0
others_senti <- others_score %>% group_by(L1) %>% summarize(score = sum(score_others))


sentiment <- cbind(individual_senti$score, foreign_senti$score, institution_senti$score, others_senti$score)
colnames(sentiment) <- c('individual', 'foreign', 'institution','others')
sentiment <- cbind(data_article ,sentiment)
write.csv(sentiment,'sentiment.csv', row.names = F)
#결과적으로 신문 기사에 대한 감성 점수를 뽑아낼 수 있다. 


#----------------------no considering forgetting curve
rm(list = ls())
#우선 데이터를 불러오자.
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터/뉴스크롤링')
rm(list = ls())
article <- fread('HyengTaeSo_merged.csv')
datedata <- fread('article_merged.csv')
data_article <- datedata$date
rm(datedata)
basedata <- fread('HyengTaeSo.csv')
basedata$L1 <-  gsub(basedata$L1,pattern = "article",replacement =  '') %>% as.numeric
basedata <- basedata %>% arrange(L1) 
basedata$L1 <- basedata$L1 %>% as.factor()

#individual의 감성을 분석해보자.
pos_individual <- fread('pos_ridge_individual_noforget.csv')
neg_individual <- fread('neg_ridge_individual_noforget.csv')
#만들어 둔 개인 투자자에 대한 감성 사전을가져온다.
individual_score <- basedata
individual_dic <- rbind(pos_individual, neg_individual)
m_individual <- match(individual_score$words, individual_dic$V1)
#해당 날짜의 뉴스 기사에 사전의 단어가 있다면 사전에서의 점수를
#넣어주고,
individual_score$score_individual <- individual_dic$x[m_individual]
#그 날에 감성사전에 있으면 이것을 score로 뽑아주고 
individual_score$score_individual[is.na(individual_score$score_individual)] <- 0
individual_senti <- individual_score %>% group_by(L1) %>% summarize(score = sum(score_individual))
#그 날에 추출된 단어에 대한 사전의 점수를 모두 더해주면 
#개인 감성사전에 기반한 그 날의 신문 감성점수를 추출할 수 있다.

#위와 같은 과정을 외국인, 기관, 기타법인에 대해 반복해주면
#각 투자자별로 뉴스에 반응한 주식 감성점수를 추출할 수 있다.

#foreign의 감성을 분석해보자.
pos_foreign <- fread('pos_ridge_foreign_noforget.csv')
neg_foreign <- fread('neg_ridge_foreign_noforget.csv')
foreign_score <- basedata
foreign_dic <- rbind(pos_foreign, neg_foreign)
m_foreign <- match(foreign_score$words, foreign_dic$V1)
foreign_score$score_foreign <- foreign_dic$x[m_foreign]
foreign_score$score_foreign[is.na(foreign_score$score_foreign)] <- 0
foreign_senti <- foreign_score %>% group_by(L1) %>% summarize(score = sum(score_foreign))

#institution의 감성을 분석해보자.
pos_institution <- fread('pos_ridge_institution_noforget.csv')
neg_institution <- fread('neg_ridge_institution_noforget.csv')
institution_score <- basedata
institution_dic <- rbind(pos_institution, neg_institution)
m_institution <- match(institution_score$words, institution_dic$V1)
institution_score$score_institution <- institution_dic$x[m_institution]
institution_score$score_institution[is.na(institution_score$score_institution)] <- 0
institution_senti <- institution_score %>% group_by(L1) %>% summarize(score = sum(score_institution))

#others의 감성을 분석해보자.
pos_others <- fread('pos_ridge_others_noforget.csv')
neg_others <- fread('neg_ridge_others_noforget.csv')
others_score <- basedata
others_dic <- rbind(pos_others, neg_others)
m_others <- match(others_score$words, others_dic$V1)
others_score$score_others <- others_dic$x[m_others]
others_score$score_others[is.na(others_score$score_others)] <- 0
others_senti <- others_score %>% group_by(L1) %>% summarize(score = sum(score_others))


sentiment <- cbind(individual_senti$score, foreign_senti$score, institution_senti$score, others_senti$score)
colnames(sentiment) <- c('individual', 'foreign', 'institution','others')
sentiment <- cbind(data_article ,sentiment)
write.csv(sentiment,'sentiment_noforget.csv', row.names = F)
#결과적으로 신문 기사에 대한 감성 점수를 뽑아낼 수 있다. 


library(readr) 
library(data.table)
library(dplyr)
library(tm)
library(rJava)
library(KoNLP)
library(stringr)
library(reshape2)
library(KoNLP)
useSystemDic()
library(tm.plugin.sentiment)
library(devtools)

#우선 데이터를 불러오자.
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터/뉴스크롤링')
rm(list = ls())
article <- fread('HyengTaeSo_merged.csv')
datedata <- fread('article_merged.csv')
data_article <- datedata$date
rm(datedata)
basedata <- fread('HyengTaeSo.csv')
basedata$L1 <-  gsub(basedata$L1,pattern = "article",replacement =  '') %>% as.numeric
basedata <- basedata %>% arrange(L1) 
basedata$L1 <- basedata$L1 %>% as.factor()

#individual의 감성을 분석해보자.
pos_individual <- fread('pos_ridge_individual.csv')
neg_individual <- fread('neg_ridge_individual.csv')
#만들어 둔 개인 투자자에 대한 감성 사전을가져온다.
individual_score <- basedata
individual_dic <- rbind(pos_individual, neg_individual)
m_individual <- match(individual_score$words, individual_dic$V1)
#해당 날짜의 뉴스 기사에 사전의 단어가 있다면 사전에서의 점수를
#넣어주고,
individual_score$score_individual <- individual_dic$x[m_individual]
#그 날에 감성사전에 있으면 이것을 score로 뽑아주고 
individual_score$score_individual[is.na(individual_score$score_individual)] <- 0
individual_senti <- individual_score %>% group_by(L1) %>% summarize(score = sum(score_individual))
#그 날에 추출된 단어에 대한 사전의 점수를 모두 더해주면 
#개인 감성사전에 기반한 그 날의 신문 감성점수를 추출할 수 있다.

#위와 같은 과정을 외국인, 기관, 기타법인에 대해 반복해주면
#각 투자자별로 뉴스에 반응한 주식 감성점수를 추출할 수 있다.

#foreign의 감성을 분석해보자.
pos_foreign <- fread('pos_ridge_foreign.csv')
neg_foreign <- fread('neg_ridge_foreign.csv')
foreign_score <- basedata
foreign_dic <- rbind(pos_foreign, neg_foreign)
m_foreign <- match(foreign_score$words, foreign_dic$V1)
foreign_score$score_foreign <- foreign_dic$x[m_foreign]
foreign_score$score_foreign[is.na(foreign_score$score_foreign)] <- 0
foreign_senti <- foreign_score %>% group_by(L1) %>% summarize(score = sum(score_foreign))

#institution의 감성을 분석해보자.
pos_institution <- fread('pos_ridge_institution.csv')
neg_institution <- fread('neg_ridge_institution.csv')
institution_score <- basedata
institution_dic <- rbind(pos_institution, neg_institution)
m_institution <- match(institution_score$words, institution_dic$V1)
institution_score$score_institution <- institution_dic$x[m_institution]
institution_score$score_institution[is.na(institution_score$score_institution)] <- 0
institution_senti <- institution_score %>% group_by(L1) %>% summarize(score = sum(score_institution))

#others의 감성을 분석해보자.
pos_others <- fread('pos_ridge_others.csv')
neg_others <- fread('neg_ridge_others.csv')
others_score <- basedata
others_dic <- rbind(pos_others, neg_others)
m_others <- match(others_score$words, others_dic$V1)
others_score$score_others <- others_dic$x[m_others]
others_score$score_others[is.na(others_score$score_others)] <- 0
others_senti <- others_score %>% group_by(L1) %>% summarize(score = sum(score_others))


sentiment <- cbind(individual_senti$score, foreign_senti$score, institution_senti$score, others_senti$score)
colnames(sentiment) <- c('individual', 'foreign', 'institution','others')
sentiment <- cbind(data_article ,sentiment)
write.csv(sentiment,'sentiment.csv', row.names = F)
#결과적으로 신문 기사에 대한 감성 점수를 뽑아낼 수 있다. 







#----------------------no considering forgetting curve
rm(list = ls())
#우선 데이터를 불러오자.
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터/뉴스크롤링')
rm(list = ls())
article <- fread('HyengTaeSo_merged.csv')
datedata <- fread('article_merged.csv')
data_article <- datedata$date
rm(datedata)
basedata <- fread('HyengTaeSo.csv')
basedata$L1 <-  gsub(basedata$L1,pattern = "article",replacement =  '') %>% as.numeric
basedata <- basedata %>% arrange(L1) 
basedata$L1 <- basedata$L1 %>% as.factor()

#individual의 감성을 분석해보자.
pos_individual <- fread('pos_ridge_individual_noforget.csv')
neg_individual <- fread('neg_ridge_individual_noforget.csv')
#만들어 둔 개인 투자자에 대한 감성 사전을가져온다.
individual_score <- basedata
individual_dic <- rbind(pos_individual, neg_individual)
m_individual <- match(individual_score$words, individual_dic$V1)
#해당 날짜의 뉴스 기사에 사전의 단어가 있다면 사전에서의 점수를
#넣어주고,
individual_score$score_individual <- individual_dic$x[m_individual]
#그 날에 감성사전에 있으면 이것을 score로 뽑아주고 
individual_score$score_individual[is.na(individual_score$score_individual)] <- 0
individual_senti <- individual_score %>% group_by(L1) %>% summarize(score = sum(score_individual))
#그 날에 추출된 단어에 대한 사전의 점수를 모두 더해주면 
#개인 감성사전에 기반한 그 날의 신문 감성점수를 추출할 수 있다.

#위와 같은 과정을 외국인, 기관, 기타법인에 대해 반복해주면
#각 투자자별로 뉴스에 반응한 주식 감성점수를 추출할 수 있다.

#foreign의 감성을 분석해보자.
pos_foreign <- fread('pos_ridge_foreign_noforget.csv')
neg_foreign <- fread('neg_ridge_foreign_noforget.csv')
foreign_score <- basedata
foreign_dic <- rbind(pos_foreign, neg_foreign)
m_foreign <- match(foreign_score$words, foreign_dic$V1)
foreign_score$score_foreign <- foreign_dic$x[m_foreign]
foreign_score$score_foreign[is.na(foreign_score$score_foreign)] <- 0
foreign_senti <- foreign_score %>% group_by(L1) %>% summarize(score = sum(score_foreign))

#institution의 감성을 분석해보자.
pos_institution <- fread('pos_ridge_institution_noforget.csv')
neg_institution <- fread('neg_ridge_institution_noforget.csv')
institution_score <- basedata
institution_dic <- rbind(pos_institution, neg_institution)
m_institution <- match(institution_score$words, institution_dic$V1)
institution_score$score_institution <- institution_dic$x[m_institution]
institution_score$score_institution[is.na(institution_score$score_institution)] <- 0
institution_senti <- institution_score %>% group_by(L1) %>% summarize(score = sum(score_institution))

#others의 감성을 분석해보자.
pos_others <- fread('pos_ridge_others_noforget.csv')
neg_others <- fread('neg_ridge_others_noforget.csv')
others_score <- basedata
others_dic <- rbind(pos_others, neg_others)
m_others <- match(others_score$words, others_dic$V1)
others_score$score_others <- others_dic$x[m_others]
others_score$score_others[is.na(others_score$score_others)] <- 0
others_senti <- others_score %>% group_by(L1) %>% summarize(score = sum(score_others))


sentiment <- cbind(individual_senti$score, foreign_senti$score, institution_senti$score, others_senti$score)
colnames(sentiment) <- c('individual', 'foreign', 'institution','others')
sentiment <- cbind(data_article ,sentiment)
write.csv(sentiment,'sentiment_noforget.csv', row.names = F)
#결과적으로 신문 기사에 대한 감성 점수를 뽑아낼 수 있다. 

#######################################################################################################
#######################################################################################################
#########################################################################################################
#######XGBOOST
##########################################################################################################
############################################################################################################
########################################################################################################


library(dplyr)
library(data.table)
library(caret)
library(xgboost)
library(rBayesianOptimization)
library(MLmetrics)
library(sdcTable)
library(dummies)
library(Metrics)
library(rBayesianOptimization)
options(scipen = 100)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[1]거시경제 지표만 고려한 경우.
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 


FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model1 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model1 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model1, 'opt_model1.csv', row.names = T)
write.csv(result_model1, 'result_model1.csv', row.names = T)

library(dplyr)
library(data.table)
library(caret)
library(xgboost)
library(rBayesianOptimization)
library(MLmetrics)
library(sdcTable)
library(dummies)
library(Metrics)
library(rBayesianOptimization)
options(scipen = 100)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[2]거시지표 + 산업군(수익률)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, is.ind.1, is.fr.1, is.ins.1, is.oth.1, Y)
stock_ct_train %>% str
#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 

FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 

set.seed(1234)

opt <- fread('opt_forget.csv')
OPT_Res1 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res2 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res3 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res4 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res5 <- data.frame(Best_Par = c(0,0,0,0))

OPT_Res1$Best_Par <- opt$window1 %>% as.vector()
OPT_Res2$Best_Par <- opt$window2 %>% as.vector()
OPT_Res3$Best_Par <- opt$window3 %>% as.vector()
OPT_Res4$Best_Par <- opt$window4 %>% as.vector()
OPT_Res5$Best_Par <- opt$window5 %>% as.vector()


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model2 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model2 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model2, 'opt_model2.csv', row.names = T)
write.csv(result_model2, 'result_model2.csv', row.names = T)
rm(list = ls())


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[3]거시지표 + 산업군(수익률X시가총액) 
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, is.ind.2, is.fr.2, is.ins.2, is.oth.2, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 

FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 

set.seed(1234)

opt <- fread('opt_forget.csv')
OPT_Res1 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res2 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res3 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res4 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res5 <- data.frame(Best_Par = c(0,0,0,0))

OPT_Res1$Best_Par <- opt$window1 %>% as.vector()
OPT_Res2$Best_Par <- opt$window2 %>% as.vector()
OPT_Res3$Best_Par <- opt$window3 %>% as.vector()
OPT_Res4$Best_Par <- opt$window4 %>% as.vector()
OPT_Res5$Best_Par <- opt$window5 %>% as.vector()


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model3 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model3 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model3, 'opt_model3.csv', row.names = T)
write.csv(result_model3, 'result_model3.csv', row.names = T)
rm(list = ls())


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[4]거시지표 + 산업군(수익률X시가총액X컨센서스) 
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, is.ind.3, is.fr.3, is.ins.3, is.oth.3, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 

FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 

set.seed(1234)

opt <- fread('opt_forget.csv')
OPT_Res1 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res2 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res3 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res4 <- data.frame(Best_Par = c(0,0,0,0))
OPT_Res5 <- data.frame(Best_Par = c(0,0,0,0))

OPT_Res1$Best_Par <- opt$window1 %>% as.vector()
OPT_Res2$Best_Par <- opt$window2 %>% as.vector()
OPT_Res3$Best_Par <- opt$window3 %>% as.vector()
OPT_Res4$Best_Par <- opt$window4 %>% as.vector()
OPT_Res5$Best_Par <- opt$window5 %>% as.vector()


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model4 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model4 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model4, 'opt_model4.csv', row.names = T)
write.csv(result_model4, 'result_model4.csv', row.names = T)



library(dplyr)
library(data.table)
library(caret)
library(xgboost)
library(rBayesianOptimization)
library(MLmetrics)
library(sdcTable)
library(dummies)
library(Metrics)
library(rBayesianOptimization)
options(scipen = 100)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[5]5. 거시지표 + 산업군(수익률) + 심리(망각 O)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, is.ind.1, is.fr.1, is.oth.1, is.ins.1, senti_for, 
                                            senti_ind, senti_insti, senti_other, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 


FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model5 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model5 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model5, 'opt_model5.csv', row.names = T)
write.csv(result_model5, 'result_model5.csv', row.names = T)



##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[6]거시지표 + 산업군(수익률) + 심리(망각 X)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
stock <- fread('final_data_copy.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, is.ind.1, is.fr.1, is.oth.1, is.ins.1, senti_for_nf, 
                                            senti_ind_nf, senti_insti_nf, senti_other_nf, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 


FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model6 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model6 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model6, 'opt_model6_copy.csv', row.names = T)
write.csv(result_model6, 'result_model6_copy.csv', row.names = T)
rm(list = ls())

library(dplyr)
library(data.table)
library(caret)
library(xgboost)
library(rBayesianOptimization)
library(MLmetrics)
library(sdcTable)
library(dummies)
library(Metrics)
library(rBayesianOptimization)
options(scipen = 100)

######################################################################################################################
######################################################################################################################
#[7]############거시지표 + 산업군(수익률X시가총액) + 심리(망각 O)
######################################################################################################################
######################################################################################################################
######################################################################################################################


stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ms.ind, ms.fr, ms.ins, ms.oth, is.ind.2, is.fr.2, is.ins.2, is.oth.2, senti_for,
                                            senti_ind, senti_insti, senti_other, ind.Y, fr.Y, ins.Y, oth.Y, Y)
stock_ct_train %>% str

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 

FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 

#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model7 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model7 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model7, 'opt_model7.csv', row.names = T)
write.csv(result_model7, 'result_model7.csv', row.names = T)
rm(list = ls())




######################################################################################################################
######################################################################################################################
#[8]############거시지표 + 산업군(수익률X시가총액) + 심리(망각 X)
######################################################################################################################
######################################################################################################################
######################################################################################################################

stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ms.ind, ms.fr, ms.ins, ms.oth, is.ind.2, is.fr.2, is.ins.2, is.oth.2, senti_for_nf,
                                            senti_ind_nf, senti_insti_nf, senti_other_nf, ind.Y, fr.Y, ins.Y, oth.Y, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 

FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 

set.seed(1234)

#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model8 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model8 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model8, 'opt_model8.csv', row.names = T)
write.csv(result_model8, 'result_model8.csv', row.names = T)




##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[9]거시지표 + 산업군(수익률X시가총액X컨센서스) + 심리(망각 O)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, is.ind.3, is.fr.3, is.oth.3, is.ins.3, senti_for, 
                                            senti_ind, senti_insti, senti_other, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 


FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model9 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                   window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model9 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                      accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                      accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                      f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                      mean_acc =
                        mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                               accuracy5$overall[1])), 
                      mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model9, 'opt_model9.csv', row.names = T)
write.csv(result_model9, 'result_model9.csv', row.names = T)
rm(list = ls())

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[10]거시지표 + 산업군(수익률X시가총액X컨센서스) + 심리(망각 X)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
stock <- fread('final_data.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)
stock_ct_train <- stock_ct_train %>% select(ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, is.ind.3, is.fr.3, is.oth.3, is.ins.3, senti_for_nf, 
                                            senti_ind_nf, senti_insti_nf, senti_other_nf, Y)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 


FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model10 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                    window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model10 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                       accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                       accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                       f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                       mean_acc =
                         mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                                accuracy5$overall[1])), 
                       mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model10, 'opt_model10.csv', row.names = T)
write.csv(result_model10, 'result_model10.csv', row.names = T)




##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[11]거시지표 + 산업군(수익률X시가총액) + 심리 + 변동성지수
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#[1]----------20일 이평선과 60일 이평선 차이만 사용해서 모델링
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
stock <- fread('final_data_real_train_6_v02.csv') #Train데이터를 가져온다. 
stock_ct <- stock  %>% select(-date) %>% as.data.frame()
stock_ct$Y <- ifelse(stock_ct$Y == '상승', 1, 0) #상승 하락을 숫자로 넣어줘야 xgb를 적합시킬 있다.
stock_ct_train <- stock_ct

#[2]----------그럼 망각곡선 넣은거랑 안 넣은거랑 다르게 해야한다.
colnames(stock_ct_train)

#----------------Matrix를 만들자
trainmat <-  xgb.DMatrix(as.matrix(stock_ct_train %>% select(-Y)), label = stock_ct_train$Y) #y값in을 빼주고 label에 넣어야함

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 


FOLDS <-  list()
FOLDS$Ftest1 = 1:1944
FOLDS$Ftest2 = 383:2326
FOLDS$Ftest3 = 765:2708
FOLDS$Ftest4 = 1147:3090
FOLDS$Ftest5 = 1529:3472

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = 1945:2431
FOLDS_TEST$Fold2 = 2327:2813
FOLDS_TEST$Fold3 = 2709:3195
FOLDS_TEST$Fold4 = 3091:3577
FOLDS_TEST$Fold5 = 3473:3958

####------------베이지안 파라미터 찾기
set.seed(1234)
#xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
# cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                           max_depth = max.depth,
#                          min_child_weight = min_child_weight,
#                         subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
#          data = trainmat, nround = 1000,
#         folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
#        early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) 
#list(Score = cv$evaluation_log[,max(test_auc_mean)],
#     Pred = cv$pred)
#}


#validation에 대한 accuracy를 봐보자.

trainmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest1,]$Y)
label1 <- stock_ct_train[FOLDS$Ftest1,]$Y
trainmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest2,]$Y)
label2 <- stock_ct_train[FOLDS$Ftest2,]$Y
trainmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest3,]$Y)
label3 <- stock_ct_train[FOLDS$Ftest3,]$Y
trainmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest4,]$Y)
label4 <- stock_ct_train[FOLDS$Ftest4,]$Y
trainmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS$Ftest5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS$Ftest5,]$Y)
label5 <- stock_ct_train[FOLDS$Ftest5,]$Y
testmat1 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold1,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold1,]$Y)
test1 <- stock_ct_train[FOLDS_TEST$Fold1,]$Y
testmat2 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold2,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold2,]$Y)
test2 <- stock_ct_train[FOLDS_TEST$Fold2,]$Y
testmat3 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold3,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold3,]$Y)
test3 <- stock_ct_train[FOLDS_TEST$Fold3,]$Y
testmat4 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold4,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold4,]$Y)
test4 <- stock_ct_train[FOLDS_TEST$Fold4,]$Y
testmat5 <-  xgb.DMatrix(as.matrix(stock_ct_train[FOLDS_TEST$Fold5,-which(colnames(stock_ct_train) == 'Y')]), label = stock_ct_train[FOLDS_TEST$Fold5,]$Y)
test5 <- stock_ct_train[FOLDS_TEST$Fold5,]$Y

#아래와 같이 함수를 짜고,
#_1번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_1 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_1 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_1 <- predict(bayes_fit_1, testmat1)
  x <- ifelse(pred_1 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_1$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res1 <- BayesianOptimization(xgb_cv_bayes_1,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res1
# max.depth min_child_weight        subsample        colsample 
#    6.0              1.0              0.8              1.0

#_2번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_2 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_2 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_2 <- predict(bayes_fit_2, testmat2)
  x <- ifelse(pred_2 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_2$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res2 <- BayesianOptimization(xgb_cv_bayes_2,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res2
# max.depth    min_child_weight        subsample        colsample 
#  6.0000000      1.0000000            0.7935487        1.0000000 

#_3번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_3 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_3 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_3 <- predict(bayes_fit_3, testmat3)
  x <- ifelse(pred_3 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_3$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res3 <- BayesianOptimization(xgb_cv_bayes_3,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res3
#max.depth    min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7173724        1.0000000 

#_4번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_4 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_4 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_4 <- predict(bayes_fit_4, testmat4)
  x <- ifelse(pred_4 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_4$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res4 <- BayesianOptimization(xgb_cv_bayes_4,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res4
#max.depth     min_child_weight        subsample        colsample 
# 6.0000000        1.0000000        0.7083429        1.0000000 

#_5번 롤링윈도우
set.seed(1234)
xgb_cv_bayes_5 <- function(max.depth, min_child_weight, subsample, colsample) {
  bayes_fit_5 <- xgboost(data = trainmat1, label = label1, max_depth = max.depth,
                         eta = 0.01,  min_child_weight = min_child_weight,
                         subsample = subsample, colsample_bytree = colsample,
                         nrounds = 1000, objective = "binary:logistic", eval_metric = "auc", verbose = 0,
                         early_stopping_rounds = 200)
  pred_5 <- predict(bayes_fit_5, testmat5)
  x <- ifelse(pred_5 >= 0.5, '1', "0") %>% as.factor()
  list(Score = bayes_fit_5$evaluation_log[,max(train_auc)],
       Pred = x)
}
OPT_Res5 <- BayesianOptimization(xgb_cv_bayes_5,
                                 bounds = list(max.depth = c(2L, 6L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 0.8), colsample = c(0.5, 1)),
                                 init_grid_dt = NULL, init_points = 10, n_iter = 30,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)
#OPT_Res5
#max.depth     min_child_weight        subsample        colsample 
#6.0000000        1.0000000        0.7964781        0.9417423 


#첫번째 롤링윈도우.
set.seed(1234)
xgfit1 <- xgboost(data = trainmat1, label = label1, max_depth = OPT_Res1$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res1$Best_Par[2],
                  subsample = OPT_Res1$Best_Par[3], colsample_bytree = OPT_Res1$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred1 <- predict(xgfit1, testmat1)
x1 <- ifelse(pred1 >= 0.5, '1', "0") %>% as.factor()
accuracy1 <- confusionMatrix(x1,test1 %>% as.factor())
f1_1 <- F1_Score(test1 %>% as.factor(), x1)

#두번째 롤링윈도우.
set.seed(1234)
xgfit2 <- xgboost(data = trainmat2, label = label2, max_depth = OPT_Res2$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res2$Best_Par[2],
                  subsample = OPT_Res2$Best_Par[3], colsample_bytree = OPT_Res2$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred2 <- predict(xgfit2, testmat2)
x2 <- ifelse(pred2 >= 0.5, '1', "0") %>% as.factor()
accuracy2 <- confusionMatrix(x2,test2 %>% as.factor())
f1_2 <- F1_Score(test2 %>% as.factor(), x2)

#세번째 롤링윈도우.
set.seed(1234)
xgfit3 <- xgboost(data = trainmat3, label = label3, max_depth = OPT_Res3$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res3$Best_Par[2],
                  subsample = OPT_Res3$Best_Par[3], colsample_bytree = OPT_Res3$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred3 <- predict(xgfit3, testmat3)
x3 <- ifelse(pred3 >= 0.5, '1', "0") %>% as.factor()
accuracy3 <- confusionMatrix(x3,test3 %>% as.factor())
f1_3 <- F1_Score(test3 %>% as.factor(), x3)

#네번째 롤링윈도우.
set.seed(1234)
xgfit4 <- xgboost(data = trainmat4, label = label4, max_depth = OPT_Res4$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res4$Best_Par[2],
                  subsample = OPT_Res4$Best_Par[3], colsample_bytree = OPT_Res4$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred4 <- predict(xgfit4, testmat4)
x4 <- ifelse(pred4 >= 0.5, '1', "0") %>% as.factor()
accuracy4 <- confusionMatrix(x4,test4 %>% as.factor())
f1_4 <- F1_Score(test4 %>% as.factor(), x4)

#다섯번째 롤링윈도우.
set.seed(1234)
xgfit5 <- xgboost(data = trainmat5, label = label5, max_depth = OPT_Res5$Best_Par[1],
                  eta = 0.01,  min_child_weight = OPT_Res5$Best_Par[2],
                  subsample = OPT_Res5$Best_Par[3], colsample_bytree = OPT_Res5$Best_Par[4],
                  nrounds = 1000, objective = "binary:logistic", eval_metric = "auc")
pred5 <- predict(xgfit5, testmat5)
x5 <- ifelse(pred5 >= 0.5, '1', "0") %>% as.factor()
accuracy5 <- confusionMatrix(x5,test5 %>% as.factor())
f1_5 <- F1_Score(test5 %>% as.factor(), x5)

#cv에 대한 평균을 찾아보자. 
mean(f1_1, f1_2, f1_3, f1_4, f1_5)
mean(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
     accuracy5$overall[1])
mean(accuracy1$byClass[1], accuracy2$byClass[1],accuracy3$byClass[1],accuracy4$byClass[1],
     accuracy5$byClass[1])
mean(accuracy1$byClass[2], accuracy2$byClass[2],accuracy3$byClass[2],accuracy4$byClass[2],
     accuracy5$byClass[2])

opt_model11 <- list(window1 = OPT_Res1$Best_Par, window2 = OPT_Res2$Best_Par, window3 = OPT_Res3$Best_Par,
                    window4 = OPT_Res4$Best_Par, window5 = OPT_Res5$Best_Par)
result_model11 <- list(accuracy1 = accuracy1$overall[1],accuracy2 =  accuracy2$overall[1],
                       accuracy3 = accuracy3$overall[1],accuracy4 = accuracy4$overall[1],
                       accuracy5 = accuracy5$overall[1], f1_1 = f1_1, 
                       f1_2 = f1_2, f1_3 = f1_3, f1_4 = f1_4, f1_5 = f1_5, 
                       mean_acc =
                         mean(c(accuracy1$overall[1], accuracy2$overall[1],accuracy3$overall[1],accuracy4$overall[1],
                                accuracy5$overall[1])), 
                       mean_f1 = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(opt_model11, 'opt_model11.csv', row.names = T)
write.csv(result_model11, 'result_model11.csv', row.names = T)


#######################################################################################################
#######################################################################################################
#########################################################################################################
#######ARIMAX
##########################################################################################################
############################################################################################################
########################################################################################################

library(dplyr)
library(forecast)
library(data.table)
library(readxl)
library(caret)
library(lubridate)
library(MLmetrics)
library(fDMA)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[1]거시경제 지표만 고려한 경우.
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
rm(list = ls())

## ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0) #상승하락으로 넣어준다.
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA) #시계열 모델은 Y값이 들어가기 때문에 5일 후의 값으로 밀어줘야 한다. 
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, cut_kospee)
#사용할 변수들만 추출하는 과정. 
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)

#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
#시계열 모델은 상승하락이 아닌 실제 값을 도출하게 되기 때문에 이를 
#등락으로 바꿔주는 작업을 해줘야 한다. 
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
#평균 accuracy를 이 변수를 사용했을 경우의 결과값으로 뽑아줘야 한다. 
result_model_1 <- list(accuracy = mean(c(acc_5 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_5, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_1, 'arimax_result_model_1.csv', row.names = T)
rm(list = ls())

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[2]거시지표 + 산업군(수익률)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

# ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, 
                                  is.ind.1, is.fr.1, is.oth.1, is.ins.1, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)

FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_2 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_2, 'arimax_result_model_2.csv', row.names = T)
rm(list = ls())



##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[3]거시지표 + 산업군(수익률X시가총액) 
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

# ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth, 
                                  is.ind.2, is.fr.2, is.ins.2, is.oth.2, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)


FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_3 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_3, 'arimax_result_model_3.csv', row.names = T)
rm(list = ls())

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[4]거시지표 + 산업군(수익률X시가총액X컨센서스) 
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

# ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ind.3, is.fr.3, is.ins.3, is.oth.3, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)



#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_4 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_4, 'arimax_result_model_4.csv', row.names = T)
rm(list = ls())

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[5]거시지표 + 산업군(수익률) + 심리(망각 O)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

# ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ind.1, is.fr.1, is.ins.1, is.oth.1, 
                                  senti_for_nf, senti_insti_nf, senti_ind_nf,senti_other_nf, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)


#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_5 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_5, 'arimax_result_model_5.csv', row.names = T)
rm(list = ls())

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[6]거시지표 + 산업군(수익률) + 심리(망각 X)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

# ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ins.1, is.ind.1, is.fr.1, is.oth.1, 
                                  senti_ind, senti_for, senti_insti, senti_other, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)


#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_6 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_6, 'arimax_result_model_6.csv', row.names = T)
rm(list = ls())

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[7]거시지표 + 산업군(수익률X시가총액) + 심리(망각 O)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

# ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ind.2, is.fr.2, is.ins.2, is.oth.2, senti_ind_nf, senti_for_nf, 
                                  senti_insti_nf, senti_other_nf, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)


#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_7 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_7, 'arimax_result_model_7.csv', row.names = T)
rm(list = ls())

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[8]거시지표 + 산업군(수익률X시가총액) + 심리(망각 X)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')
# ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ind.2, is.fr.2, is.ins.2, is.oth.2, senti_ind, senti_for,
                                  senti_insti, senti_other, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)


#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)

###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_8 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_8, 'arimax_result_model_8.csv', row.names = T)
rm(list = ls())


#########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[9]거시지표 + 산업군(수익률X시가총액X컨센서스) + 심리(망각 O)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

## ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ind.3, is.fr.3, is.ins.3, is.oth.3, 
                                  senti_ind_nf, senti_for_nf, senti_insti_nf, senti_other_nf, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)


#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_9 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                         acc_5 )),
                       f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_9, 'arimax_result_model_9.csv', row.names = T)
rm(list = ls())


#########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[10]거시지표 + 산업군(수익률X시가총액X컨센서스) + 심리(망각 X)
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

## ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ind.2, is.fr.2, is.ins.2, is.oth.2, 
                                  senti_ind_nf, senti_for_nf, senti_insti_nf, senti_other_nf,
                                  variability_index ,cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)


#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_10 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                          acc_5 )),
                        f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_10, 'arimax_result_model_10.csv', row.names = T)
rm(list = ls())




#########################################################################################################################
##########################################################################################################################
##########################################################################################################################
#[11]거시지표 + 산업군(수익률X시가총액) + 심리 + 변동성
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
setwd('C:/Users/jiho0/OneDrive/바탕 화면/2019빅데이터페스티벌데이터')

## ARIMAX 
kospidata <- fread('final_data.csv') %>% as.data.frame()
kospidata <- kospidata %>% select(-variability_index)
new_data <- fread('final_data_real_train_6_v02.csv')
new_data <- new_data %>% select(date, variability_index)
kospidata <- left_join(kospidata, new_data, by = c('date' = 'date'))
colnames(kospidata)
real_updown <- kospidata %>% select(Y)
real_updown$Y <- ifelse(real_updown$Y == '상승', 1, 0)
real_updown <- rbind(real_updown, NA,NA,NA,NA,NA)
cut_kospee <- kospidata$kospi
cut_kospee <- c(cut_kospee, NA,NA,NA,NA,NA)
matmat <- matrix(nrow = 5, ncol = 32, NA) %>% as.data.frame()
colnames(matmat) <- colnames(kospidata)
kospidata <- rbind(matmat, kospidata)
real_updown <- cbind(real_updown, cut_kospee)
kospidata <- cbind(kospidata, real_updown)
kospidata <- kospidata %>% na.omit
kospidata <- kospidata %>% as.data.frame()
kospidata <- kospidata[,-2]
kospidata <- kospidata %>% select(date, ind.Y, fr.Y, ins.Y, oth.Y, ms.ind, ms.fr, ms.ins, ms.oth,
                                  is.ind.3, is.fr.3, is.ins.3, is.oth.3, 
                                  senti_ind, senti_for, senti_insti, senti_other, cut_kospee)
Stock_Price <- kospidata$cut_kospee %>% as.numeric()
kospidata$date <- kospidata$date %>% ts(frequency = 365.25)
str(kospidata)



#-----------------롤링윈도우 방식을 사용하여 다섯개의 train - validation셋을 만들어준다. 
FOLDS <-  list()
FOLDS$Ftest_1 = 1:1944 
FOLDS$Ftest_2 = 383:2326 
FOLDS$Ftest_3 = 765:2708 
FOLDS$Ftest_4 = 1147:3090 
FOLDS$Ftest_5 = 1529:3472 

FOLDS_TEST = list()
FOLDS_TEST$Fold_1 = 1945:2431
FOLDS_TEST$Fold_2 = 2327:2813
FOLDS_TEST$Fold_3 = 2709:3195
FOLDS_TEST$Fold_4 = 3091:3577
FOLDS_TEST$Fold_5 = 3473:3953

###############rolling window_1
ts_train_1 <-Stock_Price[FOLDS$Ftest_1]
ts_train_1 <-ts(ts_train_1,frequency = 365.25)
ts_test_1 <- Stock_Price[FOLDS_TEST$Fold_1]
ts_test_1 <- ts(ts_test_1,frequency=365.25)
xreg_train_1 <- kospidata[FOLDS$Ftest_1, 2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_train_1) <- FOLDS$Ftest_1
xreg_test_1<-kospidata[FOLDS_TEST$Fold_1,2:(ncol(kospidata)-1)] %>% as.matrix()
#rownames(xreg_test_1) <- FOLDS_TEST$Fold_1
fit_arima_regression_1 <- auto.arima(ts_train_1,xreg=xreg_train_1 ,approximation=FALSE)
yhat_1 <- forecast::forecast(fit_arima_regression_1,xreg=xreg_test_1)$mean
#acc_1 <- hit.ratio(ts_test_1,yhat_1)
updown_1 <- c()
for(i in 1:(length(yhat_1)-5)) { 
  if((yhat_1[i+5] - yhat_1[i] >= 0) == TRUE){
    updown_1[i] <- 1
  }
  else{
    updown_1[i] <- 0
  }
}
made_real_updown_1 <- c()
for(i in 1:(length(ts_test_1)-5)) { 
  if((ts_test_1[i+5] - ts_test_1[i] >= 0) == TRUE){
    made_real_updown_1[i] <- 1
  }
  else{
    made_real_updown_1[i] <- 0
  }
}
updown_1 <- updown_1 %>% as.factor()
updown_1 <- updown_1 %>% as.factor()
made_real_updown_1 <- made_real_updown_1 %>% as.factor()
confusionMatrix(updown_1, made_real_updown_1)
acc_1 <- Accuracy(y_pred = updown_1, y_true = made_real_updown_1)
f1_1 <- F1_Score(y_pred = made_real_updown_1, y_true =  updown_1)


###############rolling window_2
ts_train_2 <-Stock_Price[FOLDS$Ftest_2]
ts_train_2 <-ts(ts_train_2,frequency = 365.25)
ts_test_2 <- Stock_Price[FOLDS_TEST$Fold_2]
ts_test_2 <- ts(ts_test_2,frequency=365.25)
xreg_train_2 <- kospidata[FOLDS$Ftest_2, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_2) <- FOLDS$Ftest_2
xreg_test_2<-kospidata[FOLDS_TEST$Fold_2,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_2) <- FOLDS_TEST$Fold_2
fit_arima_regression_2 <- auto.arima(ts_train_2,xreg=xreg_train_2,approximation=FALSE)
yhat_2 <- forecast(fit_arima_regression_2,xreg=xreg_test_2)$mean
#acc_2 <- hit.ratio(ts_test_2,yhat_2)
updown_2 <- c()
for(i in 1:(length(yhat_2)-5)) { 
  if((yhat_2[i+5] - yhat_2[i] >= 0) == TRUE){
    updown_2[i] <- 1
  }
  else{
    updown_2[i] <- 0
  }
}
made_real_updown_2 <- c()
for(i in 1:(length(ts_test_2)-5)) { 
  if((ts_test_2[i+5] - ts_test_2[i] >= 0) == TRUE){
    made_real_updown_2[i] <- 1
  }
  else{
    made_real_updown_2[i] <- 0
  }
}
updown_2 <- updown_2 %>% as.factor()
updown_2 <- updown_2 %>% as.factor()
made_real_updown_2 <- made_real_updown_2 %>% as.factor()
confusionMatrix(updown_2, made_real_updown_2)
acc_2 <- Accuracy(y_pred = updown_2, y_true = made_real_updown_2)
f1_2 <- F1_Score(y_pred = made_real_updown_2, y_true =  updown_2)

###############rolling window_3
ts_train_3 <-Stock_Price[FOLDS$Ftest_3]
ts_train_3 <-ts(ts_train_3,frequency = 365.25)
ts_test_3 <- Stock_Price[FOLDS_TEST$Fold_3]
ts_test_3 <- ts(ts_test_3,frequency=365.25)
xreg_train_3 <- kospidata[FOLDS$Ftest_3, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_3) <- FOLDS$Ftest_3
xreg_test_3<-kospidata[FOLDS_TEST$Fold_3,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_3) <- FOLDS_TEST$Fold_3
fit_arima_regression_3 <- auto.arima(ts_train_3,xreg=xreg_train_3,approximation=FALSE)
yhat_3 <- forecast(fit_arima_regression_3,xreg=xreg_test_3)$mean
#acc_3 <- hit.ratio(ts_test_3,yhat_3)
updown_3 <- c()
for(i in 1:(length(yhat_3)-5)) { 
  if((yhat_3[i+5] - yhat_3[i] >= 0) == TRUE){
    updown_3[i] <- 1
  }
  else{
    updown_3[i] <- 0
  }
}
made_real_updown_3 <- c()
for(i in 1:(length(ts_test_3)-5)) { 
  if((ts_test_3[i+5] - ts_test_3[i] >= 0) == TRUE){
    made_real_updown_3[i] <- 1
  }
  else{
    made_real_updown_3[i] <- 0
  }
}
updown_3 <- updown_3 %>% as.factor()
updown_3 <- updown_3 %>% as.factor()
made_real_updown_3 <- made_real_updown_3 %>% as.factor()
confusionMatrix(updown_3, made_real_updown_3)
acc_3 <- Accuracy(y_pred = updown_3, y_true = made_real_updown_3)
f1_3 <- F1_Score(y_pred = made_real_updown_3, y_true =  updown_3)

###############rolling window_4
ts_train_4 <-Stock_Price[FOLDS$Ftest_4]
ts_train_4 <-ts(ts_train_4,frequency = 365.25)
ts_test_4 <- Stock_Price[FOLDS_TEST$Fold_4]
ts_test_4 <- ts(ts_test_4,frequency=365.25)
xreg_train_4 <- kospidata[FOLDS$Ftest_4, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_4) <- FOLDS$Ftest_4
xreg_test_4<-kospidata[FOLDS_TEST$Fold_4,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_4) <- FOLDS_TEST$Fold_4
fit_arima_regression_4 <- auto.arima(ts_train_4,xreg=xreg_train_4,approximation=FALSE)
yhat_4 <- forecast(fit_arima_regression_4,xreg=xreg_test_4)$mean
#acc_4 <- hit.ratio(ts_test_4,yhat_4)
updown_4 <- c()
for(i in 1:(length(yhat_4)-5)) { 
  if((yhat_4[i+5] - yhat_4[i] >= 0) == TRUE){
    updown_4[i] <- 1
  }
  else{
    updown_4[i] <- 0
  }
}
made_real_updown_4 <- c()
for(i in 1:(length(ts_test_4)-5)) { 
  if((ts_test_4[i+5] - ts_test_4[i] >= 0) == TRUE){
    made_real_updown_4[i] <- 1
  }
  else{
    made_real_updown_4[i] <- 0
  }
}
updown_4 <- updown_4 %>% as.factor()
updown_4 <- updown_4 %>% as.factor()
made_real_updown_4 <- made_real_updown_4 %>% as.factor()
confusionMatrix(updown_4, made_real_updown_4)
acc_4 <- Accuracy(y_pred = updown_4, y_true = made_real_updown_4)
f1_4 <- F1_Score(y_pred = made_real_updown_4, y_true =  updown_4)


###############rolling window_5
ts_train_5 <-Stock_Price[FOLDS$Ftest_5]
ts_train_5 <-ts(ts_train_5,frequency = 365.25)
ts_test_5 <- Stock_Price[FOLDS_TEST$Fold_5]
ts_test_5 <- ts(ts_test_5,frequency=365.25)
xreg_train_5 <- kospidata[FOLDS$Ftest_5, 2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_train_5) <- FOLDS$Ftest_5
xreg_test_5<-kospidata[FOLDS_TEST$Fold_5,2:(ncol(kospidata)-1)] %>% as.matrix()
rownames(xreg_test_5) <- FOLDS_TEST$Fold_5
fit_arima_regression_5 <- auto.arima(ts_train_5,xreg=xreg_train_5,approximation=FALSE)
yhat_5 <- forecast(fit_arima_regression_5,xreg=xreg_test_5)$mean
#acc_5 <- hit.ratio(ts_test_5,yhat_5)
updown_5 <- c()
for(i in 1:(length(yhat_5)-5)) { 
  if((yhat_5[i+5] - yhat_5[i] >= 0) == TRUE){
    updown_5[i] <- 1
  }
  else{
    updown_5[i] <- 0
  }
}
made_real_updown_5 <- c()
for(i in 1:(length(ts_test_5)-5)) { 
  if((ts_test_5[i+5] - ts_test_5[i] >= 0) == TRUE){
    made_real_updown_5[i] <- 1
  }
  else{
    made_real_updown_5[i] <- 0
  }
}
updown_5 <- updown_5 %>% as.factor()
updown_5 <- updown_5 %>% as.factor()
made_real_updown_5 <- made_real_updown_5 %>% as.factor()
confusionMatrix(updown_5, made_real_updown_5)
acc_5 <- Accuracy(y_pred = updown_5, y_true = made_real_updown_5)
f1_5 <- F1_Score(y_pred = made_real_updown_5, y_true =  updown_5)

#롤윈의 평균값을 구해준다.
result_model_11 <- list(accuracy = mean(c(acc_1 , acc_2 ,acc_3 ,acc_4 ,
                                          acc_5 )),
                        f1_score = mean(c(f1_1, f1_2, f1_3, f1_4, f1_5)))
write.csv(result_model_11, 'arimax_result_model_11.csv', row.names = T)
rm(list = ls())