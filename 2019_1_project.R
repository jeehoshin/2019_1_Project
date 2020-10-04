library('rvest')
library('dplyr')
library('stringr')
library('httr')

####### ???? URL
url <- read_html('https://www.wine21.com/13_search/wine_view.html?Idx=163213')
winebase <- html_nodes(url, "div.wine_info")

test <- html_nodes(winebase,"dd") %>% html_text()
test
test2 <- html_nodes(winebase,"dt") %>% html_text()
test2
#---------------name
winename <- html_nodes(winebase, "div.name_en") %>% html_text()
winename <- winename[1]
#--------------winery
wineproducer <- html_nodes(winebase, "dd.winery") %>% html_text()
wineproducer <- wineproducer[1]
#--------------wine area
winearea <- html_nodes(winebase, "dd.wine_area") %>% html_text()
winearea <- winearea[1]
#-------------variety
variety <- html_nodes(winebase, "dd.variety") %>% html_text()
variety <- variety[1]
#--------------price
base1 <- html_nodes(winebase, "strong") %>% html_text()
price <- base1[1]
#---------------year
year <- base1[2]
#----------------class
base2 <- html_nodes(winebase, "dd") %>% html_text()
class <- base2[7]
#---------------usage
usage <- base2[8]
#----------------ABV
ABV <- base2[9]


winedata <- data.frame(winename, wineproducer,winearea, variety, price, year, class, usage, ABV)
winedata
write.csv(winedata, 'test.csv')


#-----------------mining the data


#137100 - set 0
#140100???? - set 1
#143100???? - set 2
#146100???? - set 3
#149100???? - set 4
#151000???? - set 5

#---------------for(except the price)
library('rvest')
library('dplyr')
library('stringr')
library('httr')
d_winename <- c()
d_winearea <- c()
d_wineproducer <- c()
d_variety <- c()
d_price <- c()
d_year <- c()
d_class <- c()
d_usage <- c()
d_ABV <- c()


url <- read_html('https://www.wine21.com/13_search/wine_view.html?Idx=163213')
winebase <- html_nodes(url, "div.wine_info")

test <- html_nodes(winebase,"dd") %>% html_text()
test
test2 <- html_nodes(winebase,"dt") %>% html_text()
test2

for (i in 149101:151000) {
  url_a <- paste0('https://www.wine21.com/13_search/wine_view.html?Idx=',sep = "",i)
  url_ad <- read_html(url_a)
  winebase <- html_nodes(url_ad, "div.column_detail2")
  
  
  #---------------name
  winename <- html_nodes(winebase, "div.name_en") %>% html_text()
  winename <- winename[1]
  #--------------winery
  wineproducer <- html_nodes(winebase, "dd.winery") %>% html_text()
  wineproducer <- wineproducer[1]
  #--------------wine area
  winearea <- html_nodes(winebase, "dd.wine_area") %>% html_text()
  winearea <- winearea[1]
  #-------------variety
  variety <- html_nodes(winebase, "dd.variety") %>% html_text()
  variety <- variety[1]
  #--------------price
  base1 <- html_nodes(winebase, "strong") %>% html_text()
  price <- base1[1]
  #---------------year
  year <- base1[2]
  #----------------class
  base2 <- html_nodes(winebase, "dd") %>% html_text()
  class <- base2[7]
  #---------------usage
  usage <- base2[8]
  #----------------ABV
  ABV <- base2[9]
  
  
  d_winename <- c(d_winename, winename)
  d_wineproducer <- c(d_wineproducer, wineproducer)
  d_winearea <- c(d_winearea, winearea)
  d_variety <- c(d_variety, variety)
  d_price <- c(d_price, price)
  d_year <- c(d_year, year)
  d_class <- c(d_class, class)
  d_usage <- c(d_usage, usage)
  d_ABV <- c(d_ABV, ABV)
  
  winedata <- data.frame(d_winename, d_wineproducer,d_winearea, d_variety, d_price, d_year, d_class,d_usage,d_ABV)
  
  if (i %% 20 == 0 ){
    print(i)
  }
}


write.csv(winedata, "winedataset.csv")



#-------------removing NA
x <- read.csv('winedataset5.csv')
x <- x[!is.na(x$d_winename),]
x
write.csv(x, "set1.csv")

#--------rbind
x1 <- read.csv("set1.csv")
x2 <- read.csv("set2.csv")
x3 <- read.csv("set3.csv")
x4 <- read.csv("set4.csv")
x5 <- read.csv("set5.csv")
y <- rbind(x1, x2, x3, x4, x5)
write.csv(y, "wineset.csv")



#########-----------------------winecode
library('rvest')
library('dplyr')
library('stringr')
library('httr')
d_winename <- c()
d_winecode <- c()

#137000 - set 0
#140000???? - set 1
#143000???? - set 2
#146000???? - set 3
#149000???? - set 4
#151000???? - set 5

for (i in 149001:151000) {
  url_a <- paste0('https://www.wine21.com/13_search/wine_view.html?Idx=',sep = "",i)
  url_ad <- read_html(url_a)
  winebase <- html_nodes(url_ad, "div.column_detail2")
  codebase <- html_nodes(url_ad, "div.column_detail1")
  
  #---------------name
  winename <- html_nodes(winebase, "div.name_en") %>% html_text()
  winename <- winename[1]
  
  #-------------code
  winecode <- html_nodes(codebase, "span.code") %>% html_text()
  winecode <- winecode[1]
  
  d_winename <- c(d_winename, winename)
  d_winecode <- c(d_winecode, winecode)
  
  codedata <- data.frame(d_winename, d_winecode)
  
  if (i %% 20 == 0 ){
    print(i)
  }
}

write.csv(codedata, "codedataset5.csv")



#-------------removing NA
x <- read.csv('codedataset5.csv')
x <- x[!is.na(x$d_winename),]
write.csv(x, "codeset5.csv")


#--------rbind
x1 <- read.csv("codeset1.csv")
x2 <- read.csv("codeset2.csv")
x3 <- read.csv("codeset3.csv")
x4 <- read.csv("codeset4.csv")
x5 <- read.csv("codeset5.csv")
y <- rbind(x1, x2, x3, x4, x5)
write.csv(y, "codeset.csv")

##############-------------------통합 데이터

library('rvest')
library('dplyr')
library('stringr')
library('httr')

url <- read_html('https://www.wine21.com/13_search/wine_view.html?Idx=163213')
winebas <- html_nodes(url, "div.wine_info")

test <- html_nodes(winebas,"dt") %>% html_text()
x <- as.data.frame(matrix('a', ncol = 15, nrow = 1))
names(x) <- c(test, "name", "code")

#137000 - set 0
#140000???? - set 1
#143000???? - set 2
#146000???? - set 3
#149000???? - set 4
#151000???? - set 5

#151001 ~ 154000 set 1
#154001 ~ 157000 set 2
#157001 ~ 160000 set 3
#160001 ~ 163000 set 4
#163001 ~ 165000 set 5

for (i in 163001:165000) {
  url_a <- paste0('https://www.wine21.com/13_search/wine_view.html?Idx=',sep = "",i)
  #url_a <- "https://www.wine21.com/13_search/wine_view.html?Idx=163213"
  url_ad <- read_html(url_a)
  winebase <- html_nodes(url_ad, "div.column_detail2")
  codebase <- html_nodes(url_ad, "div.column_detail1")
  infobase <- html_nodes(url_ad, "div.wine_info")
  
  ####### URL
  info <- html_nodes(infobase,"dd") %>% html_text()
  naming <- html_nodes(infobase,"dt") %>% html_text()
  info <- matrix(info, nrow = 1)
  #---------------name
  winename <- html_nodes(winebase, "div.name_en") %>% html_text()
  winename <- winename[1]
  
  #-------------code
  winecode <- html_nodes(codebase, "span.code") %>% html_text()
  winecode <- winecode[1]
  
  y <- data.frame(info, winename, winecode)
  names(y) <- c(naming, "name", "code")
  y
  x
  x <- bind_rows(x, y)
  
  if (i %% 20 == 0 ){
    print(i)
  }
}
write.csv(x, "testset5.csv")


x1 <- read.csv("testset1.csv")
x1 <- x1[-1,]
x1 <- x1[!is.na(x1$code),]

x2 <- read.csv("testset2.csv")
x2 <- x2[-1,]
x2 <- x2[!is.na(x2$code),]

x3 <- read.csv("testset3.csv")
x3 <- x3[-1,]
x3 <- x3[!is.na(x3$code),]

x4 <- read.csv("testset4.csv")
x4 <- x4[-1,]
x4 <- x4[!is.na(x4$code),]

x5 <- read.csv("testset5.csv")
x5 <- x5[-1,]
x5 <- x5[!is.na(x5$code),]

y <- rbind(x1, x2, x3, x4, x5)
write.csv(y, "finaldataset2.csv")
y
y <- y[,-1]

y2 <- read.csv("finaldataset.csv")
y2 <- y2[,-1]
wine21data <- rbind(y2, y)

wine21 <- read.csv("wine21_final7.csv")
library('dplyr')
library('ggplot2')
#[1]-----------------[vintage로 시각화]
wine21$vintage <- as.factor(wine21$vintage)
vintage_c <- table(wine21$vintage)
vintage_c <- data.frame(wine21$vintage)
names
ggplot(wine21, aes(x = vintage, fill = vintage)) + 
  geom_bar(color = 'black',show.legend = T) + 
  theme(legend.position="bottom")


#[2]-----------------[country로 시각화]
wine21$country <- as.factor(wine21$country)
ggplot(wine21, aes(x = country, fill = country)) + 
  geom_bar(color = 'black',show.legend = T) +
  geom_text(aes(label=..count..),stat="count", vjust=-0.3) + 
  theme(legend.position="bottom")

####도넛으로 할 경우
ggplot(wine21, aes(x = country, fill = country)) + 
  geom_bar(color = 'black',show.legend = T) + 
  coord_polar()

####비율 도출
ggplot(wine21, aes(x = country, fill = country)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom")


#★★★★★★★★★★피드백 - 너무 적은 개수들은 없애버리자

#[3]---------------[variant로 시각화]
wine21$variant <- as.factor(wine21$variant)
ggplot(wine21, aes(x = variant, fill = variant)) + 
  geom_bar(color = 'black',show.legend = T) +
  geom_text(aes(label=..count..),stat="count", vjust=-0.3)


#[4]--------------[use로 시각화]
wine21$use <- as.factor(wine21$use)
ggplot(wine21, aes(x = use, fill = use)) + 
  geom_bar(color = 'black',show.legend = T) +
  geom_text(aes(label=..count..),stat="count",vjust=-0.3) +
  theme(legend.position="bottom")


#[5]--------------[class와 remark를 유무 상태로 변수를 만들자]

for (i in 1:length(wine21$class)) {
  if (is.na(wine21$class[i]) == TRUE) {
    wine21$class_dumb[i] <- 0
  } else {
    wine21$class_dumb[i] <- 1
  }
}

for (i in 1:length(wine21$remark)) {
  if (is.na(wine21$remark[i]) == TRUE) {
    wine21$remark_dumb[i] <- 0
  } else {
    wine21$remark_dumb[i] <- 1
  }
}

#[6]-----------[class로 시각화]

wine21$class_dumb <- as.factor(wine21$class_dumb)
ggplot(wine21, aes(x = class_dumb, fill = class_dumb)) + 
  geom_bar(color = 'black',show.legend = T) +
  geom_text(aes(label=..count..),stat="count", vjust=-0.3)

#[7]----------[remark로 시각화]

wine21$remark_dumb <- as.factor(wine21$remark_dumb)
ggplot(wine21, aes(x = remark_dumb, fill = remark_dumb)) + 
  geom_bar(color = 'black',show.legend = T) +
  geom_text(aes(label=..count..),stat="count", vjust=-0.3)

#######################################################################
#######################################################################
##########여기서 부터 가격을 사용할 것#################################
#######################################################################

wine21 <- read.csv("wine21_final7.csv")
library('dplyr')
library('ggplot2')
options(scipen = "100")

#[8]----------[price 전처리 단위는 원화]
library(stringr)
wine21$price <- str_replace_all(wine21$price, "원", "")
wine21$price <- str_replace_all(wine21$price, ",", "")
wine21_na <- wine21[!is.na(wine21$price),] #NA값만 제거

#outlier를 제거하지 않았을때의 price의 분포를 봐보자. log함수로의 변환
wine21_na$price <- as.numeric(wine21_na$price)
ggplot(wine21_na, aes(x = log(wine21_na$price))) +
  geom_histogram()

#------[가격의 outlier 추려내기]
wine21_na$price <- as.numeric(wine21_na$price)
x <- boxplot(wine21_na$price, na.action = NULL)
outlier <- x$out
range(x$out)
wine21_robust <- wine21_na[!(wine21_na$price %in% outlier), ]

#[9]------------[가격대 분포 확인]
wine21_robust$price <- as.numeric(wine21_robust$price)
ggplot(wine21_robust, aes(x = wine21_robust$price)) +
  geom_histogram(binwidth = 50000) 


######################################################
############################class와 remark 변수 만들기
######################################################

for (i in 1:length(wine21_robust$class)) {
  if (is.na(wine21_robust$class[i]) == TRUE) {
    wine21_robust$class_dumb[i] <- 0
  } else {
    wine21_robust$class_dumb[i] <- 1
  }
}

for (i in 1:length(wine21_robust$remark)) {
  if (is.na(wine21_robust$remark[i]) == TRUE) {
    wine21_robust$remark_dumb[i] <- 0
  } else {
    wine21_robust$remark_dumb[i] <- 1
  }
}

for (i in 1:length(wine21_na$class)) {
  if (is.na(wine21_na$class[i]) == TRUE) {
    wine21_na$class_dumb[i] <- 0
  } else {
    wine21_na$class_dumb[i] <- 1
  }
}

for (i in 1:length(wine21_na$remark)) {
  if (is.na(wine21_na$remark[i]) == TRUE) {
    wine21_na$remark_dumb[i] <- 0
  } else {
    wine21_na$remark_dumb[i] <- 1
  }
}

label_class <- c("no_class", "class")
label_remark <- c("no_mark", "mark")

wine21_na$class_dumb <- as.factor(wine21_na$class_dumb)
wine21_na$remark_dumb <-  as.factor(wine21_na$remark_dumb)
wine21_robust$class_dumb <- as.factor(wine21_robust$class_dumb)
wine21_robust$remark_dumb <- as.factor(wine21_robust$remark_dumb)

levels(wine21_na$class_dumb) <- label_class
levels(wine21_robust$class_dumb) <- label_class
levels(wine21_na$remark_dumb) <- label_remark
levels(wine21_robust$remark_dumb) <- label_remark




#[11]----------[국가별 가격대 + class 유무별]

###########NA만 제거
ggplot(wine21_na, aes( x = country, y = price, color = country))+
  geom_point(stat = 'identity', show.legend = T) +
  facet_grid(.~class_dumb)

###########outlier제거
ggplot(wine21_robust, aes( x = country, y = price, color = country))+
  geom_point(stat = 'identity', show.legend = T) +
  facet_grid(.~class_dumb)


#[12]----------[국가별 가격대 + remark 유무별]

###########NA만 제거
ggplot(wine21_na, aes( x = country, y = price, color = country))+
  geom_point(stat = 'identity', show.legend = T) +
  facet_grid(.~remark_dumb)

###########outlier제거
ggplot(wine21_robust, aes( x = country, y = price, color = country))+
  geom_point(stat = 'identity', show.legend = T) +
  facet_grid(.~remark_dumb)

#[13]-------------어떤 vintage가 흥하고 망했는가?
wine21_robust$vintage <- as.factor(wine21_robust$vintage)
wine21_na$vintage <- as.factor(wine21_na$vintage)

###############NA만 제거한 경우
ggplot(wine21_na, aes(x = vintage, y = price, color = vintage)) +
  geom_point(stat = 'identity', show.legend = T) + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") #mean

ggplot(wine21_na, aes(x = vintage, y = price, color = vintage)) +
  geom_point(stat = 'identity', show.legend = T) + 
  stat_summary(fun.y=median, geom="point", shape=20, size=5, color="red", fill="red") #median


###############outlier까지 제거한 경우
ggplot(wine21_robust, aes(x = vintage, y = price, color = vintage)) +
  geom_point(stat = 'identity', show.legend = T) + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") #mean

ggplot(wine21_robust, aes(x = vintage, y = price, color = vintage)) +
  geom_point(stat = 'identity', show.legend = T) + 
  stat_summary(fun.y=median, geom="point", shape=20, size=5, color="red", fill="red") #median



#[14]-------------지역별로(국가별로 어떤 빈티지가 흥하고 망했는가?)

#################주요 국가들과 아닌 국가로 나눠보자. NA만 제거한 경우
chile1 <- filter(wine21_na, country == "Chile" )
france1 <- filter(wine21_na, country == "France")
italy1 <- filter(wine21_na, country == "Italy" )
spain1 <- filter(wine21_na, country == "Spain" )
usa1 <- filter(wine21_na, country == "U.S.A")
aus1 <- filter(wine21_na, country == "Australia")                 
argen1 <- filter(wine21_na, country == "Argentina")
others1 <- filter(wine21_na, country != "Chile" &  country != "France" & country != "Italy" & country != "Spain" & 
                    country != "U.S.A" & country != "Australia" & country != "Argentina")

others1$country <- "others"
par(mfrow = c(4,2))

ggplot(chile1, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(france1, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(italy1, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(usa1, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(argen1, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(aus1, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(others1, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

###################outlier까지 제거한 경우 
chile2 <- filter(wine21_robust, country == "Chile" )
france2 <- filter(wine21_robust, country == "France")
italy2 <- filter(wine21_robust, country == "Italy" )
spain2 <- filter(wine21_robust, country == "Spain" )
usa2 <- filter(wine21_robust, country == "U.S.A")
aus2 <- filter(wine21_robust, country == "Australia")                 
argen2 <- filter(wine21_robust, country == "Argentina")
others2 <- filter(wine21_robust, country != "Chile" &  country != "France" & country != "Italy" & country != "Spain" & 
                    country != "U.S.A" & country != "Australia" & country != "Argentina")

others2$country <- "others"
par(mfrow = c(4,2))
ggplot(chile2, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(france2, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(italy2, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(usa2, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(argen2, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(aus2, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

ggplot(others2, aes(x = vintage, y = price)) + 
  geom_histogram(stat = 'identity', show.legend = T) + 
  theme(legend.position="bottom") 

#[15]-------같은 와인이라면 vintage가 오래된 거 사는 게 낫나 (평점, 가격)

#[16]-------나라별 와인가격 평균 : 미디언 시각화

##############NA만 제거한 경우
wine21_world1 <- rbind(chile1, france1, italy1, others1, spain1, aus1, usa1, argen1)
wine21_world1$country <- as.factor(wine21_world1$country)
wine21_world1$country %>% summary() %>% sort()
par(mfrow = c(1,1))

world_mean1 <- wine21_world1 %>% group_by(country) %>% summarise(mean = mean(price))
ggplot(world_mean1, aes(x = country, y = mean , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= mean), vjust=-0.3)

world_median1 <- wine21_world1 %>% group_by(country) %>% summarise(median = median(price))
ggplot(world_median1, aes(x = country, y = median , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= median), vjust=-0.3)


#############outlier까지제거한 경우
wine21_world2 <- rbind(chile2, france2, italy2, others2, spain2, aus2, usa2, argen2)
wine21_world2$country <- as.factor(wine21_world2$country)
wine21_world2$country %>% summary() %>% sort()
par(mfrow = c(1,1))

world_mean2 <- wine21_world2 %>% group_by(country) %>% summarise(mean = mean(price))
ggplot(world_mean2, aes(x = country, y = mean , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= mean), vjust=-0.3)

world_median2 <- wine21_world2 %>% group_by(country) %>% summarise(median = median(price))
ggplot(world_median2, aes(x = country, y = median , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= median), vjust=-0.3)



#[17]----------구세계 신세계 와인가격 평균, 미디언 비교

##################NA만 제거한 경우
old_w1 <- rbind(france1, italy1, spain1)
new_w1 <- rbind(usa1, aus1, argen1, chile1)
old_mean1 <- old_w1 %>% group_by(country) %>% summarise(mean = mean(price))
new_mean1 <- new_w1 %>% group_by(country) %>% summarise(mean = mean(price))
old_median1 <- old_w1 %>% group_by(country) %>% summarise(median = median(price))
new_median1 <- new_w1 %>% group_by(country) %>% summarise(median = median(price))

#######old mean
ggplot(old_mean1, aes(x = country, y = mean , color = country)) +
  geom_point(stat = 'identity', size = 3) + 
  geom_text(aes(label= mean), vjust=-0.3)

########new mean
ggplot(new_mean1, aes(x = country, y = mean , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= mean), vjust=-0.3)

#######old median
ggplot(old_median1, aes(x = country, y = median , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= median), vjust=-0.3)

########new median
ggplot(new_median1, aes(x = country, y = median , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= median), vjust=-0.3)

###################outlier까지 제거한 경우

old_w2 <- rbind(france2, italy2, spain2)
new_w2 <- rbind(usa2, aus2, argen2, chile2)
old_mean2 <- old_w2 %>% group_by(country) %>% summarise(mean = mean(price))
new_mean2 <- new_w2 %>% group_by(country) %>% summarise(mean = mean(price))
old_median2 <- old_w2 %>% group_by(country) %>% summarise(median = median(price))
new_median2 <- new_w2 %>% group_by(country) %>% summarise(median = median(price))

#######old mean
ggplot(old_mean2, aes(x = country, y = mean , color = country)) +
  geom_point(stat = 'identity', size = 3) + 
  geom_text(aes(label= mean), vjust=-0.3)

########new mean
ggplot(new_mean2, aes(x = country, y = mean , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= mean), vjust=-0.3)

#######old median
ggplot(old_median2, aes(x = country, y = median , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= median), vjust=-0.3)

########new median
ggplot(new_median2, aes(x = country, y = median , color = country)) +
  geom_point(stat = 'identity',size = 3) + 
  geom_text(aes(label= median), vjust=-0.3)
#[18]---------------대륙별 와인가격... 구세계 신세계랑 비슷한듯?

#[19]--------------- 와인 원산지 클러스터링 후 시각화(가격 / 품종)

#[20]--------------

wine <- read.csv("final.csv", header = T)

#-------meat과 cheese로 나눈다. 
meatdata <- wine[,1:12]
cheesedata <- cbind(wine[,1:11], wine[,13])
colnames(cheesedata)[12] <- "p_cheese"
meatdata$p_meat<- gsub("Yes", "1", meatdata$p_meat)
meatdata$p_meat<- gsub("No", "0", meatdata$p_meat)
meatdata$vintage <- as.numeric(meatdata$vintage)
cheesedata$vintage <- as.numeric(cheesedata$vintage)
cheesedata$p_cheese<- gsub("Yes", "1", cheesedata$p_cheese)
cheesedata$p_cheese<- gsub("No", "0", cheesedata$p_cheese)
meatdata$p_meat <- as.numeric(meatdata$p_meat)
cheesedata$p_cheese <- as.numeric(cheesedata$p_cheese)
rm(wine)

#--------------dummy화
meatdata_d <- dummy.data.frame(meatdata)
cheesedata_d <- dummy.data.frame(cheesedata)
rm(meatdata)
rm(cheesedata)

#----------------Matrix를 만들자
meatmat <-  xgb.DMatrix(as.matrix(meatdata_d[,-33]), label = meatdata_d$p_meat) #y값을 빼주고 label에 넣어야함
cheesemat <-  xgb.DMatrix(as.matrix(cheesedata_d[,-33]), label = cheesedata_d$p_cheese)

#-------------5 cv (여기부턴 meat만 다룬다.)
FOLDS = createFolds(meatdata_d$p_meat, k=5, returnTrain = TRUE)
FOLDS_test_1 = setdiff(1:nrow(meatdata_d), FOLDS$Fold1)
FOLDS_test_2 = setdiff(1:nrow(meatdata_d), FOLDS$Fold2)
FOLDS_test_3 = setdiff(1:nrow(meatdata_d), FOLDS$Fold3)
FOLDS_test_4 = setdiff(1:nrow(meatdata_d), FOLDS$Fold4)
FOLDS_test_5 = setdiff(1:nrow(meatdata_d), FOLDS$Fold5)

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = FOLDS_test_1
FOLDS_TEST$Fold2 = FOLDS_test_2
FOLDS_TEST$Fold3 = FOLDS_test_3
FOLDS_TEST$Fold4 = FOLDS_test_4
FOLDS_TEST$Fold5 = FOLDS_test_5

######### prepare xgb
library(xgboost)
library(MLmetrics)
library(sdcTable)
library(data.table)
library(dplyr)
library(dummies)
library(caret)
library(Metrics)

set.seed(1)

#1. 위 과정을 진행한 후
#2. 그 주변 부위?를 그리드 서치 진행
#3. 최종 파라미터 선정
#4. 그 파라미터를 가지고 모델 적합 
#5. cv에러 도출 

###------------베이지안 파라미터 찾기 
xgb_cv_bayes <- function(max.depth, min_child_weight, subsample, colsample) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                             max_depth = max.depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
               data = meatmat, nround = 1000,
               folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) ### Mr.Kwon => nthread= 4
  list(Score = cv$evaluation_log[,max(test_auc_mean)],
       Pred = cv$pred)
}

OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max.depth = c(1L, 5L),
                                              min_child_weight = c(1L, 10L),
                                              subsample = c(0.5, 1),
                                              colsample = c(0.5, 1)),
                                init_points = 20, n_iter = 80,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)
#Best Parameters Found: 
#Round = 67	max.depth = 4.0000	min_child_weight = 8.0000	
#subsample = 0.9836	colsample = 0.9424	Value = 0.8808 


#####---------------------모델을 적합하잩!
xgfit_meat <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                                   max_depth = OPT_Res$Best_Par[1],
                                   min_child_weight = OPT_Res$Best_Par[2],
                                   subsample = OPT_Res$Best_Par[3], colsample_bytree = OPT_Res$Best_Par[4],
                                   objective = "binary:logistic", eval_metric = "auc"),
                     data = meatmat, nround = 1000,
                     folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
                     early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2)
xgfit_meat


#######################################
##########--------------cheese의 best parameter를 찾자.
FOLDS2 = createFolds(cheesedata_d$p_cheese, k=5, returnTrain = TRUE)
FOLDS_test_1_c = setdiff(1:nrow(cheesedata_d), FOLDS2$Fold1)
FOLDS_test_2_c = setdiff(1:nrow(cheesedata_d), FOLDS2$Fold2)
FOLDS_test_3_c = setdiff(1:nrow(cheesedata_d), FOLDS2$Fold3)
FOLDS_test_4_c = setdiff(1:nrow(cheesedata_d), FOLDS2$Fold4)
FOLDS_test_5_c = setdiff(1:nrow(cheesedata_d), FOLDS2$Fold5)

FOLDS_TEST2 = list()
FOLDS_TEST2$Fold1 = FOLDS_test_1_c
FOLDS_TEST2$Fold2 = FOLDS_test_2_c
FOLDS_TEST2$Fold3 = FOLDS_test_3_c
FOLDS_TEST2$Fold4 = FOLDS_test_4_c
FOLDS_TEST2$Fold5 = FOLDS_test_5_c

######### prepare xgb
library(xgboost)
library(MLmetrics)
library(sdcTable)
library(data.table)
library(dplyr)
library(dummies)
library(caret)
library(Metrics)

set.seed(1)

#1. 위 과정을 진행한 후
#2. 그 주변 부위?를 그리드 서치 진행
#3. 최종 파라미터 선정
#4. 그 파라미터를 가지고 모델 적합 
#5. cv에러 도출 

###------------cheese 베이지안 파라미터 찾기 
xgb_cv_bayes2 <- function(max.depth, min_child_weight, subsample, colsample) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                             max_depth = max.depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree = colsample, objective = "binary:logistic", eval_metric = "auc"),
               data = cheesemat, nround = 1000,
               folds = FOLDS_TEST2, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2) ### Mr.Kwon => nthread= 4
  list(Score = cv$evaluation_log[,max(test_auc_mean)],
       Pred = cv$pred)
}

OPT_Res <- BayesianOptimization(xgb_cv_bayes2,
                                bounds = list(max.depth = c(1L, 5L),
                                              min_child_weight = c(1L, 10L),
                                              subsample = c(0.5, 1),
                                              colsample = c(0.5, 1)),
                                init_points = 20, n_iter = 80,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)


#####---------------------모델을 적합하잩!
xgfit_cheese <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                                     max_depth = OPT_Res$Best_Par[1],
                                     min_child_weight = OPT_Res$Best_Par[2],
                                     subsample = OPT_Res$Best_Par[3], colsample_bytree = OPT_Res$Best_Par[4],
                                     objective = "binary:logistic", eval_metric = "auc"),
                       data = cheesemat, nround = 1000,
                       folds = FOLDS_TEST2, prediction = TRUE, showsd = TRUE,
                       early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2)
xgfit_cheese


#------------------default value finding
xgfit_default_cheese <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                                             objective = "binary:logistic", eval_metric = "auc"),
                               data = cheesemat, nround = 1000,
                               folds = FOLDS_TEST2, prediction = TRUE, showsd = TRUE,
                               early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2)
xgfit_default_cheese

xgfit_default_meat <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                                           objective = "binary:logistic", eval_metric = "auc"),
                             data = meatmat, nround = 1000,
                             folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
                             early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2)
xgfit_default_meat
#Best Parameters Found: 
#Round = 67	max.depth = 4.0000	min_child_weight = 8.0000	
#subsample = 0.9836	colsample = 0.9424	Value = 0.8808 
xgfit_meat <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                                   max_depth = 4,
                                   min_child_weight = 8,
                                   subsample = 0.9836, colsample_bytree = 0.9424,
                                   objective = "binary:logistic", eval_metric = "auc"),
                     data = meatmat, nround = 1000,
                     folds = FOLDS_TEST, prediction = TRUE, showsd = TRUE,
                     early_stopping_rounds = 200, maximize = T, verbose = 0, nthread = 2)
xgfit_meat


#----------------prediction part
holdoutset <- read.csv("holdoutset.csv", header = T)
holdoutset <- holdoutset[,-1]
holdoutset$vintage <- as.numeric(holdoutset$vintage)
holdoutset$p_meat <- as.logical(holdoutset$p_meat)

#holdoutset$p_meat <- rep(TRUE, 491)
holdoutset_d <- dummy.data.frame(holdoutset)
holdoutset_d <- holdoutset_d[-491,]
holdmat <-  xgb.DMatrix(as.matrix(holdoutset_d[,-33]), label = holdoutset_d$p_meat)

meatdata_d$p_meat <- as.logical(meatdata_d$p_meat)
meatmat <-  xgb.DMatrix(as.matrix(meatdata_d[,-33]), label = meatdata_d$p_meat)
xgfitmeat <- xgb.train(data = meatmat, label = meatdata_d$p_meat,
                       params = list(booster = "gbtree", eta = 0.01, max_depth = 4,
                                     min_child_weight = 8,  subsample = 0.9836, colsample_bytree = 0.9424,
                                     objective = "binary:logistic", eval_metric = "auc"), nround = 1000)

pred<-predict(xgfitmeat, holdmat,type="terms", se.fit = T)
pred
x <- ifelse(pred >= 0.5, "1", "0")
confusionMatrix(pred)
library(caret)

#----------percentage
holdoutset <- read.csv("holdoutset.csv", header = T)
holdoutset <- holdoutset[-491,]
holdoutset$p_meat <- pred
write.csv(holdoutset, "predictpercentage.csv", row.names = F)

#--------------predicting
holdoutset <- read.csv("holdoutset.csv", header = T)
holdoutset <- holdoutset[-491,]
holdoutset$p_meat <- x
write.csv(holdoutset, "predicting.csv", row.names = F)

#---------implot
bst <- xgboost(data = agaricus.train$data, label = agaricus.train$label, max_depth = 3,
               eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

importance_matrixs <- xgb.importance(colnames(cheesedata_d), model = xgfitcheese)
xgb.plot.importance(importance_matrixs)
xgfitcheese <- xgb.train(data = cheesemat, label = cheesedata_d$p_cheese,
                         params = list(booster = "gbtree", eta = 0.01, max_depth = 4,
                                       min_child_weight = 8,  subsample = 0.9836, colsample_bytree = 0.9424,
                                       objective = "binary:logistic", eval_metric = "auc"), nround = 1000)
write.csv(wine21data, "wine21data.csv")