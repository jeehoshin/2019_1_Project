library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr)
options(scipen = 100)
#-----------기상 데이터 열기 (노원구)
setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터/노원구')
codebook <- fread('codebook.csv')
codebook <- codebook[,c(1,2)]
colnames(codebook) <- c('code', 'name')
codename <- c('V10O1610102', 'V10O1610200', 'V10O1610293', 'V10O1610297', 'V10O1610312', 
              'V10O1610351', 'V10O1610356', 'V10O1610376', 'V10O1610610', 'V10O1610616',
              'V10O1610629', 'V10O1610630', 'V10O1610642', 'V10O1610643', 'V10O1611097', 
              'V10O1611100' ,'V10O1611102', 'V10O1611104', 'V10O1611150', 'V10O1611229', 
              'V10O1611652', 'V10O1612126')
fuck <- data.frame(tm = NA, flag = NA, pm10 = NA, co2 = NA, vocs = NA, noise = NA,temp = NA,
                   humi = NA, pm25 = NA, code = NA, name = NA)
for(k in codename){
  no_one <- read.csv(paste0(k, '.csv'))
  no_one <- no_one %>% as.data.frame()
  #999, 9999 -> NA
  for(i in 3:ncol(no_one)){
    no_one[,i] <- no_one[,i] %>% as.character %>% revalue(c('-999' = NA, '-9999' = NA))
  }
  #시간은 필요 없다.
  no_one$tm <- no_one$tm %>% substr(1,8)
  #평균값 도출
  for(i in c(1, 3:ncol(no_one))) { 
    if(i == 1) {
      no_one[,i]<- no_one[,i] %>% as.factor()
    } else
      no_one[,i] <- no_one[,i] %>% as.numeric
  }
  no_one <- no_one %>% filter(pm25 != 4104)
  no_one <- no_one %>% filter(pm25 != 3803)
  no_one <- no_one %>% filter(pm10 != 1057)
  x <- no_one %>% dplyr::group_by(tm) %>% dplyr::summarise(flag = mean(flag, na.rm = T),
                                                           pm10 = max(pm10, na.rm = T), co2 = mean(co2, na.rm = T),
                                                           vocs = mean(vocs, na.rm = T), noise = mean(noise, na.rm = T),
                                                           temp = mean(temp, na.rm = T), humi = mean(humi, na.rm = T), 
                                                           pm25 = max(pm25, na.rm = T))
  x$code <- rep(k, nrow(x))
  x <- left_join(x, codebook, by = c('code' = 'code'))
  print(k)
  fuck <- rbind(fuck, x)
  rm(x)
  rm(no_one)
}
fuck <- fuck %>% select(-c(flag,co2,vocs,code))
fuck <- fuck[-1,] #알 수 없는 NA
fuck <- fuck %>% dplyr::group_by(tm, name) %>% dplyr::summarise( pm10 = max(pm10, na.rm = T),noise = mean(noise, na.rm = T),
                                                                 temp = mean(temp, na.rm = T), humi = mean(humi, na.rm = T), 
                                                                 pm25 = max(pm25, na.rm = T))
setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터')
write.csv(fuck, '노원구_기상.csv', row.names = F)
rm(fuck)
#-------------------------------------------------------------------------------------------
#-----------기상 데이터 열기(종로구)
setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터/종로구')
codename <- c('V01o1610468', 'V10O1610252', 'V10O1610540' ,'V10O1610542', 'V10O1610543',
              'V10O1610544', 'V10O1610545', 'V10O1610546', 'V10O1610567', 'V10O1611145', 
              'V10O1611151', 'V10O1611170', 'V10O1611172', 'V10O1611173' ,'V10O1611220', 
              'V10O1611251', 'V10O1611255', 'V10O1611258', 'V10O1611289', 'V10O1611623', 
              'V10O1611634', 'V10O1611639', 'V10O1611645', 'V10O1611658', 'V10O1611684', 
              'V10O1611698', 'V10O1611722' ,'V10O1611750', 'V10O1611887', 'V10O1612106',
              'V10O1612113')
fuck <- data.frame(tm = NA, flag = NA, pm10 = NA, co2 = NA, vocs = NA, noise = NA,temp = NA,
                   humi = NA, pm25 = NA, code = NA, name = NA)
for(k in codename){
  no_one <- read.csv(paste0(k, '.csv'))
  no_one <- no_one %>% as.data.frame()
  #999, 9999 -> NA
  for(i in 3:ncol(no_one)){
    no_one[,i] <- no_one[,i] %>% as.character %>% revalue(c('-999' = NA, '-9999' = NA))
  }
  #시간은 필요 없다.
  no_one$tm <- no_one$tm %>% substr(1,8)
  #평균값 도출
  for(i in c(1, 3:ncol(no_one))) { 
    if(i == 1) {
      no_one[,i]<- no_one[,i] %>% as.factor()
    } else
      no_one[,i] <- no_one[,i] %>% as.numeric
  }
  no_one <- no_one %>% filter(pm25 != 4406)
  no_one <- no_one %>% filter(pm25 != 6900)
  no_one <- no_one %>% filter(pm25 != 6200)
  x <- no_one %>% dplyr::group_by(tm) %>% dplyr::summarise(flag = mean(flag, na.rm = T),
                                                           pm10 = max(pm10, na.rm = T), co2 = mean(co2, na.rm = T),
                                                           vocs = mean(vocs, na.rm = T), noise = mean(noise, na.rm = T),
                                                           temp = mean(temp, na.rm = T), humi = mean(humi, na.rm = T), 
                                                           pm25 = max(pm25, na.rm = T))
  x$code <- rep(k, nrow(x))
  x <- left_join(x, codebook, by = c('code' = 'code'))
  print(nrow(x))
  fuck <- rbind(fuck, x)
  rm(x)
  rm(no_one)
}
fuck <- fuck %>% select(-c(flag,co2,vocs,code))
fuck <- fuck[-1,] #알 수 없는 NA
fuck <- fuck %>% filter(name != '명동'& name != '천연동' )
fuck <- fuck %>% dplyr::group_by(tm, name) %>% dplyr::summarise( pm10 = max(pm10, na.rm = T),noise = mean(noise, na.rm = T),
                                                                 temp = mean(temp, na.rm = T), humi = mean(humi, na.rm = T), 
                                                                 pm25 = max(pm25, na.rm = T))
setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터')
write.csv(fuck, '종로구_기상.csv', row.names = F)


###여기부터 돌려도 무방하다.
##----------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------
library(dplyr)
library(data.table)
library(plyr)
library(VIM)
options(scipen = 100)
#[1-1]유동인구 데이터 열기 - 모든 날짜를 합치기
u_dong <- data.frame(STD_YM = NA,STD_YMD  = NA,HDONG_CD= NA,HDONG_NM= NA,MAN_FLOW_POP_CNT_0004= NA,
                     MAN_FLOW_POP_CNT_0509= NA,MAN_FLOW_POP_CNT_1014= NA,MAN_FLOW_POP_CNT_1519= NA,
                     MAN_FLOW_POP_CNT_2024= NA,MAN_FLOW_POP_CNT_2529= NA,MAN_FLOW_POP_CNT_3034= NA,
                     MAN_FLOW_POP_CNT_3539= NA,MAN_FLOW_POP_CNT_4044= NA,MAN_FLOW_POP_CNT_4549= NA,
                     MAN_FLOW_POP_CNT_5054= NA,MAN_FLOW_POP_CNT_5559= NA,MAN_FLOW_POP_CNT_6064= NA,
                     MAN_FLOW_POP_CNT_6569= NA,MAN_FLOW_POP_CNT_70U= NA,WMAN_FLOW_POP_CNT_0004= NA,
                     WMAN_FLOW_POP_CNT_0509= NA,WMAN_FLOW_POP_CNT_1014= NA,WMAN_FLOW_POP_CNT_1519= NA,
                     WMAN_FLOW_POP_CNT_2024= NA,WMAN_FLOW_POP_CNT_2529= NA,WMAN_FLOW_POP_CNT_3034= NA,
                     WMAN_FLOW_POP_CNT_3539= NA,WMAN_FLOW_POP_CNT_4044= NA,WMAN_FLOW_POP_CNT_4549= NA,
                     WMAN_FLOW_POP_CNT_5054= NA,WMAN_FLOW_POP_CNT_5559= NA,WMAN_FLOW_POP_CNT_6064= NA,
                     WMAN_FLOW_POP_CNT_6569= NA,WMAN_FLOW_POP_CNT_70U = NA)
setwd('C:/Users/jiho0/Downloads/방학세미나/유동인구데이터/성연령유동')
for (i in 1:12) {
  fuck <- fread(paste0(i, '.csv'), encoding = 'UTF-8')
  fuck <- fuck %>% as.data.frame()
  u_dong <- rbind(u_dong,fuck)
  rm(fuck)
}
setwd('C:/Users/jiho0/Downloads/방학세미나/카드매출데이터')
gucode <- fread('shinhancode.csv') #지역 코드를 추출

#[3-1]기상데이터와 머지해서 최종 데이터 셋을 일단! 뽑자.
setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터')
noone <- fread('노원구_기상.csv')
jongno <- fread('종로구_기상.csv')
#[3-2]기상데이터의 이름을 맞춰준다.
noone$name <- gsub(",", ".",noone$name)
jongno$name <- gsub(",", ".",jongno$name)
jongno[(jongno$name == '종로 1.2.3.4가동'),]$name <- '종로1.2.3.4가동'

#[3-3]기상데이터를 합쳐주자
nalssi <- rbind(noone, jongno)
#[3-5]우선 유동과 날씨를 머지시켜야 한다. (신한은 중복이 들어가서 데이터가 늘어남)
udongnalssi <- left_join(u_dong,nalssi, by = c('STD_YMD' = 'tm' , 'HDONG_NM' = 'name'))


#[5-1]na가 얼마나 있나 보자.
udongnalssi  <- udongnalssi[-1,]
udongnalssi <- left_join(udongnalssi,gucode[,c(3,4)], by = c('HDONG_NM' = 'Dong'))
aggr(udongnalssi[,30:39],prop=FALSE,numbers=TRUE)

#[5-2]우리의 데이터에 어떤 방법이 어울리는지 찾아보자.(KNN imputation vs MiCE)
udongnalssi <- udongnalssi %>% select(-c(STD_YM,HDONG_CD))
or_buam <- udongnalssi %>% filter(HDONG_NM == '부암동')
buamdong <- or_buam
bunam_knn <- knnImputation(buamdong[,-c(1,2,38)], 3)
buamdong <- or_buam
buamdong <- buamdong %>% as.data.frame()
set.seed(1234)
x2_random <- sample(1:365, 20, replace = T)
for(i in 1:20) { 
  buamdong[x2_random[i],33:37] <- NA
}
#[5-2-1] KNN Imputation
library(DMwR)
set.seed(1234)
buam_knn <- knnImputation(buamdong[,-c(1,2,38)], 3)
actuals <- or_buam[,-c(1,2,38)]
regr.eval(actuals, buam_knn)

#[5-2-2] MICE
#library(mice)
#md.pattern(buamdong[,-c(1,2,38)])
#set.seed(1234)
#buam_mice <- mice(buamdong[,-c(1,2,38)],25, method = 'rf')
#predicteds_mice <- complete(buam_mice)
#actuals <- or_buam[,-c(1,2,38)]
#regr.eval(actuals, predicteds_mice)
#pmm?
#set.seed(1234)
#buam_mice <- mice(buamdong[,-c(1,2,38)],25, method = 'pmm')
#predicteds_mice <- complete(buam_mice)
#actuals <- or_buam[,-c(1,2,38)]
#regr.eval(actuals, predicteds_mice)
#default
#set.seed(1234)
#buam_mice <- mice(buamdong[,-c(1,2,38)],25)
#predicteds_mice <- complete(buam_mice)
#actuals <- or_buam[,-c(1,2,38)]
#regr.eval(actuals, predicteds_mice)


#[5-2-3]na interpolation 
#set.seed(1234)
#library(zoo)
#buam_inter <- na.spline(buamdong[,-c(1,2,38)])
#actuals <- or_buam[,-c(1,2,38)]
#regr.eval(actuals, buam_inter)

#한번 열두개를 뽑아보자.
interpol_list <- c('가회동', '공릉1동', '공릉2동' , '교남동' , '상계10동', '상계1동' ,'숭인2동', 
                   '월계3동' , '이화동', '종로5.6가동' , '중계2.3동' , '평창동' , '하계1동',
                   '청운효자동', '사직동')
nulldata <- udongnalssi[1,]
nulldata[1,] <- NA
colnames(nulldata) <- c( "STD_YMD" ,"HDONG_NM" , "MAN_FLOW_POP_CNT_0004" , "MAN_FLOW_POP_CNT_0509",
                         "MAN_FLOW_POP_CNT_1014","MAN_FLOW_POP_CNT_1519", "MAN_FLOW_POP_CNT_2024",
                         "MAN_FLOW_POP_CNT_2529","MAN_FLOW_POP_CNT_3034","MAN_FLOW_POP_CNT_3539",
                         "MAN_FLOW_POP_CNT_4044", "MAN_FLOW_POP_CNT_4549","MAN_FLOW_POP_CNT_5054",
                         "MAN_FLOW_POP_CNT_5559", "MAN_FLOW_POP_CNT_6064", "MAN_FLOW_POP_CNT_6569",
                         "MAN_FLOW_POP_CNT_70U","WMAN_FLOW_POP_CNT_0004", "WMAN_FLOW_POP_CNT_0509",
                         "WMAN_FLOW_POP_CNT_1014","WMAN_FLOW_POP_CNT_1519","WMAN_FLOW_POP_CNT_2024",
                         "WMAN_FLOW_POP_CNT_2529", "WMAN_FLOW_POP_CNT_3034","WMAN_FLOW_POP_CNT_3539",
                         "WMAN_FLOW_POP_CNT_4044", "WMAN_FLOW_POP_CNT_4549","WMAN_FLOW_POP_CNT_5054",
                         "WMAN_FLOW_POP_CNT_5559", "WMAN_FLOW_POP_CNT_6064","WMAN_FLOW_POP_CNT_6569",
                         "WMAN_FLOW_POP_CNT_70U",  "pm10", "noise","temp","humi","pm25","x[, 38]")

for(i in interpol_list) { 
  x <- udongnalssi %>% filter(HDONG_NM == i)
  x_inter <- knnImputation(x[,-c(1,2,38)], 3)
  x <- cbind(x[,1:2],x_inter,x[,38])
  nulldata <- rbind(nulldata, x)
}
nulldata <- nulldata[-1,] 

#완벽한 데이터만 뽑아보자. 
perfection <- udongnalssi %>% filter(HDONG_NM != '공릉1동' & HDONG_NM != '공릉2동'& HDONG_NM !='교남동'&
                                       HDONG_NM !='상계10동'& HDONG_NM !='상계1동'& HDONG_NM !='숭인2동'& 
                                       HDONG_NM !='월계3동'& HDONG_NM !='이화동'& HDONG_NM !='종로5.6가동'&
                                       HDONG_NM !='중계2.3동'& HDONG_NM !='평창동'& HDONG_NM !='하계1동'&
                                       HDONG_NM !='청운효자동' &HDONG_NM !='사직동' &HDONG_NM != '가회동')
perfection <- perfection %>% na.omit
colnames(nulldata) <- colnames(perfection)
perfectdata <- rbind(perfection, nulldata)


#신한 머지 안한거
write.csv(perfectdata, '유동인구+날씨.csv', row.names = F)
#신한 머지 한거
setwd('C:/Users/jiho0/Downloads/방학세미나/카드매출데이터')
shinhan <- read.delim('CARD_SPENDING.txt', encoding = 'UTF=8')
upjongcode <- fread('upjongcode.csv') #업종 코드를 추출
gucode <- fread('shinhancode.csv') #지역 코드를 추출
shinhan <- left_join(shinhan, upjongcode, by = c('MCT_CAT_CD' = 'code'))
shinhan <- left_join(shinhan, gucode, by = c('GU_CD' = 'Gu_code', 'DONG_CD' = 'Dong_code'))
shinhan <- shinhan %>% select(-c(GU_CD,DONG_CD,MCT_CAT_CD,SEX_CD,AGE_CD))
lightshinhan <- shinhan %>% dplyr::group_by(STD_DD, Dong,upjong) %>%
  dplyr::summarise(usecount = sum(USE_CNT), useamount = sum(USE_AMT), groupcount = n())

roughjoin <- right_join(lightshinhan, perfectdata, 
                        by = c('STD_DD' = 'STD_YMD', 'Dong' = 'HDONG_NM'))
setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터')

#나이를 재범주화 해주자. 
roughjoin <- roughjoin %>% mutate(male_yoonyun = MAN_FLOW_POP_CNT_0004 + MAN_FLOW_POP_CNT_0509 + 
                                    WMAN_FLOW_POP_CNT_1014 + WMAN_FLOW_POP_CNT_1519, female_yoonyun = WMAN_FLOW_POP_CNT_0004 + 
                                    WMAN_FLOW_POP_CNT_0509 + WMAN_FLOW_POP_CNT_1014 + WMAN_FLOW_POP_CNT_1519, 
                                  male_chungnyun = MAN_FLOW_POP_CNT_2024 + MAN_FLOW_POP_CNT_2529 + MAN_FLOW_POP_CNT_3034 +
                                    WMAN_FLOW_POP_CNT_3539,  female_chungnyun = WMAN_FLOW_POP_CNT_2024 + WMAN_FLOW_POP_CNT_2529 + WMAN_FLOW_POP_CNT_3034 +
                                    WMAN_FLOW_POP_CNT_3539, male_jangnyun = MAN_FLOW_POP_CNT_4044 + MAN_FLOW_POP_CNT_4549 + 
                                    MAN_FLOW_POP_CNT_5054+ MAN_FLOW_POP_CNT_5559, 
                                  female_jangnyun = WMAN_FLOW_POP_CNT_4044 + WMAN_FLOW_POP_CNT_4549 + 
                                    WMAN_FLOW_POP_CNT_5054+ WMAN_FLOW_POP_CNT_5559, male_nonyun = MAN_FLOW_POP_CNT_6064 + 
                                    WMAN_FLOW_POP_CNT_6569 + WMAN_FLOW_POP_CNT_70U, female_nonyun = WMAN_FLOW_POP_CNT_6064 + 
                                    WMAN_FLOW_POP_CNT_6569 + WMAN_FLOW_POP_CNT_70U)
roughjoin <- roughjoin %>% select(-c(contains('FLOW')))
write.csv(roughjoin, '유동인구+날씨+신한카드데이터.csv', row.names = F)

#가벼운데이터를 원한다면! (신한카드 데이터를 merge)
lightshinhan <- shinhan %>% select(-c(SEX_CD, AGE_CD))
lightshinhan <- lightshinhan %>% dplyr::group_by(STD_DD, Dong) %>%
  dplyr::summarise(usecount = sum(USE_CNT), useamount = sum(USE_AMT), groupcount = n())


#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#--------------의도치않은 PCA가 필요하다면..--------------------------------------------
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
df <- perfectdata %>% na.omit()
str(df)
colSums(is.na(df))
diff<-df[,c(33:37)]
corr<-cor(diff)
library(corrplot)
corr
corrplot(corr, method="shade",addshade="all",shade.col=NA,tl.col="red",tl.srt=30,diag=FALSE,
         addCoef.col="black",
         order="FPC")
diff.pca <- prcomp(diff,center = T, scale. = T) 
print(diff.pca) 
plot(diff.pca, type="l")
summary(diff.pca) 
PRC <- as.matrix(diff) %*% diff.pca$rotation
head(PRC)
noteasy <- cbind(df,PRC)
i_am_na<- perfectdata[!complete.cases(perfectdata),]
nadata <- matrix(NA, ncol = 5, nrow = 4016) %>% as.data.frame()
colnames(nadata) <- colnames(PRC)
i_am_na <- cbind(i_am_na,nadata)
fucking_final_pca <- rbind(noteasy, i_am_na)
write.csv(fucking_final_pca,file="df.csv",row.names=FALSE)
lightdata<- left_join(lightshinhan,fucking_final_pca, by = c('STD_DD' = 'STD_YMD', 'Dong' = 'HDONG_NM'))
colSums(is.na(lightdata))

#혹시 모르니 플롯 하나만
before <- udongnalssi[,33:37]
after <- perfectdata[,33:37]

ggplot() + 
  geom_point(data = after,aes(x = log(pm10), y = temp), color = 'red', cex = 2)+
  geom_point(data = before, aes(x = log(pm10), y = temp),color = 'yellow', cex = 2) +
  scale_y_continuous(limits = c(-50, 50)) + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


library(dplyr)
library(readr)
library(ggplot2)
library(catboost)
library(CatEncoders)
library(tibble)
library(data.table)
library(devtools)
library(caret)
devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.15/catboost-R-Windows-0.15.tgz', 
                      args = c("--no-multiarch"))

setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터')
rawdata <- fread('유동인구+날씨+신한카드데이터.csv')
setwd('C:/Users/jiho0/Downloads/방학세미나')
#주말 유무를 팩터처리해주자.
rawdata$holiday <- rawdata$holiday %>% as.integer()
#필요없는 열들을 날려보내자.(구와 날짜)
rawdata <- rawdata %>% select(-c(STD_DD, Gu))

#주중 데이터를 만들어보자.
rawdata <- rawdata %>% filter(holiday == 1)
joojoong1 <- rawdata %>% filter(Dong == '종로1.2.3.4가동' | Dong == '숭인2동' | Dong == '상계3.4동') %>%
  filter(upjong == '유통업')
joojoong2 <- rawdata %>% filter(Dong == '혜화동') %>%
  filter(upjong == '서적문구')
joojoong3 <- rawdata %>% filter(Dong == '창신3동' | Dong == '월계3동' |
                                  Dong == '청운효자동'| Dong == '월계1동' ) %>%
  filter(upjong == '의료기관')
joojoong4 <- rawdata %>% filter(Dong == '상계6.7동' | Dong == '가회동' |
                                  Dong == '하계1동'| Dong == '이화동' ) %>%
  filter(upjong == '음료식품')
joojoong5 <- rawdata %>% filter(Dong == '중계본동' | Dong == '상계2동' |
                                  Dong == '공릉1동'| Dong == '공릉2동' | 
                                  Dong == '부암동'  | Dong == '창신1동') %>%
  filter(upjong == '레저업소')
joojoong6 <- rawdata %>% filter(Dong == '교남동' ) %>%
  filter(upjong == '수리서비스')
joojoong7 <- rawdata %>% filter(Dong == '중계2.3동' ) %>%
  filter(upjong == '문화취미')
joojoong8 <- rawdata %>% filter(Dong == '평창동' | Dong == '상계1동') %>%
  filter(upjong == '요식업소')
joojoong9 <- rawdata %>% filter(Dong == '사직동' ) %>%
  filter(upjong == '자동차정비')
joojoong10 <- rawdata %>% filter(Dong == '상계5동' | Dong == '상계10동' ) %>%
  filter(upjong == '보건위생')
joojoong11 <- rawdata %>% filter(Dong == '종로5.6가동' ) %>%
  filter(upjong == '직물')

joomal <- rbind(joojoong1,joojoong2, joojoong3, joojoong4,joojoong5,joojoong6, joojoong7,
                joojoong8, joojoong9,joojoong10, joojoong11)

write.csv(joomal, 'joomal.csv', row.names = F)


################------------------------------------------------###########################
################------------여기부터 돌려도 무방하다. -------##############################
################------------------------------------------------###########################
library(dplyr)
library(readr)
library(ggplot2)
library(catboost)
library(CatEncoders)
library(tibble)
library(data.table)
library(devtools)
library(caret)
###주말부터 조져보자
setwd('C:/Users/jiho0/Downloads/방학세미나')
joomal <- fread('joomal.csv')
# define categorical variables and encode
cat_clean <- c('upjong')
cat_data <- joomal[,'upjong']
lenc <- sapply(joomal[,'upjong'], function(x) LabelEncoder.fit(x))
for (i in cat_clean){
  cat_data[[i]] <- transform(lenc[[i]], joomal[[i]])
}
joomal <- cbind(cat_data, joomal[,-'upjong'])
#필요없는 열들을 제거하고 y를 떼어주자
joomal <- joomal %>% select(-c(Dong, groupcount, holiday))
target_m_y <- joomal$male_yoonyun
target_f_y <- joomal$female_yoonyun
target_m_c <- joomal$male_chungnyun
target_f_c <- joomal$female_chungnyun
target_m_j <- joomal$male_jangnyun
target_f_j <- joomal$female_jangnyun
target_m_n <- joomal$male_nonyun
target_f_n <- joomal$female_nonyun
joomal <- joomal %>% select(-c(contains('male')))
###########################################################################################
###########################################################################################
#----------------------------------parameter tunning
###########################################################################################
###########################################################################################
set.seed(12345)
x <- joomal
y <- target_m_y

fit_control <- trainControl(method = "cv",
                            number = 4,
                            classProbs = TRUE)
grid <- expand.grid(depth = c(4, 6, 8),
                    learning_rate = 0.1,
                    iterations = 100,
                    l2_leaf_reg = 1e-3,
                    rsm = 0.95,
                    border_count = 64)

report <- train(x, as.factor(make.names(y)),
                method = catboost.caret,
                logging_level = 'Verbose', preProc = NULL,
                tuneGrid = grid, trControl = fit_control)

print(report)

importance <- varImp(report, scale = FALSE)
print(importance)




#--------------------------------[1] 주말 남자 유년기
joomal_m_y <- cbind(target_m_y, joomal)
joomal_m_y <- joomal_m_y[!(joomal_m_y$target_m_y %in% boxplot(joomal_m_y$target_m_y)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_m_y$target_m_y, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_m_y <- joomal_m_y[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_m_y <- joomal_m_y[validation_index,]
#seperate x and y
y_train_m_y <- dataset_m_y %>% select(target_m_y) %>% unlist
X_train_m_y <- dataset_m_y %>% select(-target_m_y)
y_valid_m_y <- validation_m_y %>% select(target_m_y) %>% unlist
X_valid_m_y <- validation_m_y %>% select(-target_m_y)
#Convert the train and test dataset to catboost specific format
train_pool_m_y <- catboost.load_pool(data = X_train_m_y, label = y_train_m_y)
test_pool_m_y <- catboost.load_pool(data = X_valid_m_y, label = y_valid_m_y)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)

model_m_y <- catboost.train(learn_pool = train_pool_m_y ,params = params)
#Predict the output using the catboost predict function.
y_pred_m_y <- catboost.predict(model_m_y,test_pool_m_y)
#RMSE
postResample(y_pred_m_y,validation_m_y$target_m_y)
#rm(joomal_m_y)
#rm(y_train_m_y)
#rm(y_valid_m_y)

#--------------------------------[2] 주말 여자 유년기
joomal_f_y <- cbind(target_f_y, joomal)
joomal_f_y <- joomal_f_y[!(joomal_f_y$target_f_y %in% boxplot(joomal_f_y$target_f_y)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_f_y$target_f_y, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_f_y <- joomal_f_y[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_f_y <- joomal_f_y[validation_index,]
#seperate x and y
y_train_f_y <- dataset_f_y %>% select(target_f_y) %>% unlist
X_train_f_y <- dataset_f_y %>% select(-target_f_y)
y_valid_f_y <- validation_f_y %>% select(target_f_y) %>% unlist
X_valid_f_y <- validation_f_y %>% select(-target_f_y)
#Convert the train and test dataset to catboost specific format
train_pool_f_y <- catboost.load_pool(data = X_train_f_y, label = y_train_f_y)
test_pool_f_y <- catboost.load_pool(data = X_valid_f_y, label = y_valid_f_y)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_f_y <- catboost.train(learn_pool = train_pool_f_y ,params = params)
#Predict the output using the catboost predict function.
y_pred_f_y <- catboost.predict(model_f_y,test_pool_f_y)
#RMSE
postResample(y_pred_f_y,validation_f_y$target_f_y)
#rm(joomal_f_y)
#rm(y_train_f_y)
#rm(y_valid_f_y)

#--------------------------------[3] 주말 남자 청년기
joomal_m_c <- cbind(target_m_c, joomal)
joomal_m_c <- joomal_m_c[!(joomal_m_c$target_m_c %in% boxplot(joomal_m_c$target_m_c)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_m_c$target_m_c, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_m_c <- joomal_m_c[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_m_c <- joomal_m_c[validation_index,]
#seperate x and y
y_train_m_c <- dataset_m_c %>% select(target_m_c) %>% unlist
X_train_m_c <- dataset_m_c %>% select(-target_m_c)
y_valid_m_c <- validation_m_c %>% select(target_m_c) %>% unlist
X_valid_m_c <- validation_m_c %>% select(-target_m_c)
#Convert the train and test dataset to catboost specific format
train_pool_m_c <- catboost.load_pool(data = X_train_m_c, label = y_train_m_c)
test_pool_m_c <- catboost.load_pool(data = X_valid_m_c, label = y_valid_m_c)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_m_c <- catboost.train(learn_pool = train_pool_m_c ,params = params)
#Predict the output using the catboost predict function.
y_pred_m_c <- catboost.predict(model_m_c,test_pool_m_c)
#RMSE
postResample(y_pred_m_c,validation_m_c$target_m_c)
#rm(joomal_m_c)
#rm(y_train_m_c)
#rm(y_valid_m_c)

#--------------------------------[4] 주말 여자 청년기
joomal_f_c <- cbind(target_f_c, joomal)
joomal_f_c <- joomal_f_c[!(joomal_f_c$target_f_c %in% boxplot(joomal_f_c$target_f_c)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_f_c$target_f_c, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_f_c <- joomal_f_c[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_f_c <- joomal_f_c[validation_index,]
#seperate x and y
y_train_f_c <- dataset_f_c %>% select(target_f_c) %>% unlist
X_train_f_c <- dataset_f_c %>% select(-target_f_c)
y_valid_f_c <- validation_f_c %>% select(target_f_c) %>% unlist
X_valid_f_c <- validation_f_c %>% select(-target_f_c)
#Convert the train and test dataset to catboost specific format
train_pool_f_c <- catboost.load_pool(data = X_train_f_c, label = y_train_f_c)
test_pool_f_c <- catboost.load_pool(data = X_valid_f_c, label = y_valid_f_c)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_f_c <- catboost.train(learn_pool = train_pool_f_c ,params = params)
#Predict the output using the catboost predict function.
y_pred_f_c <- catboost.predict(model_f_c,test_pool_f_c)
#RMSE
postResample(y_pred_f_c,validation_f_c$target_f_c)
#rm(joomal_f_c)
#rm(y_train_f_c)
#rm(y_valid_f_c)

#--------------------------------[5] 주말 남자 장년기
joomal_m_j <- cbind(target_m_j, joomal)
joomal_m_j <- joomal_m_j[!(joomal_m_j$target_m_j %in% boxplot(joomal_m_j$target_m_j)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_m_j$target_m_j, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_m_j <- joomal_m_j[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_m_j <- joomal_m_j[validation_index,]
#seperate x and y
y_train_m_j <- dataset_m_j %>% select(target_m_j) %>% unlist
X_train_m_j <- dataset_m_j %>% select(-target_m_j)
y_valid_m_j <- validation_m_j %>% select(target_m_j) %>% unlist
X_valid_m_j <- validation_m_j %>% select(-target_m_j)
#Convert the train and test dataset to catboost specific format
train_pool_m_j <- catboost.load_pool(data = X_train_m_j, label = y_train_m_j)
test_pool_m_j <- catboost.load_pool(data = X_valid_m_j, label = y_valid_m_j)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_m_j <- catboost.train(learn_pool = train_pool_m_j ,params = params)
#Predict the output using the catboost predict function.
y_pred_m_j <- catboost.predict(model_m_j,test_pool_m_j)
#RMSE
postResample(y_pred_m_j,validation_m_j$target_m_j)
#rm(joomal_m_j)
#rm(y_train_m_j)
#rm(y_valid_m_j)

#--------------------------------[6] 주말 여자 장년기
joomal_f_j <- cbind(target_f_j, joomal)
joomal_f_j <- joomal_f_j[!(joomal_f_j$target_f_j %in% boxplot(joomal_f_j$target_f_j)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_f_j$target_f_j, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_f_j <- joomal_f_j[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_f_j <- joomal_f_j[validation_index,]
#seperate x and y
y_train_f_j <- dataset_f_j %>% select(target_f_j) %>% unlist
X_train_f_j <- dataset_f_j %>% select(-target_f_j)
y_valid_f_j <- validation_f_j %>% select(target_f_j) %>% unlist
X_valid_f_j <- validation_f_j %>% select(-target_f_j)
#Convert the train and test dataset to catboost specific format
train_pool_f_j <- catboost.load_pool(data = X_train_f_j, label = y_train_f_j)
test_pool_f_j <- catboost.load_pool(data = X_valid_f_j, label = y_valid_f_j)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_f_j <- catboost.train(learn_pool = train_pool_f_j ,params = params)
#Predict the output using the catboost predict function.
y_pred_f_j <- catboost.predict(model_f_j,test_pool_f_j)
#RMSE
postResample(y_pred_f_j,validation_f_j$target_f_j)
#rm(joomal_f_j)
#rm(y_train_f_j)
#rm(y_valid_f_j)


#--------------------------------[7] 주말 남자 노년기
joomal_m_n <- cbind(target_m_n, joomal)
joomal_m_n <- joomal_m_n[!(joomal_m_n$target_m_n %in% boxplot(joomal_m_n$target_m_n)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_m_n$target_m_n, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_m_n <- joomal_m_n[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_m_n <- joomal_m_n[validation_index,]
#seperate x and y
y_train_m_n <- dataset_m_n %>% select(target_m_n) %>% unlist
X_train_m_n <- dataset_m_n %>% select(-target_m_n)
y_valid_m_n <- validation_m_n %>% select(target_m_n) %>% unlist
X_valid_m_n <- validation_m_n %>% select(-target_m_n)
#Convert the train and test dataset to catboost specific format
train_pool_m_n <- catboost.load_pool(data = X_train_m_n, label = y_train_m_n)
test_pool_m_n <- catboost.load_pool(data = X_valid_m_n, label = y_valid_m_n)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_m_n <- catboost.train(learn_pool = train_pool_m_n ,params = params)
#Predict the output using the catboost predict function.
y_pred_m_n <- catboost.predict(model_m_n,test_pool_m_n)
#RMSE
postResample(y_pred_m_n,validation_m_n$target_m_n)
#rm(joomal_m_n)
#rm(y_train_m_n)
#rm(y_valid_m_n)


#--------------------------------[8] 주말 여자 노년기
joomal_f_n <- cbind(target_f_n, joomal)
joomal_f_n <- joomal_f_n[!(joomal_f_n$target_f_n %in% boxplot(joomal_f_n$target_f_n)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joomal_f_n$target_f_n, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_f_n <- joomal_f_n[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_f_n <- joomal_f_n[validation_index,]
#seperate x and y
y_train_f_n <- dataset_f_n %>% select(target_f_n) %>% unlist
X_train_f_n <- dataset_f_n %>% select(-target_f_n)
y_valid_f_n <- validation_f_n %>% select(target_f_n) %>% unlist
X_valid_f_n <- validation_f_n %>% select(-target_f_n)
#Convert the train and test dataset to catboost specific format
train_pool_f_n <- catboost.load_pool(data = X_train_f_n, label = y_train_f_n)
test_pool_f_n <- catboost.load_pool(data = X_valid_f_n, label = y_valid_f_n)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_f_n <- catboost.train(learn_pool = train_pool_f_n ,params = params)
#Predict the output using the catboost predict function.
y_pred_f_n <- catboost.predict(model_f_n,test_pool_f_n)
#RMSE
postResample(y_pred_f_n,validation_f_n$target_f_n)
#rm(joomal_f_n)
#rm(y_train_f_n)
#rm(y_valid_f_n)

##
target_m_y %>% sd
postResample(y_pred_m_y,validation_m_y$target_m_y)
target_f_y %>% sd
postResample(y_pred_f_y,validation_f_y$target_f_y)
target_m_c %>% sd
postResample(y_pred_m_c,validation_m_c$target_m_c)
target_f_c %>% sd
postResample(y_pred_f_c,validation_f_c$target_f_c)
target_m_j %>% sd
postResample(y_pred_m_j,validation_m_j$target_m_j)
target_f_j %>% sd
postResample(y_pred_f_j,validation_f_j$target_f_j)
target_m_n %>% sd
postResample(y_pred_m_n,validation_m_n$target_m_n)
target_f_n %>% sd
postResample(y_pred_f_n,validation_f_n$target_f_n)

library(dplyr)
library(readr)
library(ggplot2)
library(catboost)
library(CatEncoders)
library(tibble)
library(data.table)
library(devtools)
library(caret)
devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.23/catboost-R-Windows-0.23.tgz',
                      args = c("--no-multiarch"))

setwd('C:/Users/jiho0/Downloads/방학세미나/환경기상데이터')
rawdata <- fread('유동인구+날씨+신한카드데이터.csv')
setwd('C:/Users/jiho0/Downloads/방학세미나')
#주중 유무를 팩터처리해주자.
rawdata$holiday <- rawdata$holiday %>% as.integer()
#필요없는 열들을 날려보내자.(구와 날짜)
rawdata <- rawdata %>% select(-c(STD_DD, Gu))

#주중 데이터를 만들어보자.
rawdata <- rawdata %>% filter(holiday == 1)
joojoong1 <- rawdata %>% filter(Dong == '종로1.2.3.4가동' | Dong == '숭인2동' | Dong == '상계3.4동') %>%
  filter(upjong == '유통업')
joojoong2 <- rawdata %>% filter(Dong == '혜화동') %>%
  filter(upjong == '서적문구')
joojoong3 <- rawdata %>% filter(Dong == '창신3동' | Dong == '월계3동' |
                                  Dong == '청운효자동'| Dong == '월계1동' ) %>%
  filter(upjong == '의료기관')
joojoong4 <- rawdata %>% filter(Dong == '상계6.7동' | Dong == '가회동' |
                                  Dong == '하계1동'| Dong == '이화동' ) %>%
  filter(upjong == '음료식품')
joojoong5 <- rawdata %>% filter(Dong == '중계본동' | Dong == '상계2동' |
                                  Dong == '공릉1동'| Dong == '공릉2동' | 
                                  Dong == '부암동'  | Dong == '창신1동') %>%
  filter(upjong == '레저업소')
joojoong6 <- rawdata %>% filter(Dong == '교남동' ) %>%
  filter(upjong == '수리서비스')
joojoong7 <- rawdata %>% filter(Dong == '중계2.3동' ) %>%
  filter(upjong == '문화취미')
joojoong8 <- rawdata %>% filter(Dong == '평창동' | Dong == '상계1동') %>%
  filter(upjong == '요식업소')
joojoong9 <- rawdata %>% filter(Dong == '사직동' ) %>%
  filter(upjong == '자동차정비')
joojoong10 <- rawdata %>% filter(Dong == '상계5동' | Dong == '상계10동' ) %>%
  filter(upjong == '보건위생')
joojoong11 <- rawdata %>% filter(Dong == '종로5.6가동' ) %>%
  filter(upjong == '직물')

joomal <- rbind(joojoong1,joojoong2, joojoong3, joojoong4,joojoong5,joojoong6, joojoong7,
                joojoong8, joojoong9,joojoong10, joojoong11)

write.csv(joomal, 'joomal.csv', row.names = F)


################------------------------------------------------###########################
################------------여기부터 돌려도 무방하다. -------##############################
################------------------------------------------------###########################
library(dplyr)
library(readr)
library(ggplot2)
library(catboost)
library(CatEncoders)
library(tibble)
library(data.table)
library(devtools)
library(caret)
###주중부터 조져보자
setwd('C:/Users/jiho0/Downloads/방학세미나')
joojoong <- fread('joojoong.csv')


# define categorical variables and encode
cat_clean <- c('upjong')
cat_data <- joojoong[,'upjong']
lenc <- sapply(joojoong[,'upjong'], function(x) LabelEncoder.fit(x))
for (i in cat_clean){
  cat_data[[i]] <- transform(lenc[[i]], joojoong[[i]])
}
joojoong <- cbind(cat_data, joojoong[,-'upjong'])
#필요없는 열들을 제거하고 y를 떼어주자
joojoong <- joojoong %>% select(-c(Dong,STD_DD,Gu, groupcount))
target_j_m_y <- joojoong$male_yoonyun
target_j_f_y <- joojoong$female_yoonyun
target_j_m_c <- joojoong$male_chungnyun
target_j_f_c <- joojoong$female_chungnyun
target_j_m_j <- joojoong$male_jangnyun
target_j_f_j <- joojoong$female_jangnyun
target_j_m_n <- joojoong$male_nonyun
target_j_f_n <- joojoong$female_nonyun
joojoong <- joojoong %>% select(-c(contains('male')))
###########################################################################################
###########################################################################################
#----------------------------------parameter tunning
###########################################################################################
###########################################################################################
set.seed(12345)
x <- joojoong
y <- target_j_m_y

fit_control <- trainControl(method = "cv",
                            number = 4,
                            classProbs = TRUE)
grid <- expand.grid(depth = c(4, 6, 8),
                    learning_rate = 0.1,
                    iterations = 100,
                    l2_leaf_reg = 1e-3,
                    rsm = 0.95,
                    border_count = 64)

report <- train(x, as.factor(make.names(y)),
                method = catboost.caret,
                logging_level = 'Verbose', preProc = NULL,
                tuneGrid = grid, trControl = fit_control)

print(report)

importance <- varImp(report, scale = FALSE)
print(importance)




#--------------------------------[1] 주중 남자 유년기
joojoong_j_m_y <- cbind(target_j_m_y, joojoong)
joojoong_j_m_y <- joojoong_j_m_y[!(joojoong_j_m_y$target_j_m_y %in% boxplot(joojoong_j_m_y$target_j_m_y)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_m_y$target_j_m_y, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_m_y <- joojoong_j_m_y[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_m_y <- joojoong_j_m_y[validation_index,]
#seperate x and y
y_train_j_m_y <- dataset_j_m_y %>% select(target_j_m_y) %>% unlist
X_train_j_m_y <- dataset_j_m_y %>% select(-target_j_m_y)
y_valid_j_m_y <- validation_j_m_y %>% select(target_j_m_y) %>% unlist
X_valid_j_m_y <- validation_j_m_y %>% select(-target_j_m_y)
#Convert the train and test dataset to catboost specific format
train_pool_j_m_y <- catboost.load_pool(data = X_train_j_m_y, label = y_train_j_m_y)
test_pool_j_m_y <- catboost.load_pool(data = X_valid_j_m_y, label = y_valid_j_m_y)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_m_y <- catboost.train(learn_pool = train_pool_j_m_y ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_m_y <- catboost.predict(model_j_m_y,test_pool_j_m_y)
#RMSE
postResample(y_pred_j_m_y,validation_j_m_y$target_j_m_y)
#rm(joojoong_j_m_y)
#rm(y_train_j_m_y)
#rm(y_valid_j_m_y)

#--------------------------------[2] 주중 여자 유년기
joojoong_j_f_y <- cbind(target_j_f_y, joojoong)
joojoong_j_f_y <- joojoong_j_f_y[!(joojoong_j_f_y$target_j_f_y %in% boxplot(joojoong_j_f_y$target_j_f_y)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_f_y$target_j_f_y, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_f_y <- joojoong_j_f_y[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_f_y <- joojoong_j_f_y[validation_index,]
#seperate x and y
y_train_j_f_y <- dataset_j_f_y %>% select(target_j_f_y) %>% unlist
X_train_j_f_y <- dataset_j_f_y %>% select(-target_j_f_y)
y_valid_j_f_y <- validation_j_f_y %>% select(target_j_f_y) %>% unlist
X_valid_j_f_y <- validation_j_f_y %>% select(-target_j_f_y)
#Convert the train and test dataset to catboost specific format
train_pool_j_f_y <- catboost.load_pool(data = X_train_j_f_y, label = y_train_j_f_y)
test_pool_j_f_y <- catboost.load_pool(data = X_valid_j_f_y, label = y_valid_j_f_y)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_f_y <- catboost.train(learn_pool = train_pool_j_f_y ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_f_y <- catboost.predict(model_j_f_y,test_pool_j_f_y)
#RMSE
postResample(y_pred_j_f_y,validation_j_f_y$target_j_f_y)
#rm(joojoong_j_f_y)
#rm(y_train_j_f_y)
#rm(y_valid_j_f_y)

#--------------------------------[3] 주중 남자 청년기
joojoong_j_m_c <- cbind(target_j_m_c, joojoong)
joojoong_j_m_c <- joojoong_j_m_c[!(joojoong_j_m_c$target_j_m_c %in% boxplot(joojoong_j_m_c$target_j_m_c)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_m_c$target_j_m_c, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_m_c <- joojoong_j_m_c[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_m_c <- joojoong_j_m_c[validation_index,]
#seperate x and y
y_train_j_m_c <- dataset_j_m_c %>% select(target_j_m_c) %>% unlist
X_train_j_m_c <- dataset_j_m_c %>% select(-target_j_m_c)
y_valid_j_m_c <- validation_j_m_c %>% select(target_j_m_c) %>% unlist
X_valid_j_m_c <- validation_j_m_c %>% select(-target_j_m_c)
#Convert the train and test dataset to catboost specific format
train_pool_j_m_c <- catboost.load_pool(data = X_train_j_m_c, label = y_train_j_m_c)
test_pool_j_m_c <- catboost.load_pool(data = X_valid_j_m_c, label = y_valid_j_m_c)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_m_c <- catboost.train(learn_pool = train_pool_j_m_c ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_m_c <- catboost.predict(model_j_m_c,test_pool_j_m_c)
#RMSE
postResample(y_pred_j_m_c,validation$target_j_m_c)
#rm(joojoong_j_m_c)
#rm(y_train_j_m_c)
#rm(y_valid_j_m_c)

#--------------------------------[4] 주중 여자 청년기
joojoong_j_f_c <- cbind(target_j_f_c, joojoong)
joojoong_j_f_c <- joojoong_j_f_c[!(joojoong_j_f_c$target_j_f_c %in% boxplot(joojoong_j_f_c$target_j_f_c)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_f_c$target_j_f_c, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_f_c <- joojoong_j_f_c[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_f_c <- joojoong_j_f_c[validation_index,]
#seperate x and y
y_train_j_f_c <- dataset_j_f_c %>% select(target_j_f_c) %>% unlist
X_train_j_f_c <- dataset_j_f_c %>% select(-target_j_f_c)
y_valid_j_f_c <- validation_j_f_c %>% select(target_j_f_c) %>% unlist
X_valid_j_f_c <- validation_j_f_c %>% select(-target_j_f_c)
#Convert the train and test dataset to catboost specific format
train_pool_j_f_c <- catboost.load_pool(data = X_train_j_f_c, label = y_train_j_f_c)
test_pool_j_f_c <- catboost.load_pool(data = X_valid_j_f_c, label = y_valid_j_f_c)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_f_c <- catboost.train(learn_pool = train_pool_j_f_c ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_f_c <- catboost.predict(model_j_f_c,test_pool_j_f_c)
#RMSE
postResample(y_pred_j_f_c,validation$target_j_f_c)
#rm(joojoong_j_f_c)
#rm(y_train_j_f_c)
#rm(y_valid_j_f_c)

#--------------------------------[5] 주중 남자 장년기
joojoong_j_m_j <- cbind(target_j_m_j, joojoong)
joojoong_j_m_j <- joojoong_j_m_j[!(joojoong_j_m_j$target_j_m_j %in% boxplot(joojoong_j_m_j$target_j_m_j)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_m_j$target_j_m_j, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_m_j <- joojoong_j_m_j[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_m_j <- joojoong_j_m_j[validation_index,]
#seperate x and y
y_train_j_m_j <- dataset_j_m_j %>% select(target_j_m_j) %>% unlist
X_train_j_m_j <- dataset_j_m_j %>% select(-target_j_m_j)
y_valid_j_m_j <- validation_j_m_j %>% select(target_j_m_j) %>% unlist
X_valid_j_m_j <- validation_j_m_j %>% select(-target_j_m_j)
#Convert the train and test dataset to catboost specific format
train_pool_j_m_j <- catboost.load_pool(data = X_train_j_m_j, label = y_train_j_m_j)
test_pool_j_m_j <- catboost.load_pool(data = X_valid_j_m_j, label = y_valid_j_m_j)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_m_j <- catboost.train(learn_pool = train_pool_j_m_j ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_m_j <- catboost.predict(model_j_m_j,test_pool_j_m_j)
#RMSE
postResample(y_pred_j_m_j,validation$target_j_m_j)
#rm(joojoong_j_m_j)
#rm(y_train_j_m_j)
#rm(y_valid_j_m_j)

#--------------------------------[6] 주중 여자 장년기
joojoong_j_f_j <- cbind(target_j_f_j, joojoong)
joojoong_j_f_j <- joojoong_j_f_j[!(joojoong_j_f_j$target_j_f_j %in% boxplot(joojoong_j_f_j$target_j_f_j)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_f_j$target_j_f_j, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_f_j <- joojoong_j_f_j[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_f_j <- joojoong_j_f_j[validation_index,]
#seperate x and y
y_train_j_f_j <- dataset_j_f_j %>% select(target_j_f_j) %>% unlist
X_train_j_f_j <- dataset_j_f_j %>% select(-target_j_f_j)
y_valid_j_f_j <- validation_j_f_j %>% select(target_j_f_j) %>% unlist
X_valid_j_f_j <- validation_j_f_j %>% select(-target_j_f_j)
#Convert the train and test dataset to catboost specific format
train_pool_j_f_j <- catboost.load_pool(data = X_train_j_f_j, label = y_train_j_f_j)
test_pool_j_f_j <- catboost.load_pool(data = X_valid_j_f_j, label = y_valid_j_f_j)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_f_j <- catboost.train(learn_pool = train_pool_j_f_j ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_f_j <- catboost.predict(model_j_f_j,test_pool_j_f_j)
#RMSE
postResample(y_pred_j_f_j,validation$target_j_f_j)
#rm(joojoong_j_f_j)
#rm(y_train_j_f_j)
#rm(y_valid_j_f_j)


#--------------------------------[7] 주중 남자 노년기
joojoong_j_m_n <- cbind(target_j_m_n, joojoong)
joojoong_j_m_n <- joojoong_j_m_n[!(joojoong_j_m_n$target_j_m_n %in% boxplot(joojoong_j_m_n$target_j_m_n)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_m_n$target_j_m_n, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_m_n <- joojoong_j_m_n[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_m_n <- joojoong_j_m_n[validation_index,]
#seperate x and y
y_train_j_m_n <- dataset_j_m_n %>% select(target_j_m_n) %>% unlist
X_train_j_m_n <- dataset_j_m_n %>% select(-target_j_m_n)
y_valid_j_m_n <- validation_j_m_n %>% select(target_j_m_n) %>% unlist
X_valid_j_m_n <- validation_j_m_n %>% select(-target_j_m_n)
#Convert the train and test dataset to catboost specific format
train_pool_j_m_n <- catboost.load_pool(data = X_train_j_m_n, label = y_train_j_m_n)
test_pool_j_m_n <- catboost.load_pool(data = X_valid_j_m_n, label = y_valid_j_m_n)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_m_n <- catboost.train(learn_pool = train_pool_j_m_n ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_m_n <- catboost.predict(model_j_m_n,test_pool_j_m_n)
#RMSE
postResample(y_pred_j_m_n,validation$target_j_m_n)
#rm(joojoong_j_m_n)
#rm(y_train_j_m_n)
#rm(y_valid_j_m_n)


#--------------------------------[8] 주중 여자 노년기
joojoong_j_f_n <- cbind(target_j_f_n, joojoong)
joojoong_j_f_n <- joojoong_j_f_n[!(joojoong_j_f_n$target_j_f_n %in% boxplot(joojoong_j_f_n$target_j_f_n)$out)]
#train test 분리 - 유년 남자
set.seed(7)
validation_index <- createDataPartition(joojoong_j_f_n$target_j_f_n, p=0.80, list=FALSE)
# select 20% of the data for validation
validation_j_f_n <- joojoong_j_f_n[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset_j_f_n <- joojoong_j_f_n[validation_index,]
#seperate x and y
y_train_j_f_n <- dataset_j_f_n %>% select(target_j_f_n) %>% unlist
X_train_j_f_n <- dataset_j_f_n %>% select(-target_j_f_n)
y_valid_j_f_n <- validation_j_f_n %>% select(target_j_f_n) %>% unlist
X_valid_j_f_n <- validation_j_f_n %>% select(-target_j_f_n)
#Convert the train and test dataset to catboost specific format
train_pool_j_f_n <- catboost.load_pool(data = X_train_j_f_n, label = y_train_j_f_n)
test_pool_j_f_n <- catboost.load_pool(data = X_valid_j_f_n, label = y_valid_j_f_n)
#parameter
params <- list(iterations=1000,
               learning_rate=0.1,
               depth=7,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)
model_j_f_n <- catboost.train(learn_pool = train_pool_j_f_n ,params = params)
#Predict the output using the catboost predict function.
y_pred_j_f_n <- catboost.predict(model_j_f_n,test_pool_j_f_n)
#RMSE
postResample(y_pred_j_f_n,validation$target_j_f_n)
#rm(joojoong_j_f_n)
#rm(y_train_j_f_n)
#rm(y_valid_j_f_n)

##
target_j_m_y %>% sd
postResample(y_pred_j_m_y,validation_j_m_y$target_j_m_y)
target_j_f_y %>% sd
postResample(y_pred_j_f_y,validation_j_f_y$target_j_f_y)
target_j_m_c %>% sd
postResample(y_pred_j_m_c,validation_j_m_c$target_j_m_c)
target_j_f_c %>% sd
postResample(y_pred_j_f_c,validation_j_f_c$target_j_f_c)
target_j_m_j %>% sd
postResample(y_pred_j_m_j,validation_j_m_j$target_j_m_j)
target_j_f_j %>% sd
postResample(y_pred_j_f_j,validation_j_f_j$target_j_f_j)
target_j_m_n %>% sd
postResample(y_pred_j_m_n,validation_j_m_n$target_j_m_n)
target_j_f_n %>% sd
postResample(y_pred_j_f_n,validation_j_f_n$target_j_f_n)

