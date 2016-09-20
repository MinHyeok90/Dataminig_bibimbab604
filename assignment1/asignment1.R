# 
#   [과제 1] 날씨 예측하기
#   수업시간에 수행한 예제를 참고하여, 
#   날씨를 예측(비가 온다, 안온다)하는 정확도가 가장 높은 k값을 구해 보세요.
#   또한, 첨부된 파일은 서울과 부산의 날씨 데이터로, 데이터를 잘 살펴보시고 
#   평균 온도 및 강우량등 다양한 분석 작업(제한 없음)을 수행해 보십시요.
#   
#   *참고사항
#   파일 읽기 : data1<-read.csv("파일이름.csv"), 
#   필드 액세스 : data1$월, data1$강우량
#   
#   * 제출기한 : 2016년 9월 21일 17시
#   * 제출방법 : 보고서 작성(양식 없음)하여 im.greenjoa@gmail.com으로 제출할 것
#   * 평가기준 : 자동으로 윈도우크기(k) 결정, 다양한 의미있는 분석작업, 보고서  
#   

#mac에서 파일 로딩
setwd("/Users/Kimminhyeok/Documents/Datamining/assignment1")
getwd()
data1 <- read.csv("seoul.csv",fileEncoding = "CP949")

# 전처리 : 강우량단위 24시간 -> 날짜
# 설명
#   seoul.csv에 있는 row는 날짜당 시간별로 8760개가 존재한다. 이것을 365일 날짜별로 비 온날, 안 온날로 구분한 벡터로 축소한다.
#   index_per_day : 날짜가 시작하는 index 번호나열 벡터.
#   rainy_days : 비온 날을 1, 0으로 구분해 저장할 365짜리 벡터.
#     1.강우량을 24개 단위로 나누어읽는다.(날짜단위)
#     2.24개의 row중 하나라도 0이 아닌 데이터가 있는지 확인한다 if(any(day))
#       2.1 비온시간이 존재한다면 해당 날은 비가 온 날로 표시 rainy_days[i] <- 1
#       2.2 비온시간이 없다면 해당날은 비가 안 온 날로 표시 rainy_days[i] <- 0
index_per_day <- seq(from=1,to=8760,by=24)
rainy_days <- vector(length=365)
for(i in 1:365){
  day<-data1$강우량[index_per_day[i]:(index_per_day[i]+23)]
  if(any(day))
    rainy_days[i] <- 1
  else
    rainy_days[i] <- 0
}

# 저장
write.csv(rainy_days,file="rainy_days.csv")

#예측정확도계산1
predict1 <- function(x,k){
  #x:날씨데이터, k:윈도우 길이
  n <- length(x)  #데이터 총 길이
  k2 <- k/2       #윈도우 길이의 반
  pred <- vector(length = n-k)  #예측값 저장할 벡터 : 윈도우 크기만큼 덜 필요.
  for(i in 1:(n-k)){
    if(sum(x[i:(i+(k-1))]) >= k2)
      pred[i] <- 1
    else
      pred[i] <- 0
  }
  return(mean(abs(pred-x[(k+1):n])))
}

#예측정확도계산2
predict2 <- function(x,k){
  n <- length(x)
  k2 <- k/2
  pred <- vector(length = n-k)
  csx <- c(0,cumsum(x))
  for(i in 1:(n-k)){
    if(csx[i+k] - csx[i] >= k2)
      pred[i] <- 1
    else
      pred[i] <- 0
  }
  return(mean(abs(pred-x[(k+1):n])))
}

# window 크기에 따른 정확도 결과
rate1 <- c(length=100)
for(i in 3:100){ rate1[i-2] <- predict2(rainy_days,i) }
rate1

#강우량, 적설량, 건구온도, 기압, 상대습도, 풍속, 이슬점 온도, 공기밀도, 적설량, 운량
#전체평균 구하기
mean(data1$건구온도)

#일별 평균구하기
means_func <- function(){
  month1 <- vector(length=365)
  day1 <- vector(length=365)
  mean1 <- vector(length=365)
  mean2 <- vector(length=365)
  mean3 <- vector(length=365)
  m1max <- 0
  m2max <- 0
  m3max <- 0
  for(i in 1:365){
    month1[i] <- data1$월[index_per_day[i]]
    day1[i] <- data1$일[index_per_day[i]]
    #아래에서 추출하고 싶은 column명을 작성하세요.
    one_day1 <- data1$강우량[index_per_day[i]:(index_per_day[i]+23)]
    one_day2<- data1$건구온도[index_per_day[i]:(index_per_day[i]+23)]
    one_day3<- data1$운량[index_per_day[i]:(index_per_day[i]+23)]
    mean1[i] <- mean(one_day1)
    mean2[i] <- mean(one_day2)
    mean3[i] <- mean(one_day3)
    if (m1max < mean1[i]) m1max<-mean1[i]
    if (m2max < mean2[i]) m2max<-mean2[i]
    if (m3max < mean3[i]) m3max<-mean3[i]
  }
  return(data.frame(month1, day1, mean1/m1max, mean2/m2max, mean3/m3max))
}
means_func()

