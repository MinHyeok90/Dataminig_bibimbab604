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
rainy_days <- length(365)
for(i in 1:365){
  day<-data1$강우량[index_per_day[i]:(index_per_day[i]+23)]
  if(any(day))
    rainy_days[i] <- 1
  else
    rainy_days[i] <- 0
}

# 저장
write.csv(rainy_days,file="rainy_days.csv")
