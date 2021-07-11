---
title: "EDA_CODE"
date: "2021-07-11"
output: 
  html_document:
    fig_width: 14
    fig_height: 10
    fig.align : 'center'
    toc: yes
    number_sections : yes
    code_folding: show
    keep_md: true
---

<hr>



<style type="text/css">
.main-container {
  max-width: 1200px;
  margin-left: auto;
  margin-right: auto;
}
</style>


# Load package


```r
rm(list=ls())

#load packages
library(knitr)
library(tidyverse)
library(lubridate)
library(janitor)
library(magrittr)
library(DataExplorer)
library(skimr)
library(DT)
library(gt)
library(data.table)
library(zoo)
library(ggcorrplot)

options(scipen=999) # turn-off scientific notation like 1e+48
options(tibble.width = Inf)
```

<hr>

<div style="margin-bottom:60px;">
</div>

# Import Data 

* [데이터출처]

  * 기상관측(ASOS): https://data.kma.go.kr/data/grnd/selectAsosRltmList.do?pgmNo=36 (활용x)
  
  * 황사관측(PM10): https://data.kma.go.kr/data/climate/selectDustRltmList.do?pgmNo=68 (활용x)
  
  * 공휴일(API): https://www.data.go.kr/iim/api/selectAPIAcountView.do


```r
# Dacon data
train <- read_csv("data/train.csv")
test <- read_csv("data/test.csv")
# submission <- read_csv("data/sample_submission.csv")

# # Weather
# weather <- read_csv("data/OBS_ASOS_DD_20210708161107.csv", locale=locale('ko',encoding='euc-kr'))
# 
# # Fine dust
# dust <- read_csv("data/OBS_부유분진_DD_20210708161541.csv", locale=locale('ko',encoding='euc-kr'))

# Holiday
holiday <- read_csv("data/holidays.csv")
```

<hr>

<div style="margin-bottom:60px;">
</div>

# Data pre-processing


```r
train %<>% rename(
  Date      = `일자`,
  Dayofweek = `요일`,
  Total     = `본사정원수`,
  Vacation  = `본사휴가자수`,
  Business  = `본사출장자수`,
  Overtime  = `본사시간외근무명령서승인건수`,
  Home      = `현본사소속재택근무자수`,
  Breakfast = `조식메뉴`,
  Lunch     = `중식메뉴`,
  Dinner    = `석식메뉴`,
  N_lunch   = `중식계`,
  N_dinner  = `석식계`
)

test %<>% rename(
  Date      = `일자`,
  Dayofweek = `요일`,
  Total     = `본사정원수`,
  Vacation  = `본사휴가자수`,
  Business  = `본사출장자수`,
  Overtime  = `본사시간외근무명령서승인건수`,
  Home      = `현본사소속재택근무자수`,
  Breakfast = `조식메뉴`,
  Lunch     = `중식메뉴`,
  Dinner    = `석식메뉴`
)

# submission <- submission %>% 
#   rename(Date = `일자`,
#          Sum_lunch = `중식계`,
#          Sum_dinner = `석식계`)

# weather <- weather %>% 
#   select(-`지점`, -`지점명`, -`일 최심신적설(cm)`) %>%  # 일 최심신적설(cm) 전부 결측값
#   rename(Date = `일시`,
#          Temperature = `평균기온(°C)`,
#          Precipitation = `일강수량(mm)`,
#          Humidity = `평균 상대습도(%)`) 
# 
# dust <- dust %>% 
#   select(-`지점`, -`지점명`) %>% 
#   rename(Date = `일시`,
#          Dust_concentration = `일 미세먼지 농도(㎍/㎥)`)

# 주말 리스트
## 1년 일자를 생성하고 요일 표시하기
# 시작일 지정
start_date <- as.Date("2016-01-01")
# 종료일 지정
end_date <- as.Date("2021-06-30")

# 일단위로 일련의 날짜 생성하기
df <- data.frame(seq(as.Date(start_date), as.Date(end_date), by=1))
names(df) <- "Date"
df$Holiday_name <- weekdays(as.Date(df$Date))

myweek = df[(df$Holiday_name == '토요일') | (df$Holiday_name == "일요일"), ]

# Holiday feature (normal day:1, before holiday:2, after holiday:3, sandwichday:4)
`%!in%` = Negate(`%in%`)

sandwich_set <- holiday %>% 
  rbind(myweek) %>% 
  arrange(Date) %>% 
  mutate(Before_holiday = ifelse(Holiday_name %!in% c("토요일", "일요일"), Date-days(1), NA),
         Before_holiday = as.Date(Before_holiday)) %>% 
  mutate(After_holiday = ifelse(Holiday_name %!in% c("토요일", "일요일"), Date+days(1), NA),
         After_holiday = as.Date(After_holiday)) %>% 
  mutate(interval = ymd(Date) - ymd(lag(Date))) %>% 
  mutate(interval = as.numeric(interval)) %>% 
  # filter(interval == 1 & lag(interval) == 1 | interval == 1 & lead(interval) == 1 | interval == 2) %>% 
  mutate(Sandwich_day = ifelse(interval == 2, Date-days(1), NA)) %>% 
  mutate(Sandwich_day = as.Date(Sandwich_day))

before_holiday <- sandwich_set %>% 
  select(Before_holiday, After_holiday, Sandwich_day) %>% 
  gather(key = Holiday_feature, value = value, 1:3, na.rm = T) %>% 
  filter(Holiday_feature == "Before_holiday") %>% 
  select(Date = value, Before_holiday = Holiday_feature) %>% 
  mutate(Before_holiday = 1)

after_holiday <- sandwich_set %>% 
  select(Before_holiday, After_holiday, Sandwich_day) %>% 
  gather(key = Holiday_feature, value = value, 1:3, na.rm = T) %>% 
  filter(Holiday_feature == "After_holiday") %>% 
  select(Date = value, After_holiday = Holiday_feature) %>% 
  mutate(After_holiday = 1)

sandwich_day <- sandwich_set %>% 
  select(Before_holiday, After_holiday, Sandwich_day) %>% 
  gather(key = Holiday_feature, value = value, 1:3, na.rm = T) %>% 
  filter(Holiday_feature == "Sandwich_day") %>% 
  select(Date = value, Sandwich_day = Holiday_feature) %>% 
  mutate(Sandwich_day = 1)

# Join with train set and test set
train_new <- train %>% 
  left_join(before_holiday, by = "Date") %>% 
  mutate(Before_holiday = ifelse(is.na(Before_holiday), 0, Before_holiday)) %>% 
  left_join(after_holiday, by = "Date") %>% 
  mutate(After_holiday = ifelse(is.na(After_holiday), 0, After_holiday)) %>% 
  left_join(sandwich_day, by = "Date") %>% 
  mutate(Sandwich_day = ifelse(is.na(Sandwich_day), 0, Sandwich_day))

test_new <- test %>% 
  left_join(before_holiday, by = "Date") %>% 
  mutate(Before_holiday = ifelse(is.na(Before_holiday), 0, Before_holiday)) %>% 
  left_join(after_holiday, by = "Date") %>% 
  mutate(After_holiday = ifelse(is.na(After_holiday), 0, After_holiday)) %>% 
  left_join(sandwich_day, by = "Date") %>% 
  mutate(Sandwich_day = ifelse(is.na(Sandwich_day), 0, Sandwich_day))


# mutate In_worker(possible people having meal)
train_new %<>% 
  mutate(Net_total = Total - (Vacation+Business+Home))

test_new %<>% 
  mutate(Net_total = Total - (Vacation+Business+Home))

# train_new %>% write_csv("train_new.csv")
# test_new %>% write_csv("test_new.csv")
```

<hr>

<div style="margin-bottom:60px;">
</div>

# Data EDA


```r
train_new <- read_csv("train_new.csv")
test_new <- read_csv("test_new.csv")

# Missing check
train_new %>% 
  plot_missing(
    missing_only = TRUE
  )
```

![](EDA_CODE_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
train_new %>% 
  profile_missing()
```

```
## # A tibble: 16 x 3
##    feature        num_missing pct_missing
##    <fct>                <int>       <dbl>
##  1 Date                     0     0      
##  2 Dayofweek                0     0      
##  3 Total                    0     0      
##  4 Vacation                 0     0      
##  5 Business                 0     0      
##  6 Overtime                 0     0      
##  7 Home                     0     0      
##  8 Breakfast                0     0      
##  9 Lunch                    0     0      
## 10 Dinner                   4     0.00332
## 11 N_lunch                  0     0      
## 12 N_dinner                 0     0      
## 13 Before_holiday           0     0      
## 14 After_holiday            0     0      
## 15 Sandwich_day             0     0      
## 16 Net_total                0     0
```

```r
train_new %>% 
  filter(is.na(Dinner))
```

```
## # A tibble: 4 x 16
##   Date       Dayofweek Total Vacation Business Overtime  Home
##   <date>     <chr>     <dbl>    <dbl>    <dbl>    <dbl> <dbl>
## 1 2018-04-25 수         2714       66      285        0     0
## 2 2018-05-30 수         2721       80      281        0     0
## 3 2018-07-25 수         2704      226      256        1     0
## 4 2018-09-19 수         2763       77      288        0     0
##   Breakfast                                                                     
##   <chr>                                                                         
## 1 모닝롤/롤케익  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 아욱된장국  감자채피망볶음  포기김치 (김치:국내산)~
## 2 모닝롤/꿀호떡  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 맑은만두육개장  참나물땅콩무침  포기김치 (김치:국내산)~
## 3 모닝롤/찐빵  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 새알만두국  애호박나물  포기김치 (김치:국내산)~
## 4 모닝롤/오렌지빵  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 떡국  숙주나물  포기김치 (김치:국내산)~
##   Lunch                                                                         
##   <chr>                                                                         
## 1 비빔밥 (쌀:국내산) 유부장국  오징어튀김  떡밤초  요플레  포기김치 (김치:국내산)~
## 2 콩나물밥 (쌀:국내산) 유부장국  수제돈가스  파스타샐러드  무생채  포기김치 (김치:국내산)~
## 3 쌀밥/잡곡밥 (쌀:국내산) 쇠고기샤브국  유린기  사각어묵볶음  오이사과생채  포기김치 (김치:국내산)~
## 4 카레덮밥 (쌀:국내산) 유부장국  감자프리타타  메밀전병만두  쨔샤이무침/과일  포기김치 (김치:국내산)~
##   Dinner N_lunch N_dinner Before_holiday After_holiday Sandwich_day Net_total
##   <chr>    <dbl>    <dbl>          <dbl>         <dbl>        <dbl>     <dbl>
## 1 <NA>       851        0              0             0            0      2363
## 2 <NA>       876        0              0             0            0      2360
## 3 <NA>       760        0              0             0            0      2222
## 4 <NA>       833        0              0             0            0      2398
```

```r
# Missing Imputation
# train <- train %>% 
#   mutate(Precipitation = ifelse(is.na(Precipitation), 0, Precipitation)) %>% 
#   mutate(Temperature = ifelse(is.na(Temperature), (lag(Temperature)+lead(Temperature))/2, Temperature)) %>% 
#   mutate(Humidity = ifelse(is.na(Humidity), (lag(Humidity)+lead(Humidity))/2, Humidity)) %>% 
#   mutate(Dust_concentration = ifelse(is.na(Dust_concentration), na.approx(Dust_concentration), Dust_concentration))
  # filter(!is.na(Dinner_menu))

# Histogram
train_new %>% 
  plot_histogram()
```

![](EDA_CODE_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
# Outlier check
train_new %>% 
  select(Vacation, Net_total, Overtime, Business, Home, Total, N_lunch, N_dinner) %>% 
  gather(key = Feature, value = value, 1:8) %>% 
  ggplot() + 
  aes(x = Feature, y = value) +
  geom_boxplot(fill = "#dee2e7") +
  theme_bw() +
  facet_wrap(vars(Feature), scales = "free")
```

![](EDA_CODE_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

```r
train_new %>% 
  filter(N_lunch <= quantile(N_lunch, .25) - IQR(N_lunch)*1.5)
```

```
## # A tibble: 3 x 16
##   Date       Dayofweek Total Vacation Business Overtime  Home
##   <date>     <chr>     <dbl>    <dbl>    <dbl>    <dbl> <dbl>
## 1 2017-12-28 목         2665      240      260       55     0
## 2 2018-12-24 월         2846      894      159       26     0
## 3 2018-12-31 월         2846     1085      132       28     0
##   Breakfast                                                                     
##   <chr>                                                                         
## 1 모닝롤/치즈프레즐  우유/주스 스크램블에그 견과류죽/쌀밥 (쌀:국내산) 피바지락국  치커리사과무침  포기김치 (김치:국내산)~
## 2 모닝롤/프렌치토스트  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 콩나물김치국  쥐포무침  포기김치 (김치:국내산)~
## 3 모닝롤/마늘빵  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 된장찌개  단배추나물  포기김치 (김치:국내산)~
##   Lunch                                                                         
##   <chr>                                                                         
## 1 쌀밥/잡곡밥 (쌀:국내산) 아욱된장국  돼지갈비찜  콩나물겨자채  참나물무침  포기김치 (김치:국내산)~
## 2 쌀밥/잡곡밥 (쌀:국내산) 열무된장국  훈제오리구이  매운어묵볶음  쌈무/부추생채 크리스마스케익 포기김치 (김치:국내산)~
## 3 쌀밥/잡곡밥 (쌀:국내산) 콩나물국  돈육고추장불고기 (돼지고기:국내산) 감자채볶음  양배추쌈  포기김치 (김치:국내산)~
##   Dinner                                                                        
##   <chr>                                                                         
## 1 쌀밥/잡곡밥 (쌀:국내산) 옛날왕돈까스  크림스프  감자범벅/오이피클  과일샐러드  알타리김치 (김치:국내산)~
## 2 쌀밥/잡곡밥 (쌀:국내산) 민물새우찌개  닭갈비  버섯잡채  오이무침  포기김치 (김치:국내산)~
## 3 쌀밥/잡곡밥 (쌀:국내산) 들깨시래기국  훈제오리볶음  계란찜  참나물생채  포기김치 (김치:국내산)~
##   N_lunch N_dinner Before_holiday After_holiday Sandwich_day Net_total
##     <dbl>    <dbl>          <dbl>         <dbl>        <dbl>     <dbl>
## 1     311      245              0             0            0      2165
## 2     296      104              1             0            1      1793
## 3     332      135              1             0            1      1629
```

```r
train_new %>% 
  filter(N_dinner <= quantile(N_dinner, .25) - IQR(N_dinner)*1.5)
```

```
## # A tibble: 57 x 16
##    Date       Dayofweek Total Vacation Business Overtime  Home
##    <date>     <chr>     <dbl>    <dbl>    <dbl>    <dbl> <dbl>
##  1 2016-11-30 수         2689       68      207        0     0
##  2 2016-12-28 수         2705      166      225        0     0
##  3 2017-01-25 수         2697       79      203        0     0
##  4 2017-01-26 목         2697      369      117       28     0
##  5 2017-02-22 수         2632       75      252        0     0
##  6 2017-03-22 수         2627       53      235        0     0
##  7 2017-04-26 수         2626       45      304        0     0
##  8 2017-05-31 수         2637       43      265        0     0
##  9 2017-06-28 수         2648       58      259        0     0
## 10 2017-07-26 수         2839      254      246        0     0
##    Breakfast                                                                    
##    <chr>                                                                        
##  1 모닝롤/카스텔라  우유/주스 스크램블에그 누룽지탕/쌀밥 (쌀:국내산) 고추장찌개  김자반  포기김치 (김치:국내산)~
##  2 모닝롤/고구마파이  우유/주스 스크램블에그 누룽지탕/쌀밥 (쌀:국내산) 북어계란국  느타리호박볶음  포기김치 (김치:국내산)~
##  3 모닝롤/토마토샌드  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 두부젓국찌개  김자반  포기김치 (김치:국내산)~
##  4 모닝롤/와플  우유/주스 스크램블에그 야채죽/쌀밥 (쌀:국내산) 오징어국  크래미숙주무침  포기김치 (김치:국내산)~
##  5 모닝롤/브라우니  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 꽃게탕  명엽채무침  포기김치 (김치:국내산)~
##  6 모닝롤/찐빵  우유/주스 계란후라이 김치죽/쌀밥 (쌀:국내산) 콩나물국  김실파무침  포기김치 (김치:국내산)~
##  7 모닝롤/와플  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 된장찌개  검정콩조림  포기김치 (김치:국내산)~
##  8 모닝롤/베이글  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 두부젓국찌개  자반김  포기김치 (김치:국내산)~
##  9 모닝롤/단팥빵  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 쇠고기매운국  고구마순나물  포기김치 (김치:국내산)~
## 10 모닝롤/단팥빵  우유/주스 계란후라이 누룽지탕/쌀밥 (쌀:국내산) 민물새우찌개  숙주나물  포기김치 (김치:국내산)~
##    Lunch                                                                        
##    <chr>                                                                        
##  1 나물비빔밥 (쌀:국내산) 가쯔오장국  치킨핑거*요거트D  감자샐러드  오복지무침  포기김치 (김치:국내산)~
##  2 콩나물밥 (쌀:국내산) 가쯔오장국  미트볼케찹조림  꽃맛살샐러드  군고구마  배추겉절이 (김치:국내산)~
##  3 카레덮밥 (쌀:국내산) 맑은국  유린기  개성감자만두  오이사과무침  포기김치 (김치:국내산)~
##  4 쌀밥/잡곡밥 (쌀:국내산) 배추된장국  소갈비찜  삼색꼬지전  콩나물무침  포기김치 (김치:국내산)~
##  5 나물비빔밥 (쌀:국내산) 유부장국  생선까스*탈탈소스  파스타샐러드  마늘쫑볶음  알타리김치 (김치:국내산)~
##  6 쌀밥/잡곡밥 (쌀:국내산) 돈육김치찌개  유린기  비엔나볶음  세발나물  깍두기 (김치:국내산)~
##  7 비빔밥 (쌀:국내산) 맑은국  오징어튀김  견과류조림  하와이안샐러드  깍두기 (김치:국내산)~
##  8 열무보리비빔밥 (쌀:국내산) 가쯔오장국  탕수만두  콥샐러드  오이지무침  포기김치 (김치:국내산)~
##  9 콩나물밥 (쌀:국내산) 얼갈이된장국  삼치구이  잡채  아삭고추무침  깍두기 (김치:국내산)~
## 10 나물비빔밥  미소장국  파스타샐러드  소세지오븐구이  오렌지  포기김치 (김치:국내산)~
##    Dinner                                                                       
##    <chr>                                                                        
##  1 *                                                                            
##  2 *                                                                            
##  3 *                                                                            
##  4 쌀밥/잡곡밥 (쌀:국내산) 시래기들깨탕  돈육강정  메추리알곤약조림  물파래무침  포기김치 (김치:국내산)~
##  5 *                                                                            
##  6 *                                                                            
##  7 *                                                                            
##  8 자기계발의날                                                                 
##  9 *자기계발의날*                                                               
## 10 가정의날                                                                     
##    N_lunch N_dinner Before_holiday After_holiday Sandwich_day Net_total
##      <dbl>    <dbl>          <dbl>         <dbl>        <dbl>     <dbl>
##  1    1109        0              0             0            0      2414
##  2     767        0              0             0            0      2314
##  3     720        0              0             0            0      2415
##  4     844      147              1             0            0      2211
##  5    1065        0              0             0            0      2305
##  6     953        0              0             0            0      2339
##  7     835        0              0             0            0      2277
##  8     910        0              0             0            0      2329
##  9     745        0              0             0            0      2331
## 10     797        0              0             0            0      2339
## # ... with 47 more rows
```

```r
# Correlation check
cor_data <- train_new %>% 
  select_if(is.numeric) %>% 
  # na.omit() %>% 
  cor() %>% 
  round(3)

ggcorrplot(corr = cor_data,
           type = "lower",
           lab = TRUE, 
           title = "Correlation plot")
```

![](EDA_CODE_files/figure-html/unnamed-chunk-4-4.png)<!-- -->

```r
# group Test
library(moonBook)
mytable(Before_holiday ~ N_lunch, train_new) # 공휴일 전날 점심 식사량이 적음
```

```
## 
## Descriptive Statistics by 'Before_holiday'
## __________________________________________ 
##                0             1         p  
##            (N=1165)       (N=40)    
## ------------------------------------------ 
##  N_lunch 896.5 ± 207.2 711.8 ± 198.8 0.000
## ------------------------------------------
```

```r
mytable(Before_holiday ~ N_dinner, train_new) # 공휴일 전날 저녁 식사량이 적음
```

```
## 
## Descriptive Statistics by 'Before_holiday'
## ___________________________________________ 
##                 0             1         p  
##             (N=1165)       (N=40)    
## ------------------------------------------- 
##  N_dinner 466.8 ± 134.9 316.2 ± 179.7 0.000
## -------------------------------------------
```

```r
mytable(After_holiday ~ N_lunch, train_new) # 공휴일 다음날 점심 식사량이 많음
```

```
## 
##  Descriptive Statistics by 'After_holiday'
## ___________________________________________ 
##                0             1          p  
##            (N=1160)        (N=45)    
## ------------------------------------------- 
##  N_lunch 885.6 ± 205.9 1011.4 ± 262.2 0.003
## -------------------------------------------
```

```r
mytable(After_holiday ~ N_dinner, train_new) # 공휴일 다음날 저녁 식사량과는 유의한 차이 없음
```

```
## 
##  Descriptive Statistics by 'After_holiday'
## ___________________________________________ 
##                 0             1         p  
##             (N=1160)       (N=45)    
## ------------------------------------------- 
##  N_dinner 460.3 ± 137.6 499.1 ± 172.4 0.142
## -------------------------------------------
```

```r
mytable(Sandwich_day ~ N_lunch, train_new) # 샌드위치데이날 점심 식사량이 적음
```

```
## 
##  Descriptive Statistics by 'Sandwich_day'
## __________________________________________ 
##                0             1         p  
##            (N=1189)       (N=16)    
## ------------------------------------------ 
##  N_lunch 894.1 ± 206.6 612.4 ± 245.9 0.000
## ------------------------------------------
```

```r
mytable(Sandwich_day ~ N_dinner, train_new) # 샌드위치데이날 저녁 식사량이 적음
```

```
## 
##  Descriptive Statistics by 'Sandwich_day' 
## ___________________________________________ 
##                 0             1         p  
##             (N=1189)       (N=16)    
## ------------------------------------------- 
##  N_dinner 463.9 ± 137.8 300.6 ± 150.2 0.000
## -------------------------------------------
```

```r
# Trend check
# train %>% 
#   select(Date, Sum_lunch, Sum_dinner) %>% 
#   gather(key = Type, value = Sum, 2:3) %>% 
#   ggplot() +
#   aes(x = Date, y = Sum, colour = Type) +
#   geom_line(size = 1L) +
#   scale_color_brewer(palette = "Dark2") +
#   theme_minimal()
```

<hr>

<div style="margin-bottom:60px;">
</div>

