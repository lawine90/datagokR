datagokR
========
[**공공데이터포털**](https://www.data.go.kr/)에서 REST API를 이용하여 데이터를 받을 수 있는 패키지입니다.
공공데이터포털에서 API 키를 발급받고 API 사용 승인을 받아야 사용하실 수 있습니다.


# 1. 패키지 설치 방법
```
devtools::install_github('lawine90/datagokR')
```


# 2. 현재 다운로드 가능한 데이터 목록
  - 국토교통부: [**주거시설 실거래가**](https://www.data.go.kr/dataset/3050988/openapi.do)
  - 기상청: [**생활기상지수**](https://www.data.go.kr/dataset/15000232/openapi.do)
  - 기상청: [**보건기상지수**](https://www.data.go.kr/dataset/15000154/openapi.do)
  - 기상청: [**종관기상관측**](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do)
  - 국민건강보험공단: [**예측진료정보**](https://www.data.go.kr/dataset/15028050/openapi.do)
  - 식품의약품안전처: [**의약품 부작용**](https://www.data.go.kr/dataset/15020627/openapi.do)


# 3. 각 함수 설명 및 사용법
> **1) [**주거시설 실거래가**](https://www.data.go.kr/dataset/3050988/openapi.do)(molitRealTrade)**
> 
> 부동산 거래신고에 관한 법률에 따라 신고된 주택의 실거래 자료을 제공합니다. 주거시설의 타입은 아파트, 연립다세대, 단독/다가구 3가지이며 거래 타입은 매매, 전월세 2가지 입니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3050988/openapi.do)의 참고문서를 확인하시거나 패키지 관리자에게 연락주시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $meta에는 메타데이터, $data에 실제 거래 데이터가 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - year: (필수, 정수). 주거시설 거래가 발생한 년도
> - month: (옵션, 정수). 주거시설 거래가 발생한 달. 입력하지 않을 시 해당 년도의 전체 데이터를 가져옴
> - localeCode: (옵션, 정수). 주거시설 거래가 발생한 지역의 시/군/구 코드. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 주거시설 거래가 발생한 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - houseType: (필수, 문자). 주거시설의 타입. 아파트는 "apart", 연립다세대는 "multi", 단독/다가구는 "detached"
> - tradeType: (필수, 문자). 주거시설 거래의 타입. 매매는 "trade", 전월세는 "rent"
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)

``` 
# example
> key <- 'your key'
> data <- molitRealTrade(key = key, year = 2018, localeCode = 11110,
                         houseType = "apart", tradeType = "trade")
> head(data$meta)
# A tibble: 12 x 2
   url                                                                                                                     count
   <chr>                                                                                                                   <chr>
 1 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 88   
 2 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 83   
 3 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 78   
 4 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 43   
 5 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 58   
 6 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 55   

> data$data
# A tibble: 721 x 12
    Code Dong   Trade_year Trade_month Trade_day consYear  Price addCode Name                       excArea Floor name          
   <int> <chr>       <int>       <int>     <int>    <int>  <dbl> <chr>   <chr>                      <chr>   <chr> <chr>         
 1 11110 누상동       2018           1        19     1999  17900 40      청호그린빌                 29.76   4     서울특별시 종로구~
 2 11110 누상동       2018           1        19     1999  17900 40      청호그린빌                 29.76   3     서울특별시 종로구~
 3 11110 사직동       2018           1         9     2008 110000 9       광화문풍림스페이스본(101동~105동)~ 146.92  9     서울특별시 종로구~
 4 11110 사직동       2018           1        22     2008  89000 9       광화문풍림스페이스본(101동~105동)~ 94.51   6     서울특별시 종로구~
 5 11110 사직동       2018           1        23     2008  85000 9       광화문풍림스페이스본(101동~105동)~ 95.88   2     서울특별시 종로구~
 6 11110 사직동       2018           1        24     2008 100000 9       광화문풍림스페이스본(101동~105동)~ 108.55  1     서울특별시 종로구~
 7 11110 사직동       2018           1        25     2008  85000 9       광화문풍림스페이스본(101동~105동)~ 94.51   3     서울특별시 종로구~
 8 11110 사직동       2018           1        27     2008  91000 9       광화문풍림스페이스본(101동~105동)~ 94.51   13    서울특별시 종로구~
 9 11110 사직동       2018           1        30     2008 132000 9       광화문풍림스페이스본(101동~105동)~ 147.31  6     서울특별시 종로구~
10 11110 사직동       2018           1        30     2008  85000 9       광화문풍림스페이스본(101동~105동)~ 94.51   5     서울특별시 종로구~
# ... with 711 more rows
```


> **2) [**생활기상지수**](https://www.data.go.kr/dataset/15000232/openapi.do)(kmaHealthIndex)**
> 
> 각종 기상자료를 응용하여 일반 국민의 일상생활 및 보건에 활용할 수 있도록 생산된 생활기상지수(부패지수, 동파지수, 체감온도 등)를 특정 위치(지점)별로 산출한 데이터입니다. 각 지수마다 다르나 3시간 단위로 측정되며 각 지수별로 수집할 수 있는 기간(월 기준)이 다르므로 아래 설명에서 수집 가능한 기간을 확인 후 사용하시기 바랍니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15000232/openapi.do)의 참고문서를 확인하시거나 패키지 관리자에게 연락주시기 바랍니다.함수 실행 결과는 R의 list 타입이며 $data에 실제 생활기상지수 데이터, $error에 에러가 발생한 데이터의 url, $url에 전체 데이터의 url이 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - localeCode: (옵션, 정수). 지역의 시/군/구 코드. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False
> - type: (필수, 문자). 8가지 생활기상지수의 타입. 각 타입에 대한 설명은 다음과 같습니다.
>     * fp: 1 ~ 12월. 식중독지수. 
>     * st: 11 ~ 3월. 체감온도. 
>     * hi: 6 ~ 9월. 열지수. 
>     * di: 6 ~ 9월. 불쾌지수. 
>     * ui: 3 ~ 11월. 자외선지수. 
>     * fb: 12 ~ 2월. 동파가능지수. 
>     * ap: 11 ~ 5월. 대기확산지수. 
>     * sh: 5 ~ 9월. 더위체감지수. 
>     * possible: 현재 수집 가능한 모든 생활기상지수.

```
# example
> key <- 'your key'
> data <- kmaLifeIndex(key, localeName = c('수원', '용인'), type = "fp", slow = T)
> data$data$fp
# A tibble: 158 x 8
   idxCode type  locale     time                   d0    d1    d2 level
   <chr>   <chr> <chr>      <chr>               <dbl> <dbl> <dbl> <ord>
 1 A01_2   Fsn   4111100000 2019-08-23 06:00:00    25    31    34 safe 
 2 A01_2   Fsn   4111100000 2019-08-23 18:00:00    NA    31    39 NA   
 3 A01_2   Fsn   4111156000 2019-08-23 06:00:00    25    31    34 safe 
 4 A01_2   Fsn   4111156000 2019-08-23 18:00:00    NA    31    39 NA   
 5 A01_2   Fsn   4111156600 2019-08-23 06:00:00    25    31    34 safe 
 6 A01_2   Fsn   4111156600 2019-08-23 18:00:00    NA    31    39 NA   
 7 A01_2   Fsn   4111157100 2019-08-23 06:00:00    25    31    34 safe 
 8 A01_2   Fsn   4111157100 2019-08-23 18:00:00    NA    31    39 NA   
 9 A01_2   Fsn   4111157200 2019-08-23 06:00:00    25    31    34 safe 
10 A01_2   Fsn   4111157200 2019-08-23 18:00:00    NA    31    39 NA   
# ... with 148 more rows
```


> **3) [**보건기상지수**](https://www.data.go.kr/dataset/15000154/openapi.do)(kmaHealthIndex)**
> 
> 기후변화에 능동적으로 신체를 대처할 수 있는 보건기상지수(뇌졸중가능지수, 피부질환지수, 감기가능지수, 꽃가루농도위험지수 등 5종)를 지정위치(지점)별로 산출한 데이터입니다. 각 지수마다 다르나 3시간 단위로 측정되며 각 지수별로 수집할 수 있는 기간(월 기준)이 다르므로 아래 설명에서 수집 가능한 기간을 확인 후 사용하시기 바랍니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15000154/openapi.do)의 참고문서를 확인하시거나 패키지 관리자에게 연락주시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $data에 실제 보건기상지수 데이터, $error에 에러가 발생한 데이터의 url, $url에 전체 데이터의 url이 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - localeCode: (옵션, 정수). 지역의 시/군/구 코드. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False
> - type: (필수, 문자). 7가지 보건기상지수의 타입. 각 타입에 대한 설명은 다음과 같습니다.
>     * Asthma: 1 ~ 12월. 천식, 폐질환가능지수.
>     * Brain: 1 ~ 12월. 뇌졸중가능지수.
>     * Skin: 1 ~ 12월. 피부질환가능지수.
>     * Infl: 9 ~ 4월. 감기가능지수.
>     * FlowerWoody: 4 ~ 5월. 참나무 꽃가루농도위험지수.
>     * FlowerPine: 4 ~ 5월. 소나무 꽃가루농도위험지수.
>     * FlowerWeeds: 9 ~ 10월. 잡초 꽃가루농도위험지수.
>     * possible: 현재 수집 가능한 모든 생활기상지수.

```
# example
> key <- 'your key'
> data <- kmaHealthIndex(key, localeName = c('수원', '용인'), type = "possible", slow = T)
> names(data$data)
[1] "Asthma"    "Brain"    "Skin"    "FlowerWeeds"    "Infl" 
>
> data$data$Brain
# A tibble: 158 x 8
   idxCode type  locale     time                   d0    d1    d2 level 
   <chr>   <chr> <chr>      <chr>               <dbl> <dbl> <dbl> <ord> 
 1 D02     Brain 4111100000 2019-08-23 06:00:00     1     1     1 normal
 2 D02     Brain 4111100000 2019-08-23 18:00:00    NA     1     1 NA    
 3 D02     Brain 4111156000 2019-08-23 06:00:00     1     1     1 normal
 4 D02     Brain 4111156000 2019-08-23 18:00:00    NA     1     1 NA    
 5 D02     Brain 4111156600 2019-08-23 06:00:00     1     1     1 normal
 6 D02     Brain 4111156600 2019-08-23 18:00:00    NA     1     1 NA    
 7 D02     Brain 4111157100 2019-08-23 06:00:00     1     1     1 normal
 8 D02     Brain 4111157100 2019-08-23 18:00:00    NA     1     1 NA    
 9 D02     Brain 4111157200 2019-08-23 06:00:00     1     1     1 normal
10 D02     Brain 4111157200 2019-08-23 18:00:00    NA     1     1 NA    
# ... with 148 more rows
```


> **4) [**종관기상관측**](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do)(kmaASOS)**
> 
> 정해진 시각에 각 지역의 관측소에서 실측된 지상관측 데이터입니다. 기온, 강수, 바람, 기압, 습도 등 날씨현상과 관련된 데이터이며  전국 94개 지점에서 수집된 데이터의 하루 평균을 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **기상자료개방포털에서 발급받은 별도의 API키가 필요**합니다. 기상자료개방포털에서 제공하는 API는 매우 불안정하므로 slow argument를 TRUE로 설정하고 사용하길 권정합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $meta에는 메타데이터, $data에 관측 데이터가 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 기상자료개방 포털에서 발급받은 API 키
> - branchCode: (필수, 정수|문자). 관측소의 지점 코드. 'all'을 입력시 모든 지점의 관측 데이터를 수집
> - fromDate: (필수, 날짜). 데이터를 수집하려는 기간의 시작일
> - toDate: (필수, 날짜). 데이터를 수집하려는 기간의 종료일
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key from data.kma.go.kr'
> data <- kmaASOS(key2, branchCode = 'all', fromDate = as.Date('2019-08-19'), 
                  toDate = Sys.Date(), slow = T)
> data$data
# A tibble: 658 x 46
   date   brch brch_nme araPrs_avg arTmp_max arTmp_min cldAmt_avg dewTmp_avg ertTmp_0.5m_avg ertTmp_1.5m_avg ertTmp_10cm_avg
   <chr> <dbl> <chr>         <dbl>     <dbl>     <dbl>      <dbl>      <dbl>           <dbl>           <dbl>           <dbl>
 1 2019~    90 속초          1009.      27        20.6        4.3       20.2            NA              NA              NA  
 2 2019~    90 속초          1010.      28.4      21          4.9       21.2            NA              NA              NA  
 3 2019~    90 속초          1006.      27.5      20.7        8.4       21.7            NA              NA              NA  
 4 2019~    90 속초          1003.      29.3      22.4        7.6       19.9            NA              NA              NA  
 5 2019~    90 속초          1004.      30.4      22.4        2.9       16.3            NA              NA              NA  
 6 2019~    90 속초          1007.      28.2      21.3        5.6       18.2            NA              NA              NA  
 7 2019~    90 속초          1010.      27.6      20.3        5         20.4            NA              NA              NA  
 8 2019~    93 북춘천         999.      31.1      18.1        4.4       18.9            26.3            24.6            27.1
 9 2019~    93 북춘천        1000.      32.5      19.4        2.8       19.5            26.7            24.6            28.3
10 2019~    93 북춘천         998.      29.5      20.7        8.5       21.4            27.2            24.5            27.9
# ... with 648 more rows, and 35 more variables: ertTmp_1m_avg <dbl>, ertTmp_20cm_avg <dbl>, ertTmp_30cm_avg <dbl>,
#   ertTmp_3m_avg <dbl>, ertTmp_5cm_avg <dbl>, ertTmp_5m_avg <dbl>, extmSnw_day <dbl>, fogDur_sum <dbl>, frsSnw_day <dbl>,
#   frsSnw_sum <dbl>, grsSun_sum <dbl>, grsTmp_min <dbl>, hum_avg <dbl>, hum_min <dbl>, insWdSp_max <dbl>,
#   insWdSpDrc_max <dbl>, lrgEpr_sum <dbl>, rain_10mts_max <dbl>, rain_1hr_max <dbl>, rain_9_9 <dbl>, rain_sum <dbl>,
#   seaPrs_avg <dbl>, seaPrs_max <dbl>, seaPrs_min <dbl>, slr_1hr_max <dbl>, smlEpr_sum <dbl>, sunDur_sum <dbl>,
#   sunDurTm_sum <dbl>, surfTmp_avg <dbl>, ttlCld_avg <dbl>, vapPrs_avg <dbl>, wdRun_sum <dbl>, wdSp_avg <dbl>, wdSp_max <dbl>,
#   wdSpDrc_max <dbl>
```


