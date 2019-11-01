datagokR
========
[**공공데이터포털**](https://www.data.go.kr/)에서 REST API를 이용하여 데이터를 받을 수 있는 패키지입니다. 공공데이터포털에서 API 키를 발급받고 API 사용 승인을 받아야 사용하실 수 있습니다. API 사용 승인의 경우 자동승인과 심의승인이 있으며 datagokR에서는 자동승인만을 수집 대상으로 합니다.


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
  - 식품의약품안전처: [**식품 영양성분**](https://www.data.go.kr/dataset/15020625/openapi.do)
  - 식품의약품안전처: [**의약외품 제품**](https://www.data.go.kr/dataset/15028967/openapi.do)
  - 농림축산식품부: [**가축질병발생정보**](https://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20151204000000000563&service_ty=O)
  - 국회사무처: [**국회의원 정보**](https://www.data.go.kr/dataset/15012647/openapi.do)
  - 국회사무처: [**국회의원 상세정보**](https://www.data.go.kr/dataset/15012647/openapi.do)


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
> 정해진 시각에 각 지역의 관측소에서 실측된 지상관측 데이터입니다. 기온, 강수, 바람, 기압, 습도 등 날씨현상과 관련된 데이터이며  전국 94개 지점에서 수집된 데이터의 하루 평균을 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **기상자료개방포털에서 발급받은 별도의 API키가 필요**합니다. 기상자료개방포털에서 제공하는 API는 매우 불안정하므로 slow argument를 TRUE로 설정하고 사용하길 권장합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $meta에는 메타데이터, $data에 관측 데이터가 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
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
> data <- kmaASOS(key, branchCode = 'all', fromDate = as.Date('2019-08-19'), 
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


> **5) [**예측진료정보**](https://www.data.go.kr/dataset/15028050/openapi.do)(nhisDiseaseForcast)**
> 
> 수요조사 결과에 따라 관심도가 높은 5대 질병(눈병, 천식, 감기, 피부염, 식중독)에 대한 위험도 및 예측 진료 건수 정보를 제공합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://www.data.go.kr/dataset/15028050/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $meta에는 메타데이터, $data에 관측 데이터, $recommanded에 각 질병의 위험도에 따른 추천행동이 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - localeCode: (옵션, 정수). 지역의 시/군/구 코드 앞 2자리. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False
> - type: (필수, 문자). 7가지 보건기상지수의 타입. 각 타입에 대한 설명은 다음과 같습니다.
>     * Asthma: 천식.
>     * Eye: 눈병.
>     * Food: 식중독.
>     * Influenza: 감기.
>     * Skin: 피부염.
>     * All: 모든 질병.

```
# example
> key <- 'your key'
> data <- nhisDiseaseForcast(key, localeName = c("수원"), type = "All", slow = T)
> data$data
# A tibble: 462 x 5
   diss  date     locale cnt   risk 
   <chr> <chr>    <chr>  <chr> <chr>
 1 1     20191001 41111  2117  2    
 2 1     20191001 41113  2412  2    
 3 1     20191001 41115  1435  2    
 4 1     20191001 41117  2328  2    
 5 1     20191001 41131  1605  2    
 6 1     20191001 41133  1809  2    
 7 1     20191001 41135  3555  2    
 8 1     20191001 41150  3093  2    
 9 1     20191001 41171  1736  2    
10 1     20191001 41173  2481  2    
# ... with 452 more rows
>
> data$recommanded
# A tibble: 9 x 3
  diss  risk  rcmd                                                                                                              
  <chr> <chr> <chr>                                                                                                             
1 1     1     환기를 자주 시켜 깨끗한 환경을 유지하고, 외출 후에는 반드시 손을 씻는 등 평소 손 씻기를 생활화합니다.             
2 1     2     기침과 재채기를 할 때에는 반드시 휴지나 손수건으로 가리는 등 기침 에티켓을 지켜주시고 충분한~
3 2     1     비누를 사용하여 흐르는 수돗물에 손을 자주 씻고, 손으로 얼굴, 특히 눈 주위를 만지지 않도록 합니다.                 
4 2     2     비누를 사용하여 흐르는 수돗물에 손을 자주 씻고, 수건이나 개인 소지품 등은 다른 사람과 함께 사용하지 않습니다.     
5 3     1     식중독 발생가능성은 낮으나 식중독예방에 지속적인 관심이 요망됩니다. 화장실 사용 후, 귀가 후, 조리 전에 손 씻기를 생활화 합시다.~
6 4     3     급격한 온도 변화를 피하고, 외출 후 손씻기, 양치질 등 개인위생관리를 철저히 하고, 인플루엔자 독감 예방접종을 받습니다.~
7 4     4     급격한 온도 변화를 피하고 심한 기침이 발생시 바로 의료기관을 방문하여 전문의의 지시를 따르고, 증상이 조절되더라도 재발, ~
8 5     2     보습제를 사용하고 털, 먼지, 화학물질 피하며 신선한 제철 야채와 과일을 통해 비타민C, 비타민 B1을 충분히 섭취합니다.
9 5     3     피부가 건조하지 않도록 보습제를 충분히 사용하며 실내 온도 및 습도(50~60%)를 유지하고 땀을 흘리는 운동은 피합니다. 
```


> **6) [**의약품 부작용**](https://www.data.go.kr/dataset/15020627/openapi.do)(drugsSideEffect)**
> 
> 수요조사 결과에 따라 관심도가 높은 5대 질병(눈병, 천식, 감기, 피부염, 식중독)에 대한 위험도 및 예측 진료 건수 정보를 제공합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://www.data.go.kr/dataset/15028050/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키

```
# example
> key <- 'your key'
> data <- drugsSideEffect(key)
> data
# A tibble: 54 x 7
   name_kor     name_eng    type  period               effect_kor            effect_eng                            etc          
   <chr>        <chr>       <chr> <chr>                <chr>                 <chr>                                 <chr>        
 1 이소니아지드 Isoniazide  ""    1989년 1월 ~ 2013년 6월~ 가려움증              Pruritus                              지속적 모니터링~
 2 이소니아지드 Isoniazide  ""    1989년 1월 ~ 2013년 6월~ 반점구진발진          Rash Maculo-Papular                   지속적 모니터링~
 3 프로포폴     Propofol    ""    1989년 1월 ~ 2013년 6월~ 사망                  Death                                 허가사항 변경
 4 프로포폴     Propofol    ""    1989년 1월 ~ 2013년 6월~ 약물남용              Drug Abuse                            허가사항 변경
 5 프로포폴     Propofol    ""    1989년 1월 ~ 2013년 6월~ 청색증                Cyanosis                              허가사항 변경
 6 프로포폴     Propofol    ""    1989년 1월 ~ 2013년 6월~ 상세불명의 지질대사이상~ Lipid Metabolism Disorder NOS         지속적 모니터링~
 7 졸피뎀       Zolpidem    ""    1989년 1월 ~ 2013년 6월~ 진전                  Tremor                                허가사항 변경
 8 졸피뎀       Zolpidem    ""    1989년 1월 ~ 2013년 6월~ 실신                  Syncope                               지속적 모니터링~
 9 리스페리돈   Risperidone ""    1989년 1월 ~ 2013년 6월~ 강박장애 (경구, 주사) Obsessive-compulsive Disorder (Oral,~ 지속적 모니터링~
10 리스페리돈   Risperidone ""    1989년 1월 ~ 2013년 6월~ 기억상실증 (경구)     Amnesia (Oral)                        지속적 모니터링~
# ... with 44 more rows
```


> **7) [**식품 영양성분**](https://www.data.go.kr/dataset/15020625/openapi.do)(nutriComponent)**
> 
> 식품의약품안전처에서 관리하는 식품 영양성분 데이터를 제공합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://www.data.go.kr/dataset/15020625/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key'
> data <- nutriComponent(key, verbose = T)
  |==============================================================================================================| 100%
> 
> data
# A tibble: 22,629 x 13
   name_kor serving_wt  kcal carbohydrate protein   fat sugar sodium cholesterol saturated_fatty~ trans_fatty_acid  year
   <chr>         <dbl> <dbl>        <dbl>   <dbl> <dbl> <dbl>  <dbl>       <dbl>            <dbl>            <dbl> <dbl>
 1 고량미,알곡~        100   349         67.8    10.1   3.7    NA     NA          NA            NA                  NA  2001
 2 겉귀리,생것~        100   373         73.5    11.4   3.7    NA      2           0            NA                  NA  2011
 3 겉귀리,생것~        100   332         73.5    11.4   3.7    NA      2          NA            NA                  NA  2017
 4 쌀귀리,생것~        100   371         70.4    14.3   3.8    NA      3          NA            NA                  NA  2011
 5 쌀귀리,생것~        100   334         70.4    14.3   3.8    NA      3          NA            NA                  NA  2017
 6 오트밀          100   382         64.9    13.2   8.2    NA      4           0            NA                  NA  2011
 7 오트밀          100   348         64.9    13.2   8.2    NA      4          NA            NA                  NA  2017
 8 기장,생것~        100   367         74.6    11.2   1.9    NA      6           0            NA                  NA  2011
 9 기장,생것~        100   360         74.6    11.2   1.9    NA      6          NA            NA                  NA  2017
10 메밀,생것~        100   374         74.7    11.5   2.3    NA     14           0             0.59               NA  2011
# ... with 22,619 more rows, and 1 more variable: factory <chr>
```


> **8) [**의약외품 제품**](https://www.data.go.kr/dataset/15028967/openapi.do)(quasiDrugs)**
> 
> 식품의약품안전처에서 관리하는 의약외품(quasi-drugs)의 품목명, 용법용량, 효능효과, 사용상의 주의사항 등을 제공합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://www.data.go.kr/dataset/15028967/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key'
> data <- nutriComponent(key, verbose = T)
  |==============================================================================================================| 100%
> 
> data
# A tibble: 10,334 x 8
   item_code  item_name    effect          usage              notice          type_code  type_name        firm  
   <chr>      <chr>        <chr>           <chr>              <chr>           <chr>      <chr>            <chr> 
 1 200308392  레이욘부직~  창상 및 수술~   창상 및 수술전후~  보관 및 취급시~ 35000      [35000]기타~     신용코리아~
 2 201602150  니찌반바틀~  붕대, 거즈, 카~ 필요한 길이만큼~   피부발적, 가려~ 33800      [33800]반창고~   (주)나음~
 3 200608013  헤파-알2     진료 또는 치~   제품의 코 편 폴~   없음.           32100      [32100]수술용~   (주)신진~
 4 200608007  헤파-알2~    진료 또는 치~   제품의 코 편 폴~   없음.           32100      [32100]수술용~   (주)신진~
 5 200608014  헤파-알2~    진료 또는 치~   제품의 코 편 폴~   없음.           32100      [32100]수술용~   (주)신진~
 6 200612211  1.쑥건향초~  흡연욕구를 참~  1)담배대용으로~                    45100      [45100]흡연욕구~ (주)한국~
 7 200701636  아이면생리~  생리혈의 위~    1일 수 회 사용~    사용후 변기에~  31100      [31100]생리대~   (주)아이~
 8 200701635  아이면생리~  생리혈의 위~    1일 수 회 사용~    사용후 변기에~  31100      [31100]생리대~   (주)아이~
 9 200701637  아이면생리~  생리혈의 위~    1일 수 회 사용~    사용후 변기에~  31100      [31100]생리대~   (주)아이~
10 200701638  아이면생리~  생리혈의 위~    1일 수 회 사용~    사용후 변기에~  31100      [31100]생리대~   (주)아이~
# ... with 10,324 more rows
```


> **9) [**가축질병발생정보**](https://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20151204000000000563&service_ty=O)(livestockDisease)**
>
> 농림축산식품부 농림축산검역본부 역학조사과에서 관리하며 가축 전염병, 발생 농장, 사육하는 가축, 진단자 등의 정보를 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **농림축산식품 공공데이터 포털에서 발급받은 별도의 API키가 필요**합니다. 자세한 사항은 [링크](https://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20151204000000000563&service_ty=O)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 농림축산식품 공공데이터 포털에서 발급받은 API 키
> - fromDate: (필수, 날짜). 데이터를 수집하려는 기간의 시작일
> - toDate: (필수, 날짜). 데이터를 수집하려는 기간의 종료일
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key from data.mafra.go.kr'
> data <- livestockDisease(key, fromDate = as.Date('2019-09-01'), toDate = as.Date('2019-10-29'), verbose = T)
  |==========================================================================================================| 100%
> 
> data
# A tibble: 108 x 12
   occr_no  dizz_name  farm_name farm_code farm_addr  occr_date  occr_n lvst_code lvst_type diag_code diag_name clos_date 
   <chr>    <chr>      <chr>     <chr>     <chr>      <date>      <dbl> <chr>     <chr>     <chr>     <chr>     <date>    
 1 00053833 낭충봉아부패병~ 김정근 윤미자~ 41830395~ 경기도 양평군 지~ 2019-09-02     15 418600    벌-재래종 6412106   경기 동부지소~ NA        
 2 00053834 낭충봉아부패병~ 박동선    41830250~ 경기도 양평군 양~ 2019-09-02      2 418600    벌-재래종 6412106   경기 동부지소~ NA        
 3 00203822 결핵병     한길농장  46830370~ 전라남도 영암군 ~ 2019-09-02      1 412002    소-한우   6430863   충북 중부지소~ NA        
 4 00202950 브루셀라병 세윤농장  42110340~ 강원도 춘천시 남~ 2019-09-03      1 412002    소-한우   6420912   강원 동물위생시~ NA        
 5 00053709 낭충봉아부패병~ 최헌숙    42760330~ 강원도 평창군 대~ 2019-09-03     10 418600    벌-재래종 6420925   강원 중부지소~ NA        
 6 00053710 낭충봉아부패병~ 김영목    42760330~ 강원도 평창군 대~ 2019-09-03      3 418600    벌-재래종 6420925   강원 중부지소~ NA        
 7 00053711 낭충봉아부패병~ 조창길    42760340~ 강원도 평창군 봉~ 2019-09-03      5 418600    벌-재래종 6420925   강원 중부지소~ NA        
 8 00204226 결핵병     양학농장A 46840340~ 전라남도 무안군 ~ 2019-09-04      1 412002    소-한우   6460965   전남 서부지소~ NA        
 9 00202760 결핵병     삼호한우  41670250~ 경기도 여주시 가~ 2019-09-04      1 412002    소-한우   6412106   경기 동부지소~ NA        
10 00204689 결핵병     NA        41590410~ 경기도 화성시 정~ 2019-09-05      2 412004    소-젖소   6412102   경기도 동물위생~ NA        
# ... with 98 more rows
```


> **10) 국회사무처: [**국회의원 정보**](https://www.data.go.kr/dataset/15012647/openapi.do)(nasCongressman1)**
>
> 국회사무처가 제공하는 간략한 국회의원 정보를 제공합니다. 특정 국회의원의 상세한 정보는 nasCongressman2 함수를 사용하시기 바랍니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15012647/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키


```
# example
> key <- 'your key'
> data <- nasCongressman1(key)
> data
# A tibble: 297 x 8
   code_dept code_numb name_kr name_en        name_ch poto_link                              numb_elec csty                      
   <chr>     <chr>     <chr>   <chr>          <chr>   <chr>                                  <chr>     <chr>                     
 1 9770276   153       강길부  KANG GHILBOO   姜吉夫  http://www.assembly.go.kr/photo/97702~ 4선       울산 울주군               
 2 9770933   2892      강병원  KANG BYUNGWON  姜炳遠  http://www.assembly.go.kr/photo/97709~ 초선      서울 은평구을             
 3 9771036   2927      강석진  KANG SEOGJIN   姜錫振  http://www.assembly.go.kr/photo/97710~ 초선      경남 산청군함양군거창군합천군~
 4 9770512   2788      강석호  KANG SEOKHO    姜碩鎬  http://www.assembly.go.kr/photo/97705~ 3선       경북 영양군영덕군봉화군울진군~
 5 9770279   155       강창일  KANG CHANGIL   姜昌一  http://www.assembly.go.kr/photo/97702~ 4선       제주 제주시갑             
 6 9771054   2852      강효상  KHANG HYOSHANG 姜孝祥  http://www.assembly.go.kr/photo/97710~ 초선      비례대표                  
 7 9771007   2855      강훈식  KANG HOONSIK   姜勳植  http://www.assembly.go.kr/photo/97710~ 초선      충남 아산시을             
 8 9770708   2680      경대수  KYEONG DAESOO  慶大秀  http://www.assembly.go.kr/photo/97707~ 재선      충북 증평군진천군음성군   
 9 9770931   2952      고용진  KOH YONGJIN    고용진  http://www.assembly.go.kr/photo/97709~ 초선      서울 노원구갑             
10 9770961   2862      곽대훈  KWAK DAEHOON   郭大勳  http://www.assembly.go.kr/photo/97709~ 초선      대구 달서구갑             
# ... with 287 more rows
```


> **11) 국회사무처: [**국회의원 상세정보**](https://www.data.go.kr/dataset/15012647/openapi.do)(nasCongressman1)**
>
> 국회사무처가 제공하는 상세한 국회의원 정보를 제공합니다. nasCongressman1 함수로 얻을 수 있는 부서코드(code_dept)와 식별코드(code_numb)가 필요합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15012647/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키


```
# example
> key <- 'your key'
> data <- nasCongressman2(key, code_dept = '9770931', code_numb = '2952')
> data
# A tibble: 1 x 18
  name_kr name_en name_ch birth hobby able  title party csty  board elec_numb elec_vol off_tel off_web off_mail staf_adv
  <chr>   <chr>   <chr>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>     <chr>    <chr>   <chr>   <chr>    <chr>   
1 고용진  KOH YO~ 고용진  1964~ 영화감상~ 배드민턴~ "현) ~ 더불어민~ 서울 노~ 국회운영~ 초선제20대~ 20대     02-784~ http:/~ gogoyon~ 여경훈, 홍진~
# ... with 2 more variables: staf_sec1 <chr>, staf_sec2 <chr>
```


# 4. 건의 및 문의사항
추가적으로 수집을 원하시는 공공데이터포털의 데이터나 패키지에 대한 건의 및 문의사항은 환영입니다 :) lawine90(power1sky@gmail.com)에게 많은 연락 부탁드리겠습니다.
