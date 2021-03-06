datagokR
========
[**공공데이터포털**](https://www.data.go.kr/)에서 REST API를 이용하여 데이터를 받을 수 있는 패키지입니다. 공공데이터포털에서 API 키를 발급받고 API 사용 승인을 받아야 사용하실 수 있습니다. API 사용 승인의 경우 자동승인과 심의승인이 있으며 datagokR에서는 자동승인만을 수집 대상으로 합니다. 대부분의 함수 실행 결과는 dplyr의 tbl_df 타입으로 제공되므로 dplyr 라이브러리를 load한 후 데이터를 수집하시기를 권장합니다.


# 1. 패키지 설치 방법
```
> devtools::install_github('lawine90/datagokR')
```


# 2. 현재 다운로드 가능한 데이터 목록
  - 국토교통부: [**주거시설 거래내역**](https://www.data.go.kr/dataset/3050988/openapi.do)
  - 국토교통부: [**상업시설 거래내역**](https://www.data.go.kr/dataset/3050988/openapi.do)
  - 국토교통부: [**토지 거래내역**](https://www.data.go.kr/dataset/3050988/openapi.do)
  - 기상청: [**생활기상지수**](https://www.data.go.kr/dataset/15000232/openapi.do)
  - 기상청: [**보건기상지수**](https://www.data.go.kr/dataset/15000154/openapi.do)
  - 기상청: [**종관기상관측**](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do)
  - 환경부/한국환경공단: [**시군구별 실시간 평균 대기오염**](https://www.data.go.kr/dataset/15000581/openapi.do)
  - 환경부/한국환경공단: [**시도별 실시간 대기오염 측정**](https://www.data.go.kr/dataset/15000581/openapi.do)
  - 국민건강보험공단: [**예측진료정보**](https://www.data.go.kr/dataset/15028050/openapi.do)
  - 식품의약품안전처: [**의약품 부작용**](https://www.data.go.kr/dataset/15020627/openapi.do)
  - 식품의약품안전처: [**식품 영양성분**](https://www.data.go.kr/dataset/15020625/openapi.do)
  - 식품의약품안전처: [**의약외품 제품**](https://www.data.go.kr/dataset/15028967/openapi.do)
  - 농림축산식품부: [**가축질병발생정보**](https://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20151204000000000563&service_ty=O)
  - 농림수산식품교육문화정보원: [**농수축산물 가격조사**](https://www.data.go.kr/dataset/15012298/openapi.do)
  - 농림수산식품교육문화정보원: [**농협산지공판장 경락가격**](http://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20160624000000000586&filter_ty=O&getBack=&sort_id=&s_data_nm=&instt_id=&cl_code=&shareYn=)
  - 한국농수산식품유통공사: [**농수축산물 소매가격**](http://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20141225000000000406&service_ty=O)
  - 국회사무처: [**국회의원 정보**](https://www.data.go.kr/dataset/15012647/openapi.do)
  - 국회사무처: [**국회의원 상세정보**](https://www.data.go.kr/dataset/15012647/openapi.do)
  - 국회사무처: [**최근 통과의안 목록**](https://www.data.go.kr/dataset/3037286/openapi.do)
  - 국회사무처: [**발의자별 의안 목록**](https://www.data.go.kr/dataset/3037286/openapi.do)
  - 국회사무처: [**청원 목록**](https://www.data.go.kr/dataset/3037286/openapi.do)
  - 건강보험심사평가원: [**병원평가**](https://www.data.go.kr/dataset/3048126/openapi.do)
  - 국민연금공단: [**사업장 조회**](https://www.data.go.kr/dataset/3046071/openapi.do)
  - 국민연금공단: [**사업장 상세**](https://www.data.go.kr/dataset/3046071/openapi.do)
  - 국민연금공단: [**기간별 취업/퇴직자 조회**](https://www.data.go.kr/dataset/3046071/openapi.do)
  - 서울시열린데이터광장: [**공공자전거 실시간 대여정보**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-15493&srvType=A&serviceKind=1)
  - 서울시열린데이터광장: [**버스정류장별 승하차 인원**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-12912&srvType=S&serviceKind=1)
  - 서울시열린데이터광장: [**지하철호선별 역별 승하차 인원**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-12914&srvType=A&serviceKind=1)
  - 서울시열린데이터광장: [**생필품 농수축산물 가격 정보**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-1170&srvType=S&serviceKind=1)


# 3. 각 함수 설명 및 사용법
> **1-1) [**주거시설 거래내역**](https://www.data.go.kr/dataset/3050988/openapi.do)(molitDwelling)**
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


> **1-2) [**상업시설 거래내역**](https://www.data.go.kr/dataset/3050988/openapi.do)(molitCommerce)**
> 
> 지역코드와 기간을 설정하여 해당지역, 해당기간의 상업업무용 부동산 매매 신고 자료를 제공하는 상업업무용 부동산 매매 신고 정보를 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3050988/openapi.do)의 참고문서를 확인하시거나 패키지 관리자에게 연락주시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입이며 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - year: (필수, 정수). 주거시설 거래가 발생한 년도
> - month: (옵션, 정수). 주거시설 거래가 발생한 달. 입력하지 않을 시 해당 년도의 전체 데이터를 가져옴
> - localeCode: (옵션, 정수). 주거시설 거래가 발생한 지역의 시/군/구 코드. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 주거시설 거래가 발생한 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)

``` 
# example
> key <- 'your key'
> data <- molitCommerce(key = key, year = 2018, month = 1:6, 
                        localeName = enc2utf8("수원"), slow = F, viz = T)
> data
# A tibble: 955 x 15
   code  gu    dong  landUsage tradeYear tradeMonth tradeDay price consYear consUsage consType consFloor consShare excArea
   <chr> <chr> <chr> <chr>         <int>      <int>    <int> <chr>    <int> <chr>     <chr>        <int> <lgl>     <chr>  
 1 41111 수원장안~ 파장동~ 제1종일반주거~      2018          1        4 32,0~     2016 제2종근린생활~ 집합             1 NA        103    
 2 41111 수원장안~ 정자동~ 준주거         2018          1        3 32,7~     2002 제2종근린생활~ 집합            NA NA        165    
 3 41111 수원장안~ 정자동~ 제2종일반주거~      2018          1       19 27,0~     2016 제1종근린생활~ 집합             2 NA        131    
 4 41111 수원장안~ 율전동~ 제2종일반주거~      2018          1        3 8,700     2017 제2종근린생활~ 집합             1 NA        41     
 5 41111 수원장안~ 율전동~ 일반상업       2018          1       22 27,5~     2005 제1종근린생활~ 집합            NA NA        86     
 6 41111 수원장안~ 율전동~ 일반상업       2018          1       22 27,5~     2005 제1종근린생활~ 집합            NA NA        86     
 7 41111 수원장안~ 율전동~ 일반상업       2018          1       23 24,5~     2005 제2종근린생활~ 집합            NA NA        86     
 8 41111 수원장안~ 율전동~ 제1종일반주거~      2018          1       23 11,0~     2005 제1종근린생활~ 일반            NA NA        12     
 9 41111 수원장안~ 천천동~ 제2종일반주거~      2018          1       19 5,950     1998 제1종근린생활~ 집합             1 NA        20     
10 41111 수원장안~ 천천동~ 제2종일반주거~      2018          1       22 10,3~     2009 교육연구  집합             4 NA        30     
# ... with 945 more rows, and 1 more variable: grdArea <lgl>
```


> **1-3) [**토지 거래내역**](https://www.data.go.kr/dataset/3050988/openapi.do)(molitLand)**
> 
> 지역코드와 기간을 이용하여 해당기간, 해당지역의 토지 매매 신고 자료를 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3050988/openapi.do)의 참고문서를 확인하시거나 패키지 관리자에게 연락주시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입이며 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - year: (필수, 정수). 주거시설 거래가 발생한 년도
> - month: (옵션, 정수). 주거시설 거래가 발생한 달. 입력하지 않을 시 해당 년도의 전체 데이터를 가져옴
> - localeCode: (옵션, 정수). 주거시설 거래가 발생한 지역의 시/군/구 코드. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 주거시설 거래가 발생한 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)

``` 
# example
> key <- 'your key'
> data <- molitLand(key = key, year = 2018, month = 1:6, 
                        localeName = enc2utf8("수원"), slow = F, viz = T)
> data
# A tibble: 3,264 x 11
   code  gu     dong    tradeYear tradeMonth tradeDay tradeType  price landUsage area  type             
   <chr> <chr>  <chr>       <int>      <int>    <int> <lgl>      <dbl> <chr>     <chr> <chr>            
 1 11110 종로구 청운동       2018          1        3 NA        195300 대        379   제1종일반주거지역
 2 11110 종로구 누상동       2018          1       11 NA          2194 대        4     제2종일반주거지역
 3 11110 종로구 관훈동       2018          1        5 NA         45000 도로      118   일반상업지역     
 4 11110 종로구 삼청동       2018          1        3 NA         18847 대        25    제1종일반주거지역
 5 11110 종로구 삼청동       2018          1       25 NA         28998 대        21    제1종일반주거지역
 6 11110 종로구 이화동       2018          1       30 NA         55000 대        71    제3종일반주거지역
 7 11110 종로구 동숭동       2018          1       25 NA          6278 대        16    제2종일반주거지역
 8 11110 종로구 명륜2가      2018          1       29 NA           417 대        1     제3종일반주거지역
 9 11110 종로구 숭인동       2018          1       18 NA         30000 대        19    일반상업지역     
10 11110 종로구 숭인동       2018          1       18 NA         10000 대        8     일반상업지역     
# ... with 3,254 more rows
```


> **2-1) [**생활기상지수**](https://www.data.go.kr/dataset/15000232/openapi.do)(kmaHealthIndex)**
> 
> 각종 기상자료를 응용하여 일반 국민의 일상생활 및 보건에 활용할 수 있도록 생산된 생활기상지수(부패지수, 동파지수, 체감온도 등)를 특정 위치(지점)별로 산출한 데이터입니다. 각 지수마다 다르나 3시간 단위로 측정되며 각 지수별로 수집할 수 있는 기간(월 기준)이 다르므로 아래 설명에서 수집 가능한 기간을 확인 후 사용하시기 바랍니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15000232/openapi.do)의 참고문서를 확인하시거나 패키지 관리자에게 연락주시기 바랍니다.함수 실행 결과는 R의 list 타입이며 $data에 실제 생활기상지수 데이터, $error에 에러가 발생한 데이터의 url, $url에 전체 데이터의 url이 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - localeCode: (옵션, 정수). 지역의 시/군/구 코드. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False
> - type: (필수, 문자). 8가지 생활기상지수의 타입. 각 타입에 대한 설명은 다음과 같음
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


> **2-2) [**보건기상지수**](https://www.data.go.kr/dataset/15000154/openapi.do)(kmaHealthIndex)**
> 
> 기후변화에 능동적으로 신체를 대처할 수 있는 보건기상지수(뇌졸중가능지수, 피부질환지수, 감기가능지수, 꽃가루농도위험지수 등 5종)를 지정위치(지점)별로 산출한 데이터입니다. 각 지수마다 다르나 3시간 단위로 측정되며 각 지수별로 수집할 수 있는 기간(월 기준)이 다르므로 아래 설명에서 수집 가능한 기간을 확인 후 사용하시기 바랍니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15000154/openapi.do)의 참고문서를 확인하시거나 패키지 관리자에게 연락주시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $data에 실제 보건기상지수 데이터, $error에 에러가 발생한 데이터의 url, $url에 전체 데이터의 url이 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - localeCode: (옵션, 정수). 지역의 시/군/구 코드. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False
> - type: (필수, 문자). 7가지 보건기상지수의 타입. 각 타입에 대한 설명은 다음과 같음
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


> **2-3) [**종관기상관측**](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do)(kmaASOS)**
> 
> 각 지역의 관측소에서 실측된 지상관측의 하루 기준 데이터입니다. 기온, 강수, 바람, 기압, 습도 등 날씨현상과 관련된 데이터이며 전국 94개 지점에서 수집된 데이터의 하루 평균, 최대, 최소값 등을 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **기상자료개방포털에서 발급받은 별도의 API키가 필요**합니다. 기상자료개방포털에서 제공하는 API는 매우 불안정하므로 slow argument를 TRUE로 설정하고 사용하길 권장합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $meta에는 메타데이터, $data에 관측 데이터가 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
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


> **3-1) [**시군구별 실시간 대기오염**](https://www.data.go.kr/dataset/15000581/openapi.do)(meDust)**
> 
> 시도의 각 시군구별 측정소목록의 일반 항목(아황산가스, 일산화탄소, 오존, 이산화질소, 미세먼지 등)에 대한 시간대별 평균농도를 제공합니다. 데이터는 실시간 관측된 자료이며 측정소 현지 사정이나 데이터의 수신상태에 따라 미수신될 수 있습니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15000581/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 dataframe 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 기상자료개방 포털에서 발급받은 API 키
> - localeName: (필수, 문자). 시/도 이름. 서울, 경남, 충북 등과 같이 길이 2의 형태로 입력.
> - condition: (필수, 문자). 데이터 수집 조건. 'HOUR'을 입력할 경우 함수를 실행한 시간대의 데이터를, 'DAILY'를 입력할 경우 함수를 실행한 시간대부터 24시간 전까지의 시간대별 데이터를 수집
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key from data.kma.go.kr'
> data <- meDust(key, localName = c('서울', '경기', '세종'), condition = 'HOUR')
# A tibble: 57 x 8
   time             city     so2    co    o3   no2  pm10  pm25
   <chr>            <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 1 2019-11-26 16:00 강남구 0.003   0.4 0.012 0.036    17    11
 2 2019-11-26 16:00 강동구 0.003   0.4 0.02  0.025    21    10
 3 2019-11-26 16:00 강북구 0.002   0.4 0.014 0.032    19    10
 4 2019-11-26 16:00 강서구 0.005   0.4 0.011 0.039    29    NA
 5 2019-11-26 16:00 관악구 0.003   0.3 0.017 0.03     18     9
 6 2019-11-26 16:00 광진구 0.002   0.4 0.014 0.024    10     9
 7 2019-11-26 16:00 구로구 0.003   0.3 0.021 0.025    13    10
 8 2019-11-26 16:00 금천구 0.003   0.4 0.015 0.029    16    10
 9 2019-11-26 16:00 노원구 0.004   0.4 0.016 0.028    16    12
10 2019-11-26 16:00 도봉구 0.002   0.6 0.014 0.028    13     8
# ... with 47 more rows
```


> **3-2) [**시도별 실시간 대기오염 측정**](https://www.data.go.kr/dataset/15000581/openapi.do)(meDust2)**
> 
> 시도명을 검색조건으로 하여 해당 시간의 시도별 측정소목록에 대한 일반 항목과 CAI 최종 실시간 측정값(아황산가스, 일산화탄소, 오존, 이산화질소, 미세먼지 등)과 지수 정보 조회 기능을 제공합니다. 데이터는 실시간 관측된 자료이며 측정소 현지 사정이나 데이터의 수신상태에 따라 미수신될 수 있습니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15000581/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 dataframe 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 기상자료개방 포털에서 발급받은 API 키
> - localeName: (필수, 문자). 시/도 이름. 서울, 경남, 충북 등과 같이 길이 2의 형태로 입력.
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key from data.kma.go.kr'
> data <- meDust2(key, localName = c('서울', '부산', '세종'), condition = 'HOUR')
# A tibble: 57 x 8
   time             city     so2    co    o3   no2  pm10  pm25
   <chr>            <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 1 2019-11-26 16:00 강남구 0.003   0.4 0.012 0.036    17    11
 2 2019-11-26 16:00 강동구 0.003   0.4 0.02  0.025    21    10
 3 2019-11-26 16:00 강북구 0.002   0.4 0.014 0.032    19    10
 4 2019-11-26 16:00 강서구 0.005   0.4 0.011 0.039    29    NA
 5 2019-11-26 16:00 관악구 0.003   0.3 0.017 0.03     18     9
 6 2019-11-26 16:00 광진구 0.002   0.4 0.014 0.024    10     9
 7 2019-11-26 16:00 구로구 0.003   0.3 0.021 0.025    13    10
 8 2019-11-26 16:00 금천구 0.003   0.4 0.015 0.029    16    10
 9 2019-11-26 16:00 노원구 0.004   0.4 0.016 0.028    16    12
10 2019-11-26 16:00 도봉구 0.002   0.6 0.014 0.028    13     8
# ... with 47 more rows
```


> **4-1) [**예측진료정보**](https://www.data.go.kr/dataset/15028050/openapi.do)(nhisDiseaseForcast)**
> 
> 수요조사 결과에 따라 관심도가 높은 5대 질병(눈병, 천식, 감기, 피부염, 식중독)에 대한 위험도 및 예측 진료 건수 정보를 제공합니다. 다운로드되는 데이터의 변수가 많으므로 자세한 사항을 [링크](https://www.data.go.kr/dataset/15028050/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 list 타입이며 $meta에는 메타데이터, $data에 관측 데이터, $recommanded에 각 질병의 위험도에 따른 추천행동이 포함되어 있습니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - localeCode: (옵션, 정수). 지역의 시/군/구 코드 앞 2자리. localeName 미입력시 입력 필수
> - localeName: (옵션, 문자). 지역의 시/군/구 명. localeCode 미입력시 입력 필수
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정)
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False
> - type: (필수, 문자). 7가지 보건기상지수의 타입. 각 타입에 대한 설명은 다음과 같음
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


> **5-1) [**의약품 부작용**](https://www.data.go.kr/dataset/15020627/openapi.do)(drugsSideEffect)**
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


> **5-2) [**식품 영양성분**](https://www.data.go.kr/dataset/15020625/openapi.do)(nutriComponent)**
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


> **5-3) [**의약외품 제품**](https://www.data.go.kr/dataset/15028967/openapi.do)(quasiDrugs)**
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


> **6-1) [**가축질병발생정보**](https://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20151204000000000563&service_ty=O)(livestockDisease)**
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


> **7-1) [**농수축산물 가격조사**](https://www.data.go.kr/dataset/15012298/openapi.do)(episPrice)**
>
> 안전행정부 농림수산식품교육문화정보원의 데이터로, 전국에서 거래되는 농산물, 수산물, 축산물 중 국민들의 삶에 영향을 미치는 주요 품목들에 대하여 한국농수산식품유통공사, 농업협동조합중앙회, 수산업협동조합중앙회 및 축산물품질평가원에 소속된 조사원들이 조사한 가격 정보와 거래되는 상품들의 상세 정보를 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/15012298/openapi.do)의 참고문서로 확인하시기 바라며 가격조사 방법에 대한 자세한 사항은 [링크](https://www.kamis.or.kr/customer/price/knowhow/knowhow.do?action=methodList)에서 확인하시기 바랍니다. 참고로 농수축산물 가격조사는 주말 및 휴일에는 실시하지 않으니 데이터 또한 주말 및 휴일의 가격은 없으므로 이를 유의하여 date를 입력하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - date: (필수, 문자). 데이터를 수집하려는 날짜로, YYYYMMDD 형식으로 입력 ex) '20191203'
> - items: (필수, 정수). 데이터를 수집하려는 품목의 코드로, library에 내장된 epis_items 데이터의 itemCode 참조
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key from data.go.kr'
> epis_items <- datagokR::epis_items
> epis_items
# A tibble: 351 x 4
   itemCode itemName speciesCode speciesName
      <int> <chr>          <int> <chr>      
 1      112 찹쌀               1 일반찹쌀   
 2      112 찹쌀               2 통일계     
 3      121 보리쌀             1 일반       
 4      121 보리쌀             2 과맥       
 5      121 보리쌀             3 정부방출맥 
 6      122 밀                 0 일반       
 7      131 좁쌀               0 메조       
 8      132 옥수수             0 일반       
 9      141 콩                 1 백태(국산) 
10      141 콩                 2 콩나물콩   
# ... with 341 more rows
> 
> data <- episPrice(key, date = '20191203', items = head(unique(epis_items$itemCode)))
> data
# A tibble: 99 x 13
   locCode locName  mrkCode mrkName           itmCode itmName spcCode spcName yesPrice todPrice unit  exmGradeCode exmGradeName
   <chr>   <chr>    <chr>   <chr>             <chr>   <chr>   <chr>   <chr>      <int>    <int> <chr> <chr>        <chr>       
 1 1102    서울서부 4002513 경동시장          0104    찹쌀    010401  일반계      3400     3400 1KG   1            상(1등급)   
 2 1102    서울서부 4004522 복조리시장        0104    찹쌀    010401  일반계      4830     4830 1KG   1            상(1등급)   
 3 1102    서울서부 4007308 영등포 유통상가   0104    찹쌀    010401  일반계      4160     4160 1KG   1            상(1등급)   
 4 1104    서울     7005601 롯데마트 잠실점   0104    찹쌀    010401  일반계      4700     4700 1KG   1            상(1등급)   
 5 1104    서울     7008501 홈플러스 금천점   0104    찹쌀    010401  일반계      3150     3150 1KG   1            상(1등급)   
 6 1104    서울     7003402 이마트 수색점     0104    찹쌀    010401  일반계      6450     6450 1KG   1            상(1등급)   
 7 1104    서울     7006708 하나로클럽 양재점 0104    찹쌀    010401  일반계      3730     3730 1KG   1            상(1등급)   
 8 1104    서울     7002502 롯데마트 청량리점 0104    찹쌀    010401  일반계      4700     4700 1KG   1            상(1등급)   
 9 2100    부산     4047203 부전시장          0104    찹쌀    010401  일반계      4000     4000 1KG   1            상(1등급)   
10 2104    부산     7047203 홈플러스 가야점   0104    찹쌀    010401  일반계      3150     3150 1KG   1            상(1등급)   
# ... with 89 more rows
```


> **7-2) [**농협산지공판장 경락가격**](http://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20160624000000000586&filter_ty=O&getBack=&sort_id=&s_data_nm=&instt_id=&cl_code=&shareYn=)(episJoint)**
>
> 안전행정부 농림수산식품교육문화정보원의 데이터로, 농수축산물 경락가격정보를 조회하기 위한 서비스로서 농산물, 축산물, 수산물, 화훼류에 대한 도매시장, 산지공판장, 종합유통센터 별로 농수축산물 경락가격을 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **농림축산식품 공공데이터 포털에서 발급받은 별도의 API키가 필요**합니다. 자세한 사항은 [링크](http://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20160624000000000586&filter_ty=O&getBack=&sort_id=&s_data_nm=&instt_id=&cl_code=&shareYn=)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - date: (필수, 문자). 데이터를 수집하려는 날짜로, YYYYMMDD 형식으로 입력 ex) '20191203'
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key issued from data.mafra.go.kr'
> 
> data <- episJoint(key, '20191207', T)
  |==============================================================================================| 100%
> data
# A tibble: 14,701 x 19
   date  jmrkName jmrkCode jmrkType orgName orgCode itmName itmCode spcName spcCode grdName grdCode unit  stdd  minPrice avgPrice
   <chr> <chr>    <chr>    <chr>    <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr> <chr>    <chr>   
 1 2019~ 광주공판장(공~ 8808990~ 산지     전남 진도군~ 539000  보리    0201    기타    020199  특      11      1     "kg ~ 8000     8000    
 2 2019~ 광주공판장(공~ 8808990~ 산지     전남 나주시~ 520951  보리    0201    기타    020199  특      11      4     "kg ~ 5000     5000    
 3 2019~ 광주공판장(공~ 8808990~ 산지     전남 진도군~ 539000  보리    0201    기타    020199  특      11      4     "kg ~ 6600     6600    
 4 2019~ 광주원예농업협~ 8808990~ 산지     전남 나주시~ 520951  보리    0201    기타    020199  특      11      4     "kg ~ 5000     6400    
 5 2019~ 인천원예농협(~ 8808990~ 산지     충남 예산군~ 340860  콩      0301    콩(일반)~ 030101  특      11      4     kg 그~ 12000    12000   
 6 2019~ 인천원예농협(~ 8808990~ 산지     충남 부여군~ 323871  콩      0301    서리태  030107  특      11      1     kg 상~ 7200     7200    
 7 2019~ 인천원예농협(~ 8808990~ 산지     충남 예산군~ 340860  콩      0301    서리태  030107  특      11      1     kg 상~ 7000     7000    
 8 2019~ 인천원예농협구~ 8808990~ 산지     충남 당진군~ 343000  콩      0301    서리태  030107  특      11      10    "kg ~ 77000    77000   
 9 2019~ 전주원예농협(~ 8808990~ 산지     전북 전주시~ 561302  콩      0301    서리태  030107  특      11      1     "kg ~ 7800     7800    
10 2019~ 구미농협공판장~ 8808990~ 산지     경북 김천시~ 740890  콩      0301    백태    030118  특      11      1     "kg ~ 5000     5000    
# ... with 14,691 more rows, and 3 more variables: maxPrice <chr>, vol <chr>, cnt <chr>
```


> **8-1) [**농수축산물 소매가격**](http://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20141225000000000406&service_ty=O)(atPrice)**
>
> 한국농수산식품유통공사의 데이터로, 농수축산물 도소매가격을 정보수요자에게 제공하여 시장출하 및 매매에 관한 의사결정, 원활한 수급조절 유도 및 가격안정대책 추진을 위한 기초자료를 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **농림축산식품 공공데이터 포털에서 발급받은 별도의 API키가 필요**합니다. 자세한 사항은 [링크](http://data.mafra.go.kr/opendata/data/indexOpenDataDetail.do?data_id=20141225000000000406&service_ty=O)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - date: (필수, 문자). 데이터를 수집하려는 날짜로, YYYYMMDD 형식으로 입력 ex) '20191203'
> - vebose: (옵션, T/F) 다운로드의 진행상황의 콘솔 출력여부. 기본값은 False

```
# example
> key <- 'your key issued from data.mafra.go.kr'
> 
> data <- atPrice(key, date = '20191210')
  |==============================================================================================| 100%
> data
# A tibble: 4,064 x 15
   date     locName locCode mrkName mrkCode catName  catCode itmName    itmCode spcName       spcCode grdName grdCode unit  price
   <chr>    <chr>   <chr>   <chr>   <chr>   <chr>    <chr>   <chr>      <chr>   <chr>         <chr>   <chr>   <chr>   <chr> <dbl>
 1 20191210 인천    2300    E-유통  0230401 특용작물 300     아몬드     319     수입          00      중품    05      100g   1430
 2 20191210 인천    2300    E-유통  0230401 과일류   400     사과       411     후지          05      상품    04      10개  17500
 3 20191210 인천    2300    E-유통  0230401 과일류   400     배         412     신고          01      상품    04      10개  29930
 4 20191210 인천    2300    E-유통  0230401 과일류   400     배         412     신고          01      중품    05      10개  22450
 5 20191210 인천    2300    E-유통  0230401 과일류   400     감귤       415     노지          01      M과     15      10개   2980
 6 20191210 인천    2300    E-유통  0230401 과일류   400     단감       416     단감          00      상품    04      10개  11960
 7 20191210 인천    2300    E-유통  0230401 과일류   400     바나나     418     수입          02      상품    04      100g    268
 8 20191210 인천    2300    E-유통  0230401 과일류   400     참다래     419     그린 뉴질랜드 02      상품    04      10개   6980
 9 20191210 인천    2300    E-유통  0230401 과일류   400     파인애플   420     수입          02      중품    05      1개    2980
10 20191210 인천    2300    E-유통  0230401 채소류   200     방울토마토 422     방울토마토    01      상품    04      1kg    7980
# ... with 4,054 more rows
```


> **9-1) [**국회의원 정보**](https://www.data.go.kr/dataset/15012647/openapi.do)(nasCongressman1)**
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


> **9-2) [**국회의원 상세정보**](https://www.data.go.kr/dataset/15012647/openapi.do)(nasCongressman1)**
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


> **9-3) [**최근 통과의안 목록**](https://www.data.go.kr/dataset/3037286/openapi.do)(nasPassedBill)**
>
> 최근 6개월간 통과된 의안 목록을 조회하는 기능을 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3037286/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키


```
# example
> key <- 'your key'
> data <- nasPassedBill(key)
> data
# A tibble: 387 x 8
   id                  no     name                                          proposer propose_date committee vote_date vote_result
   <chr>               <chr>  <chr>                                         <chr>    <chr>        <chr>     <chr>     <chr>      
 1 PRC_Y1Q9L1V0Y3G1Q1~ 20233~ 위문금 갹출의 건                              의장     2019-10-31   본회의    2019-10-~ 원안가결   
 2 PRC_U1F9E0X9K2J4M1~ 20232~ 초·중등교육법 일부개정법률안(대안)            위원장   2019-10-30   교육위원회~ 2019-10-~ 원안가결   
 3 PRC_Z1G9T0Q9W2T3P1~ 20232~ 교육시설 등의 안전 및 유지관리 등에 관한 법률안~ 위원장   2019-10-30   교육위원회~ 2019-10-~ 원안가결   
 4 PRC_S1K9T0H8G1S9W1~ 20232~ 재외국민의 교육지원 등에 관한 법률 일부개정법률~ 위원장   2019-10-30   교육위원회~ 2019-10-~ 원안가결   
 5 PRC_B1R9H0R8B1P9E1~ 20232~ 한국교직원공제회법 일부개정법률안(대안)       위원장   2019-10-30   교육위원회~ 2019-10-~ 원안가결   
 6 PRC_I1G9T0L8O1X9M1~ 20232~ 평생교육법 일부개정법률안(대안)               위원장   2019-10-30   교육위원회~ 2019-10-~ 원안가결   
 7 PRC_B1S9P0A8Y2Q1J1~ 20232~ 군용항공기 운용 등에 관한 법률 일부개정법률안(대~ 위원장   2019-10-30   국방위원회~ 2019-10-~ 원안가결   
 8 PRC_K1B9G0A8E2H1S1~ 20232~ 군용비행장·군사격장 소음 방지 및 피해 보상에 관한~ 위원장   2019-10-30   국방위원회~ 2019-10-~ 원안가결   
 9 PRC_Z1I9M0C8R2J2Z1~ 20232~ 금융회사부실자산 등의 효율적 처리 및 한국자산관리~ 위원장   2019-10-30   정무위원회~ 2019-10-~ 원안가결   
10 PRC_R1T9K0Q8R2Q1W1~ 20232~ 군인의 지위 및 복무에 관한 기본법 일부개정법률안~ 위원장   2019-10-30   국방위원회~ 2019-10-~ 원안가결   
# ... with 377 more rows
```


> **9-4) [**발의자별 의안 목록**](https://www.data.go.kr/dataset/3037286/openapi.do)(nasBillSearch)**
>
> 법률안 발의자의 이름을 조건으로 발의 의안의 목록과 결과를 조회하는 기능을 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3037286/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - name: (필수, 문자). 발의자의 국문 이름. nasCongressman1 함수에서 국회의원 이름을 확인 후 사용


```
# example
> key <- 'your key'
> data <- nasBillSearch(key, name = enc2utf8('경대수'))
> data
# A tibble: 462 x 9
   id                no     name                             proposer    propose_type propose_date vote_date vote_result condition
   <chr>             <chr>  <chr>                            <chr>       <chr>        <chr>        <chr>     <chr>       <chr>    
 1 PRC_W1O6D0O4J2Q9~ 19186~ 자유무역협정 체결에 따른 농어업인 등~ 홍문표의원 등 13~ 공동발의     2016-04-29   2016-05-~ 임기만료폐기~ 소관위접수~
 2 PRC_C1D6Y0N2Q2F5~ 19185~ 재외국민의 교육지원 등에 관한 법률~ 양창영의원 등 10~ 공동발의     2016-02-25   2016-05-~ 임기만료폐기~ 소관위접수~
 3 PRC_L1W6F0F2Y0K2~ 19185~ 국립대학법인 서울대학교 설립·운영에 ~ 유기홍의원 등 29~ 공동발의     2016-02-02   2016-05-~ 임기만료폐기~ 소관위심사~
 4 PRC_X1E6Z0Q1B2Y2~ 19184~ 태권도 진흥 및 태권도공원 조성 등에 ~ 신성범의원 등 12~ 공동발의     2016-01-22   2016-05-~ 수정가결    공포     
 5 PRC_C1F5M1W2G3S1~ 19184~ 농어업인 삶의 질 향상 및 농어촌지역 ~ 경대수의원 등 10~ 대표발의     2015-12-31   2016-05-~ 임기만료폐기~ 소관위심사~
 6 PRC_Y1L5M1O2D2P9~ 19183~ 문화산업진흥 기본법 일부개정법률안~ 신성범의원 등 10~ 공동발의     2015-12-29   2016-05-~ 임기만료폐기~ 소관위심사~
 7 PRC_Z1X5J1O2K1A0~ 19181~ 학교폭력예방 및 대책에 관한 법률 일~ 박대출의원 등 12~ 공동발의     2015-12-10   2016-05-~ 임기만료폐기~ 소관위심사~
 8 PRC_W1Y5R1S2M1Z0~ 19181~ 체육시설의 설치·이용에 관한 법률 일~ 박대출의원 등 11~ 공동발의     2015-12-10   2016-05-~ 임기만료폐기~ 소관위심사~
 9 PRC_N1J5P1M2A0L9~ 19181~ 원양산업발전법 일부개정법률안    윤명희의원 등 10~ 공동발의     2015-12-09   2016-05-~ 임기만료폐기~ 소관위심사~
10 PRC_S1Z5O1A2Q0D9~ 19181~ 해사안전법 일부개정법률안        윤명희의원 등 10~ 공동발의     2015-12-09   2016-05-~ 임기만료폐기~ 소관위심사~
# ... with 452 more rows
```


> **9-5) [**의안 국회의원 목록**](https://www.data.go.kr/dataset/3037286/openapi.do)(nasBillProposer)**
>
> 법률안의 고유 id를 조건으로 의안을 제안한 국회의원의 목록을 조회하는 기능을 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3037286/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - id: (필수, 문자). 의안의 고유 id. nasBillSearch 함수에서 의안의 id을 확인 후 사용


```
# example
> key <- 'your key'
> data <- nasBillProposer(key, id = 'PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3')
> data
# A tibble: 30 x 6
   id                                 type1 type2              conType name   party       
   <chr>                              <chr> <chr>              <chr>   <chr>  <chr>       
 1 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 의안  접수정보목록       발의자  박홍근 더불어민주당
 2 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 청원  접수정보목록       발의자  박홍근 더불어민주당
 3 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 의안  본회의 수정안 목록 발의자  박홍근 더불어민주당
 4 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 의안  접수정보목록       발의자  서영교 더불어민주당
 5 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 의안  본회의 수정안 목록 발의자  서영교 더불어민주당
 6 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 청원  접수정보목록       발의자  서영교 더불어민주당
 7 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 의안  본회의 수정안 목록 발의자  유인태 더불어민주당
 8 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 의안  접수정보목록       발의자  유인태 더불어민주당
 9 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 청원  접수정보목록       발의자  유인태 더불어민주당
10 PRC_L1E6Z0K1F2V8U1N7M3V5N1Y6V6G5U3 의안  본회의 수정안 목록 발의자  이원욱 더불어민주당
# ... with 20 more rows
```


> **9-6) [**청원 목록**](https://www.data.go.kr/dataset/3037286/openapi.do)(nasPetitionSearch)**
>
> 소개 의원의 이름을 조건으로 청원 목록과 그 결과를 조회하는 기능을 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3037286/openapi.do)의 참고문서로 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - name: (필수, 문자). 청원 소개 의원의 국문 이름. nasCongressman1 함수에서 국회의원 이름을 확인 후 사용


```
# example
> key <- 'your key'
> data <- nasPetitionSearch(key)
> data
# A tibble: 227 x 9
   id            no     name                         approver          proposer   propose_date committee     vote_date vote_result
   <chr>         <chr>  <chr>                        <chr>             <chr>      <chr>        <chr>         <chr>     <chr>      
 1 PRC_T1Y6P0C3~ 19002~ 선거구 획정의 개선           강기윤의원        오용외 2인 2016-03-03   안전행정위원회~ 2016-05-~ 임기만료폐기~
 2 PRC_Q1H6L0R2~ 19002~ 영화 및 비디오물의 진흥에 관한~ 김태년의원        안진걸     2016-02-19   교육문화체육관광위원회~ 2016-05-~ 임기만료폐기~
 3 PRC_V1M6P0M2~ 19002~ 4.16세월호 참사 진상규명 및 안~ 전해철의원        전명선외 53,5~ 2016-02-19   농림축산식품해양수산위원~ 2016-05-~ 임기만료폐기~
 4 PRC_Q1A6E0D1~ 19002~ 자동차보험표준약관 개악 중단 ~ 김을동의원        김주평외 12인~ 2016-01-07   정무위원회    2016-05-~ 임기만료폐기~
 5 PRC_X1R5U1W2~ 19002~ 노인복지법 일부개정 입법에 관~ 우원식의원        김영훈     2015-12-15   보건복지위원회~ 2016-05-~ 임기만료폐기~
 6 PRC_I1R5Q1T2~ 19002~ 가출청소년 보호를 위한 후견인~ 최민희의원        김진주외 300인~ 2015-12-15   보건복지위원회~ 2016-05-~ 임기만료폐기~
 7 PRC_W1H5W1B2~ 19002~ 대일항쟁기강제동원피해조사 및~ 이명수의원        안부수외 35,0~ 2015-12-07   안전행정위원회~ 2016-05-~ 임기만료폐기~
 8 PRC_I1P5Y1O2~ 19002~ 경찰 및 검찰의 비리 의혹 수사~ 장하나의원        마승관     2015-12-07   법제사법위원회~ 2016-05-~ 임기만료폐기~
 9 PRC_T1L5L1E2~ 19002~ 국유재산관리기금 거창교정시설~ 전해철의원        김은옥외 2,56~ 2015-12-01   법제사법위원회~ 2016-05-~ 임기만료폐기~
10 PRC_W1A5F1Z1~ 19002~ 도시 및 주거환경정비법 개정  송호창의원ㆍ정성호의원ㆍ김경협의~ 한용운외 7인~ 2015-11-23   국토교통위원회~ 2016-05-~ 임기만료폐기~
# ... with 217 more rows
```


> **10-1) [**병원평가**](https://www.data.go.kr/dataset/3048126/openapi.do)(hiraCancerAssess)**
>
> 수술, 질병, 약제사용 등 병원의 의료서비스를 의·약학적 측면과 비용효과적 측면에서 평가한 결과 정보를 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3048126/openapi.do)의 참고문서로 확인하시기 바랍니다. 현재는 간암, 위암 수술 사망률에 대한 평가 정보만을 제공하고 있으며 평가 등급은 1(좋음) ~ 5(나쁨), NA는 수술건수가 10건 미만으로 평가대상에서 제외되는 병원입니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키


```
# example
> key <- 'your key'
> data <- nasPetitionSearch(key)
> data
# A tibble: 257 x 8
   type_code type     name                            addr_code    addr_name1  addr_name2 liver   gastric
   <chr>     <chr>    <chr>                           <int>        <chr>    <chr>         <int>   <int>
 1 21        병원     세웅병원                        210011       부산    부산금정구     NA      0
 2 21        병원     의료법인서광의료재단 성북중앙~  110012       서울    성북구         NA      NA
 3 11        종합병원 의료법인 인당의료재단 부민병원  210005       부산    부산북구       NA      0
 4 11        종합병원 서울특별시 동부병원             110007       서울    동대문구       NA      0
 5 11        종합병원 대림성모병원                    110013       서울    영등포구       NA      1
 6 21        병원     푸른사랑병원                    320006       강원    영월군         NA      NA
 7 31        의원     주함외과의원                    110023       서울    광진구         NA      NA
 8 11        종합병원 인천광역시의료원                220002       인천    인천동구       NA      0
 9 01        상급종합 아주대학교병원                  310604       경기    수원영통구     1       1
10 11        종합병원 의료법인보원의료재단 경희의료~  380100       경남    김해시         NA      NA
# ... with 247 more rows
```


> **11-1) [**사업장 조회**](https://www.data.go.kr/dataset/3046071/openapi.do)**
>
> 국민연금에 가입한 사업장에 대한 간략한 정보를 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3046071/openapi.do)의 참고문서로 확인하시기 바랍니다. 사업자 등록번호의 일부(사업자 등록번호 앞 6자리)를 이용하여 회사를 검색하기 때문에 정확한 사업자는 함수 실행 후 사업자명(name)에서 다시 필터링 하시기 바랍니다. 해당 데이터는 국민연금공단에서 매달 15일 업데이트 합니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - regist_numb: (필수, 문자). 검색하려는 사업자의 사업자등록번호 앞 6자리
> - vebose: (옵션, T/F) 다운로드 진행상황의 콘솔 출력여부. 기본값은 False


```
# example
> key <- 'your key'
> data <- npsCorp(key, regist_numb = '208812')
> 
> data
# A tibble: 1,900 x 10
   upDate registNumb id     name             join  type  addr                             addrCode1 addrCode2 addrCode3
   <chr>  <chr>      <chr>  <chr>            <chr> <chr> <chr>                            <chr>     <chr>     <chr>    
 1 201906 208812**** 174464 서울다비산업(주) 등록  법인  서울특별시 중구 난계로           11        140       165      
 2 201906 208812**** 174473 정우씨엘에스(주) 등록  법인  경기도 의정부시 오목로225번길    41        150       106      
 3 201906 208812**** 174478 한독콘트롤즈(주) 등록  법인  서울특별시 구로구 구로중앙로     11        530       102      
 4 201906 208812**** 174480 효다섬유(주)     등록  법인  서울특별시 종로구 종로           11        110       164      
 5 201906 208812**** 174489 진화섬유(주)     등록  법인  서울특별시 종로구 율곡로14길     11        110       166      
 6 201906 208812**** 174492 (주)아람플러스   등록  법인  서울특별시 종로구 종로56길       11        110       175      
 7 201906 208812**** 174496 태두합성(주)     등록  법인  서울특별시 성동구 성수이로       11        200       115      
 8 201906 208812**** 174504 강성전기(주)     등록  법인  서울특별시 중구 청계천로         11        140       152      
 9 201906 208812**** 174512 주식회사비원섬유 등록  법인  서울특별시 동대문구 왕산로19가길 11        230       102      
10 201906 208812**** 174514 주식회사더블유투 등록  법인  울산광역시 남구 삼산로383번길    31        140       106      
# ... with 1,890 more rows
```


> **11-2) [**사업장 상세**](https://www.data.go.kr/dataset/3046071/openapi.do)**
>
> 국민연금에 가입한 사업장에 대한 상세한 정보를 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3046071/openapi.do)의 참고문서로 확인하시기 바랍니다. npsCorp 함수 실행 결과에 포함되어 있는 'id'를 사용합니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - id: (필수, 문자). 사업자의 고유 id. npsCorp 함수 실행 결과에 포함된 id column의 값.


```
# example
> key <- 'your key'
> data <- npsPension(key, id = '174489')
> 
> data
# A tibble: 1 x 14
  registNumb name     wrkCode wrkName      join  inDate outDate type  member amnt  addr          addrCode1 addrCode2 addrCode3
  <chr>      <chr>    <chr>   <chr>        <chr> <chr>  <chr>   <chr> <chr>  <chr> <chr>         <chr>     <chr>     <chr>    
1 208812**** 진화섬유(주)~ 171109  화학섬유직물 직조업~ 등록  20041~ 000101~ 법인  3      7025~ 서울특별시 종로구 율곡~ 11        110       166   
```


> **11-3) [**기간별 취업/퇴직자 조회**](https://www.data.go.kr/dataset/3046071/openapi.do)**
>
> 국민연금에 가입한 사업장의 월별 국민연금 가입자, 탈퇴자 수를 제공합니다. 자세한 사항은 [링크](https://www.data.go.kr/dataset/3046071/openapi.do)의 참고문서로 확인하시기 바랍니다. npsCorp 함수 실행 결과에 포함되어 있는 'id'를 사용합니다. 또한 같은 사업장이라 할지라도 국민연금공단에서 조사한 달(npsCorp 함수 실행 결과에 포함되어 있는 'upDate')에 다라 'id'가 다르기 때문에 정확한 가입자/탈퇴자 수의 확인을 위해선 npsCorp 함수의 upDate와 id column을 매칭하여 입력하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터 포털에서 발급받은 API 키
> - id: (필수, 문자). 사업자의 고유 id. npsCorp 함수 실행 결과에 포함된 id column의 값.
> - month: (필수, 문자). 가입자/탈퇴자 수를 확인하려는 달. npsCorp 함수 실행 결과에 포함된 upDate column의 값.


```
# example
> key <- 'your key'
> data <- npsPensionInOut(key, id = '674474', month = '201905')
> 
> data
# A tibble: 1 x 3
  month  memberIn memberOut
  <chr>     <dbl>     <dbl>
1 201905       13         7
```


> **12-1) [**공공자전거 실시간 대여정보**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-15493&srvType=A&serviceKind=1)(seoulBike)**
>
> 서울특별시 공공자전거 실시간 대여정보로, 대여소별 실시간 자전거 대여가능 건수, 거치율, 대여소 위치정보를 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **서울열린데이터광장에서 발급받은 별도의 API키가 필요**합니다. 자세한 사항은 [링크](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-15493&srvType=A&serviceKind=1)를 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 서울열린데이터광장에서 발급받은 API 키


```
# example
> key <- 'your key from data.seoul.go.kr'
> data <- seoulBike(key)
> data
# A tibble: 1,526 x 7
   id    name                               capa count share  lati longi
   <chr> <chr>                             <dbl> <dbl> <dbl> <dbl> <dbl>
 1 ST-3  101. (구)합정동 주민센터              5     0     0  37.5  127.
 2 ST-4  102. 망원역 1번출구 앞               20    19    95  37.6  127.
 3 ST-5  103. 망원역 2번출구 앞               14    10    71  37.6  127.
 4 ST-6  104. 합정역 1번출구 앞               13     6    46  37.6  127.
 5 ST-8  106. 합정역 7번출구 앞               10     7    70  37.5  127.
 6 ST-9  107. 신한은행 서교동금융센터점 앞     5     4    80  37.6  127.
 7 ST-10 108. 서교동 사거리                   10     5    50  37.6  127.
 8 ST-11 109. 제일빌딩 앞                     10     9    90  37.5  127.
 9 ST-13 110. 사천교                          20    16    80  37.6  127.
10 ST-15 111. 상수역 2번출구 앞               10     3    30  37.5  127.
# ... with 1,516 more rows
```


> **12-2) [**서울시 버스정류장별 승하차 인원**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-12912&srvType=S&serviceKind=1)(seoulBusCount)**
>
> 교통카드(선후불교통카드)를 이용한 서울버스 노선별/정류장별 승하차인원을 나타내는 정보로, 일별 버스노선마다 각 정류장에 승/하차한 데이터의 합입니다. 공공데이터 포털에서 발급받은 API키가 아닌, **서울열린데이터광장에서 발급받은 별도의 API키가 필요**합니다. 자세한 사항은 [링크](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-12912&srvType=S&serviceKind=1)를 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 서울열린데이터광장에서 발급받은 API 키
> - day: (필수, 문자). YYYYMMDD 형식의 문자열로 표현된 날짜. 20160101 ~ 현재 -5일의 데이터만 확인 가능.
> - vebose: (옵션, T/F) 다운로드 진행상황의 콘솔 출력여부. 기본값은 False


```
# example
> key <- 'your key from data.seoul.go.kr'
> data <- seoulBusCount(key, day = gsub('-', '', Sys.Date()-4), verbose = T)
  |================================================================================================| 100%
> data
# A tibble: 38,559 x 10
   bus_id   bus_no bus_name               stop_id   stop_ars sta_id  sta_name                 in_numb out_numb day     
   <chr>    <chr>  <chr>                  <chr>     <chr>    <chr>   <chr>                      <dbl>    <dbl> <chr>   
 1 11110001 100    100번(하계동~용산구청) 100000003 01003    0007197 명륜3가.성대입구             124      219 20191115
 2 11110001 100    100번(하계동~용산구청) 101000057 02156    0008839 을지로입구.로얄호텔          145      158 20191115
 3 11110001 100    100번(하계동~용산구청) 101000060 02159    0008864 을지로2가.파인에빈뉴         233      102 20191115
 4 11110001 100    100번(하계동~용산구청) 102000173 03267    0008878 서빙고역교차로                21       15 20191115
 5 11110001 100    100번(하계동~용산구청) 101000061 02160    0008881 을지로3가                    109      300 20191115
 6 11110001 100    100번(하계동~용산구청) 101000062 02161    0008890 을지로3가                    113       77 20191115
 7 11110001 100    100번(하계동~용산구청) 101000063 02162    0008935 을지로4가                     69      255 20191115
 8 11110001 100    100번(하계동~용산구청) 100000102 01198    0008952 원남동                       198      204 20191115
 9 11110001 100    100번(하계동~용산구청) 101000067 02166    0008984 을지로5가.중부시장            34       56 20191115
10 11110001 100    100번(하계동~용산구청) 101000070 02169    0009038 을지로6가.국립중앙의료원      57       65 20191115
# ... with 38,549 more rows
```


> **12-3) [**서울시 지하철호선별 역별 승하차 인원**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-12914&srvType=A&serviceKind=1)(seoulTubeCount)**
>
> 교통카드(선후불교통카드 및 1회용 교통카드)를 이용한 지하철호선별 역별(서울교통공사, 한국철도공사, 공항철도, 9호선) 승하차인원을 나타내는 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **서울열린데이터광장에서 발급받은 별도의 API키가 필요**합니다. 자세한 사항은 [링크](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-12914&srvType=A&serviceKind=1)를 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 서울열린데이터광장에서 발급받은 API 키
> - day: (필수, 문자). YYYYMMDD 형식의 문자열로 표현된 날짜. 20160101 ~ 현재 -5일의 데이터만 확인 가능.


```
# example
> key <- 'your key from data.seoul.go.kr'
> data <- seoulTubeCount(key, day = gsub('-', '', Sys.Date()-4))
> data
# A tibble: 591 x 5
   line  station                in_numb out_numb day     
   <chr> <chr>                    <dbl>    <dbl> <chr>   
 1 1호선 서울역                   60547    59385 20191116
 2 1호선 시청                     26202    27913 20191116
 3 1호선 종각                     40626    37222 20191116
 4 1호선 종로3가                  37708    35868 20191116
 5 1호선 종로5가                  29893    30538 20191116
 6 1호선 동대문                   16330    18089 20191116
 7 1호선 신설동                   14164    13213 20191116
 8 1호선 제기동                   20137    20720 20191116
 9 1호선 청량리(서울시립대입구)   25576    26989 20191116
10 1호선 동묘앞                   14038    15215 20191116
# ... with 581 more rows
```


> **12-4) [**서울시 생필품 농수축산물 가격 정보**](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-1170&srvType=S&serviceKind=1)(seoulNecessaries)**
>
> 주 2회(화, 금) 자치구별 전통시장과 대형마트의 농수축산물 16개 품목의 가격을 제공합니다. 공공데이터 포털에서 발급받은 API키가 아닌, **서울열린데이터광장에서 발급받은 별도의 API키가 필요**합니다. 자세한 사항은 [링크](http://data.seoul.go.kr/dataList/datasetView.do?infId=OA-12914&srvType=A&serviceKind=1)를 확인하시기 바랍니다. 함수 실행 결과는 R의 data.frame 타입입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 서울열린데이터광장에서 발급받은 API 키
> - recent: (옵션, T/F). 최근 7,000개의 데이터만 받을 경우 True로 설정
> - vebose: (옵션, T/F). 다운로드 진행상황의 콘솔 출력여부. 기본값은 False


```
# example
> key <- 'your key from data.seoul.go.kr'
> data <- seoulNecessaries(key, recent = F, verbose = F)
> data
# A tibble: 113,639 x 10
        id market_id market_name market_type market_dir ness_id ness_name          ness_unit ness_price date      
     <dbl>     <dbl> <chr>       <chr>       <chr>        <dbl> <chr>              <chr>     <chr>      <chr>     
 1 1480019        56 롯데백화점  대형마트    중구           305 사과(부사, 300g)   1개       2000       2019-11-14
 2 1480020        56 롯데백화점  대형마트    중구           306 배(신고, 600g)     1개       4000       2019-11-14
 3 1480021        56 롯데백화점  대형마트    중구           307 배추(2.5~3kg)      1포기     12000      2019-11-14
 4 1480022        56 롯데백화점  대형마트    중구           282 무(세척무)         1개       3980       2019-11-14
 5 1480023        56 롯데백화점  대형마트    중구           309 양파(1.5kg망)      1망       4980       2019-11-14
 6 1480024        56 롯데백화점  대형마트    중구            23 상추               100g      1986       2019-11-14
 7 1480025        56 롯데백화점  대형마트    중구           311 오이(다다기)       1개       1290       2019-11-14
 8 1480026        56 롯데백화점  대형마트    중구           312 애호박             1개       2580       2019-11-14
 9 1480027        56 롯데백화점  대형마트    중구           278 쇠고기             600g1등급 40800      2019-11-14
10 1480028        56 롯데백화점  대형마트    중구            99 돼지고기(생삼겹살) 600g      23940      2019-11-14
# ... with 113,629 more rows
```


# 4. 건의 및 문의사항
추가적으로 수집을 원하시는 공공데이터포털의 데이터나 패키지에 대한 건의 및 문의사항은 환영입니다 :) lawine90(power1sky@gmail.com)에게 많은 연락 부탁드리겠습니다.
