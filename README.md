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
> **부동산 실거래가(molitRealTrade)**
> 
> 부동산 거래신고에 관한 법률에 따라 신고된 주택의 실거래 자료을 제공합니다. 주거시설의 타입은 아파트, 연립다세대, 단독/다가구 3가지이며 거래 타입은 매매, 전월세 2가지 입니다. 함수에서 사용하는 argument는 다음과 같습니다.
> - key: (필수, 문자). 공공데이터에서 발급받은 API 키.
> - year: (필수, 정수). 주거시설 거래가 발생한 년도.
> - month: (옵션, 정수). 주거시설 거래가 발생한 달. 입력하지 않을 시 해당 년도의 전체 데이터를 가져옴.
> - localeCode: (옵션, 정수). 주거시설 거래가 발생한 지역의 시/군/구 코드. localeName 미입력시 입력 필수.
> - localeName: (옵션, 문자). 주거시설 거래가 발생한 지역의 시/군/구 명. localeCode 미입력시 입력 필수.
> - houseType: (필수, 문자). 주거시설의 타입. 아파트는 "apart", 연립다세대는 "multi", 단독/다가구는 "detached"를 입력.
> - tradeType: (필수, 문자). 주거시설 거래의 타입. 매매는 "trade", 전월세는 "rent"를 입력.
> - slow: (옵션, T/F). 데이터를 받을 시 서버에 보내는 요청에 약 1초 정도의 pause를 둠. 기본값은 False.
> - viz: (옵션, T/F). 다운받은 데이터에 대한 간략한 시각화 제공. 기본값은 False(추후 구현 예정).

``` 
# example 1
> key <- 'your key'
> data <- molitRealTrade(key = key, year = 2018, localeCode = 11110,
                         houseType = "apart", tradeType = "trade")
> data$meta
# A tibble: 12 x 2
   url                                                                                                                     count
   <chr>                                                                                                                   <chr>
 1 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 88   
 2 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 83   
 3 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 78   
 4 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 43   
 5 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 58   
 6 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 55   
 7 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 65   
 8 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 112  
 9 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 70   
10 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 29   
11 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 29   
12 http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?serviceKey=5~ 11   

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

















