# 2021년 보궐 지방선거 개표결과 - 서울/부산시장

# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(rvest)
library(httr)
library(testthat)

# 1. 스크립트 테스트 -----
# 출처: http://info.nec.go.kr

## 1.1 POST() 함수를 사용하여 시도지사선거의 서울특별시 종로구 개표 결과를 수집합니다.
resp <- POST(
    url = 'http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml',
    encode = 'form',
    body = list(
        electionId = '0020210407',
        requestURI = '/WEB-INF/jsp/electioninfo/0020210407/vc/vccp08.jsp',
        topMenuId = 'VC',
        secondMenuId = 'VCCP08',
        menuId = 'VCCP08',
        statementId = 'VCCP08_#3',
        electionCode = '3',
        cityCode = '1100',
        townCode = '1101'
    )
)

## 1.2.서울특별시장 선거의 서울특별시 종로구 개표 결과를 추출합니다.
Sys.setlocale("LC_ALL", "C")
vote_dat <- content(x=resp, as = 'text') %>%
    read_html() %>%
    html_node(css = 'table') %>%
    html_table(fill = TRUE)

Sys.setlocale("LC_ALL", "Korean")

# 2. 함수 -----
# 출처: http://info.nec.go.kr

## 2.1 서울시 전체구에 대한 함수

get_vote_data <- function(si_code, gu_code) {

    resp <- POST(
        url = 'http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml',
        encode = 'form',
        body = list(
            electionId = '0020210407',
            requestURI = '/WEB-INF/jsp/electioninfo/0020210407/vc/vccp08.jsp',
            topMenuId = 'VC',
            secondMenuId = 'VCCP08',
            menuId = 'VCCP08',
            statementId = 'VCCP08_#3',
            electionCode = '3',
            cityCode = si_code,
            townCode = gu_code
        )
    )

    ## 1.2. 개표 결과를 추출합니다.
    Sys.setlocale("LC_ALL", "C")

    vote_raw <- content(x=resp, as = 'text') %>%
        read_html() %>%
        html_node(css = 'table') %>%
        html_table(fill = TRUE)

    Sys.setlocale("LC_ALL", "Korean")

    return(vote_raw)
}

# get_vote_data('1100', '1103')
# get_vote_data('2600', '2603')


# 3. 서울시 보궐선거 데이터 ----------------
## 3.1. 코드와 서울 행정구 매칭 -----------
### https://ko.wikipedia.org/wiki/서울특별시의_행정_구역
### * 서울시 코드 : 1100
### * 서울행정구 코드: 1101:1125

resp <- POST(
    url = 'http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml',
    encode = 'form',
    body = list(
        electionId = '0020210407',
        requestURI = '/WEB-INF/jsp/electioninfo/0020210407/vc/vccp08.jsp',
        topMenuId = 'VC',
        secondMenuId = 'VCCP08',
        menuId = 'VCCP08',
        statementId = 'VCCP08_#3',
        electionCode = '3',
        cityCode = '1100',
        townCode = '1103'
    )
)

seoul_html <- resp %>%
    read_html() %>%
    html_nodes(css = 'select#townCode option')

### 구명과 구코드를 Dropdown Box에서 추출한다.
gu_code <- seoul_html %>%
    html_attr(name = 'value')

gu_name <- seoul_html %>%
    html_text()

seoul_cd_tbl <- tibble(구코드 = gu_code,
                          구명   = gu_name) %>%
    filter(구코드 != "-1") %>%
    mutate(시도 = "서울시",
             시도코드 = "1100") %>%
    select(시도코드, 시도, 구코드, 구명)

## 3.2. 코드 득표데이터 긁어오기 -----------
seoul_raw <- map2(seoul_cd_tbl$시도코드, seoul_cd_tbl$구코드, get_vote_data)

seoul_tbl <- seoul_raw %>%
    enframe(value = "data") %>%
    bind_cols(seoul_cd_tbl) %>%
    select(시도코드, 시도, 구코드, 구명, data)

# seoul_tbl

# 4. 부산시 보궐선거 데이터 ----------------
## 4.1. 코드와 부산 행정구 매칭 -----------
### https://ko.wikipedia.org/wiki/부산광역시의_행정_구역
### * 서울시 코드 : 1100
### * 서울행정구 코드: 1101:1116

busan_resp <- POST(
    url = 'http://info.nec.go.kr/electioninfo/electionInfo_report.xhtml',
    encode = 'form',
    body = list(
        electionId = '0020210407',
        requestURI = '/WEB-INF/jsp/electioninfo/0020210407/vc/vccp08.jsp',
        topMenuId = 'VC',
        secondMenuId = 'VCCP08',
        menuId = 'VCCP08',
        statementId = 'VCCP08_#3',
        electionCode = '3',
        cityCode = '2600',
        townCode = '2601'
    )
)

busan_html <- busan_resp %>%
    read_html() %>%
    html_nodes(css = 'select#townCode option')

### 구명과 구코드를 Dropdown Box에서 추출한다.
busan_gu_code <- busan_html %>%
    html_attr(name = 'value')

busan_gu_name <- busan_html %>%
    html_text()

pusan_cd_tbl <- tibble(구코드 = busan_gu_code,
                       구명   = busan_gu_name) %>%
    filter(구코드 != "-1") %>%
    mutate(시도 = "부산시",
           시도코드 = "2600") %>%
    select(시도코드, 시도, 구코드, 구명)

## 4.2. 코드 득표데이터 긁어오기 -----------
pusan_raw <- map2(pusan_cd_tbl$시도코드, pusan_cd_tbl$구코드, get_vote_data)

pusan_tbl <- pusan_raw %>%
    enframe(value = "data") %>%
    bind_cols(pusan_cd_tbl) %>%
    select(시도코드, 시도, 구코드, 구명, data)

# pusan_tbl

# 5. 서울/부산시 결합  ----------------

local_2021_by_election <- bind_rows(seoul_tbl, pusan_tbl)

## 6. 단위테스트 검증 -------------

test_that("지선 2021 보궐 후보득표검증", {

})

usethis::use_data(local_2021_by_election, overwrite = TRUE)

