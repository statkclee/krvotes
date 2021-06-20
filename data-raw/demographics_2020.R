# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)
library(here)
library(rvest)
library(httr)

# 1. 연령별 인구현황 원본데이터 가져오기 -------------
## 출처: https://jumin.mois.go.kr/

## 1.1 POST() 함수를 사용하여 시도별, 연령별, 성별 데이터를 가져온다.
demographics_resp <- POST(
    url = 'https://jumin.mois.go.kr/ageStatMonth.do',
    body = list(
        nowYear         = '2021',
        searchYearMonth = 'year',
        searchYearStart = '2020',
        searchMonthStart= '12',
        searchYearEnd   = '2020',
        searchMonthEnd  = '12',
        sum             = 'sum',
        gender          = 'gender',
        ltOrderType     = '1',
        ltOrderValue    = 'ASC',
        ltArgTypes      = '10',
        ltArgTypeA      = '0',
        ltArgTypeB      = '100'
    )
)


Sys.setlocale("LC_ALL", "C")

demographics_dat <- content(x = demographics_resp, as = 'text') %>%
    read_html() %>%
    html_node(css = '#contextTable') %>%
    html_table(fill = TRUE)

Sys.setlocale("LC_ALL", "Korean")

## 1.2 데이터 전처리 --> Tibble

demographics_colnames <- demographics_dat %>%
    janitor::clean_names(ascii = FALSE) %>%
    slice(2) %>%
    unlist() %>%
    as.character(.) %>%
    dput(.)

demographics_tbl <- demographics_dat %>%
    ## 변수명 정리
    set_names(demographics_colnames) %>%
    janitor::clean_names(ascii = FALSE) %>%
    slice(3:n()) %>%
    ## 연령대와 남녀 구분자 생성
    pivot_longer(cols = `x0_9세`:`x100세_이상_3`)  %>%
    filter(!str_detect(name, "연령구간인구수|남_인구수|여_인구수")) %>%
    mutate(성별 = case_when(str_detect(name, "_2$") ~ "남자",
                            str_detect(name, "_3$") ~ "여자",
                            TRUE ~ "남녀")) %>%
    filter(성별 !="남녀") %>%
    filter(행정기관 != "전국") %>%
    mutate(연령 = str_extract(name, pattern = "[0-9]{1,2}_[0-9]{1,2}세|[0-9]{3}세_이상")) %>%
    select(행정기관코드, 행정기관, 성별, 연령, 인구수 = value) %>%
    mutate(인구수 = parse_number(인구수))


## 3. 단위테스트 검증 -------------

test_that("2020년 12월/2021년 1월 인구통계 검증", {

    demographics_chk <- demographics_tbl %>%
        group_by(행정기관) %>%
        summarise(인구수 = sum(인구수)) %>%
        pivot_wider(names_from = 행정기관, values_from = 인구수)

    expect_that( demographics_chk$`강원도`,         equals(1542840))
    expect_that( demographics_chk$`서울특별시`,     equals(9668465))
    expect_that( demographics_chk$`경기도`,         equals(13427014))
})

usethis::use_data(demographics_tbl, overwrite = TRUE)
