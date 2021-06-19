# 2018년 지방선거 개표결과 - 시도지사

# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 데이터 불러오기 -----
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14979
local_2018_dat <- read_excel("data-raw/전국동시지방선거 개표결과(제7회)/20180619-7지선-01-(시도지사)_읍면동별개표자료.xlsx", sheet="7지선-시도지사", skip=0,
                             col_types = c(rep("text", 20)))

names(local_2018_dat) <- enc2native(names(local_2018_dat))

# 2. 시도별로 데이터 전처리 -----
## 2.1. 시도별 데이터 쪼개기 -------
local_2018_tbl <- local_2018_dat %>%
    dplyr::group_split(선거구명)

## 2.2. 변수명 깔끔하게 하는 함수 -------
clean_colnames <- function(df) {

    # 티블 변수명 정리를 위한 임시 저장소
    local_colnames_tbl <- df %>%
        janitor::clean_names(ascii = FALSE)

    # 티블 변수명 전반부 (확정)
    local_colnames_first <- local_colnames_tbl %>%
        select(선거종류:투표수) %>%
        colnames() %>%
        dput()

    # 티블 변수명 후반부 (첫번째 행에서 가져옮)
    local_colnames_second <- local_colnames_tbl %>%
        slice(1) %>%
        select(후보자별_득표수:기권수) %>%
        unlist %>% as.vector(.) %>%
        dput()

    local_colnames_second[length(local_colnames_second)-1] <- "무효투표수"
    local_colnames_second[length(local_colnames_second)] <- "기권수"

    # 변수명 결합
    local_colnames_v <- c(local_colnames_first, local_colnames_second)

    clean_tbl <- local_colnames_tbl %>%
        set_names(local_colnames_v) %>%
        janitor::clean_names(ascii = FALSE) %>%
        slice(2:n()) %>%
        select(!starts_with("x"))

    return(clean_tbl)
}

## 2.3. 시도별 변수명 작업 수행 -------
### 변수명 깔끔하게 정리
clean_variable <- function(df) {
    clean_tbl <- df %>%
        select(-선거종류) %>%
        filter(!is.na(구시군명),
               읍면동명 != "계") %>%
        mutate(구분 = ifelse(is.na(구분), 읍면동명, 구분)) %>%
        mutate_at(vars(선거인수:기권수), as.numeric) %>%
        filter(구분 != "소계")

    return(clean_tbl)
}


local_2018_tbl <- map(local_2018_tbl, clean_colnames)

local_2018 <- local_2018_tbl %>%
    enframe(value = "data") %>%
    mutate(시도명 = map_chr(data, ~ count(., 선거구명) %>% pull(선거구명))) %>%
    select(시도명, data) %>%
    mutate(clean_data = map(data, clean_variable))

# 3. 단위테스트 검증 -------------

test_that("지방선거 2018 후보득표검증", {

    # 서울시장 후보 단위테스트 검정
    local_2018_seoul_check <- local_2018 %>%
        filter(`시도명` == "서울특별시") %>%
        pull(clean_data) %>% .[[1]] %>%
        summarise(`더불어민주당_박원순` = sum(`더불어민주당_박원순`),
                  `자유한국당_김문수`   = sum(`자유한국당_김문수`),
                  `바른미래당_안철수`   = sum(`바른미래당_안철수`),
                  `정의당_김종민`       = sum(`정의당_김종민`))

    expect_that( local_2018_seoul_check$`더불어민주당_박원순`, equals(2619497))
    expect_that( local_2018_seoul_check$`자유한국당_김문수`,   equals(1158487))
    expect_that( local_2018_seoul_check$`바른미래당_안철수`,   equals(970374))
    expect_that( local_2018_seoul_check$`정의당_김종민`,       equals(81664))


    local_2018_jeju_check <- local_2018 %>%
        filter(`시도명` == "제주특별자치도") %>%
        pull(clean_data) %>% .[[1]] %>%
        summarise(`더불어민주당_문대림` = sum(`더불어민주당_문대림`),
                  `자유한국당_김방훈`   = sum(`자유한국당_김방훈`),
                  `바른미래당_장성철`   = sum(`바른미래당_장성철`),
                  `녹색당_고은영`       = sum(`녹색당_고은영`),
                  `무소속_원희룡`       = sum(`무소속_원희룡`))

    expect_that( local_2018_jeju_check$`더불어민주당_문대림`, equals(137901))
    expect_that( local_2018_jeju_check$`자유한국당_김방훈`,   equals(11241))
    expect_that( local_2018_jeju_check$`바른미래당_장성철`,   equals(5019))
    expect_that( local_2018_jeju_check$`녹색당_고은영`,       equals(12188))
    expect_that( local_2018_jeju_check$`무소속_원희룡`,       equals(178255))
})

usethis::use_data(local_2018, overwrite = TRUE)

