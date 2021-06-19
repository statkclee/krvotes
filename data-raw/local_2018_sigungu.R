# 2018년 지방선거 개표결과 - 시도지사
#
# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 시도 선거구별 정당 후보자 추출  -----
## 1.1. 데이터 불러오기 -----
# 출처: https://www.nec.go.kr/site/nec/ex/bbs/View.do?cbIdx=1129&bcIdx=14979

local_2018_sigungu_dat <- read_excel("data-raw/전국동시지방선거 개표결과(제7회)/20180619-7지선-02-(구시군의장)_읍면동별개표자료.xlsx", sheet="7회지선-구시군의장", skip=0,
                                     col_types = c(rep("text",19)))

names(local_2018_sigungu_dat) <- enc2native(names(local_2018_sigungu_dat))

# 2. 시도별로 데이터 전처리 -----
## 2.1. 시도별 데이터 쪼개기 -------
local_2018_sigungu_tbl <- local_2018_sigungu_dat %>%
  dplyr::group_split(시도, 선거구명)

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


local_2018_sigungu_tbl <- map(local_2018_sigungu_tbl, clean_colnames)

local_2018_sigungu <- local_2018_sigungu_tbl %>%
  enframe(value = "data") %>%
  mutate(시도 = map_chr(data, ~ count(., 시도) %>% pull(시도)),
         선거구명 = map_chr(data, ~ count(., 선거구명) %>% pull(선거구명))) %>%
  select(시도, 선거구명, data) %>%
  mutate(clean_data = map(data, clean_variable))

# 3. 단위테스트 검증 -------------

test_that("지방선거 구시군의 장선거 2018 후보득표검증", {

  local_2018_sigungu_check <- local_2018_sigungu %>%
        filter(시도 == "서울특별시" & 선거구명 == "종로구") %>%
        pull(clean_data) %>% .[[1]] %>%
        mutate_at(vars(contains("당")), funs(as.numeric) ) %>%
        summarise(`더불어민주당_김영종` = sum(`더불어민주당_김영종`),
                  `자유한국당_이숙연`   = sum(`자유한국당_이숙연`),
                  `바른미래당_김복동`   = sum(`바른미래당_김복동`))

    expect_that( local_2018_sigungu_check$`더불어민주당_김영종`, equals(51305))
    expect_that( local_2018_sigungu_check$`자유한국당_이숙연`,   equals(19628))
    expect_that( local_2018_sigungu_check$`바른미래당_김복동`,   equals(8765))
})

usethis::use_data(local_2018_sigungu, overwrite = TRUE)

