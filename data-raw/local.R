# 2018년 지방선거 개표결과 - 시도지사

# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 데이터 불러오기 -----
# 출처: https://bit.ly/2Q1fEcq
local_2018_dat <- read_excel("data-raw/20180619-7지선-01-(시도지사)_읍면동별개표자료.xlsx", sheet="7지선-시도지사", skip=0,
                             col_types = c(rep("text", 20)))

names(local_2018_dat) <- enc2native(names(local_2018_dat))

## 1.2.후보정당과 후보 추출 -----
var_names <- c("선거종류", "선거구명", "시도명", "구시군명", "읍면동명", "구분",
               "선거인수", "투표수", paste0("party_", seq(1:9)), "계", "무효투표수", "기권수")

local_party <- local_2018_dat %>%
    filter(is.na(`시도명`)) %>%
    set_names(var_names) %>%
    mutate(party_1 = ifelse(party_1 == "\r\r\n", "party_1", party_1),
           party_2 = ifelse(party_2 == "\r\r\n", "party_2", party_2),
           party_3 = ifelse(party_3 == "\r\r\n", "party_3", party_3),
           party_4 = ifelse(party_4 == "\r\r\n", "party_4", party_4),
           party_5 = ifelse(party_5 == "\r\r\n", "party_5", party_5),
           party_6 = ifelse(party_6 == "\r\r\n", "party_6", party_6),
           party_7 = ifelse(party_7 == "\r\r\n", "party_7", party_7),
           party_8 = ifelse(party_8 == "\r\r\n", "party_8", party_8),
           party_9 = ifelse(party_9 == "\r\r\n", "party_9", party_9))  %>%
    mutate(`시도명`    = "시도명",
           `구시군명`  = "구시군명",
           `읍면동명`  = "읍면동명",
           `구분`      = "구분",
           `선거인수`  = "선거인수",
           `투표수`    = "투표수",
           `무효투표수`= "무효투표수",
           `기권수`    = "기권수") %>%
    select(-`선거종류`, -`계`)
# select(-`선거종류`, -`시도명`, -`구시군명`, -`읍면동명`, -`구분`, -`선거인수`, -`투표수`, -`계`, -`무효투표수`, -`기권수`)

# 2. 시도 선거구별 득표데이터  -----
## 2.1. 데이터 불러오기 -----
local_2018_dat <- read_excel("data-raw/20180619-7지선-01-(시도지사)_읍면동별개표자료.xlsx", sheet="7지선-시도지사", skip=0,
                             col_types = c(rep("text",6), rep("numeric", 14)))

names(local_2018_dat) <- enc2native(names(local_2018_dat))

## 2.2. 데이터 정제 -----
### 시도 선거구별 리스트칼럼 생성
local_2018_df <- local_2018_dat %>%
    filter(!is.na(`구시군명`)) %>%
    set_names(var_names) %>%
    select(-`선거종류`, -`계`) %>%
    group_by(`시도명`) %>%
    nest()

## 2.3. 시도 선거구별 득표데이터 칼럼명 설정 -----
### 하나인 경우
local_2018_df %>%
    filter(시도명 == "서울특별시") %>%
    pull(data) %>%
    .[[1]] %>%
    set_names(local_party[1,-c(1)] %>% unlist %>% as.vector)

### 전체로 확장
sido_list <- list()

for(i in 1:nrow(local_2018_df)) {
    sido_list[[i]] <- local_2018_df %>%
        filter(row_number() == i) %>%
        pull(data) %>%
        .[[1]] %>%
        set_names(local_party[i, -c(1)] %>% unlist %>% as.vector %>% str_replace_all(., "\r\r\n", " "))
}

sido_df <- sido_list %>% enframe %>%
    set_names(c("index", "data_clean"))

local_2018_tmp <- bind_cols(local_2018_df, sido_df) %>%
    select(-index)

## 2.4. 시도 선거구별 득표데이터 정제 -----
### 데이터 후처리 -----

local_2018 <- local_2018_tmp %>%
    mutate(data_clean = map(data_clean, ~ filter(., !is.na(`구시군명`)))) %>%
    mutate(data_clean = map(data_clean, ~ filter(., `읍면동명` !="계"))) %>%
    mutate(data_clean = map(data_clean, ~ mutate(., `구분` = ifelse(is.na(`구분`), `읍면동명`, `구분`)))) %>% # because of NA
    mutate(data_clean = map(data_clean, ~ filter(., `구분` !="소계" ))) %>%
    mutate(data_clean = map(data_clean, ~ select(., -contains("party"))))

# 3. 단위테스트 검증 -------------

test_that("지방선거 2018 후보득표검증", {

    # 서울시장 후보 단위테스트 검정
    local_2018_seoul_check <- local_2018 %>%
        filter(`시도명` == "서울특별시") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`더불어민주당 박원순` = sum(`더불어민주당 박원순`),
                  `자유한국당 김문수`   = sum(`자유한국당 김문수`),
                  `바른미래당 안철수`   = sum(`바른미래당 안철수`),
                  `정의당 김종민`       = sum(`정의당 김종민`),
                  `민중당 김진숙`       = sum(`민중당 김진숙`),
                  `대한애국당 인지연`   = sum(`대한애국당 인지연`),
                  `녹색당 신지예`       = sum(`녹색당 신지예`),
                  `우리미래 우인철`     = sum(`우리미래 우인철`),
                  `친박연대 최태현`     = sum(`친박연대 최태현`))

    local_2018_jeju_check <- local_2018 %>%
        filter(`시도명` == "제주특별자치도") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`더불어민주당 문대림` = sum(`더불어민주당 문대림`),
                  `자유한국당 김방훈`   = sum(`자유한국당 김방훈`),
                  `바른미래당 장성철`   = sum(`바른미래당 장성철`),
                  `녹색당 고은영`       = sum(`녹색당 고은영`),
                  `무소속 원희룡`       = sum(`무소속 원희룡`))

    expect_that( local_2018_jeju_check$`더불어민주당 문대림`, equals(137901))
    expect_that( local_2018_jeju_check$`자유한국당 김방훈`,   equals(11241))
    expect_that( local_2018_jeju_check$`바른미래당 장성철`,   equals(5019))
    expect_that( local_2018_jeju_check$`녹색당 고은영`,       equals(12188))
    expect_that( local_2018_jeju_check$`무소속 원희룡`,       equals(178255))
})

usethis::use_data(local_2018, overwrite = TRUE)

