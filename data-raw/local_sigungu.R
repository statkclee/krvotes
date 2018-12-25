# 2018년 지방선거 개표결과 - 시도지사
#
# 지방선거 구시군의장 엑셀 데이터에서 먼저 선거구별 정당과 후보를 추출해낸다.
# 추출된 "선거구별 정당과 후보" 즉, `local_sigungu_party`를
# 구시군의장 시도, 선거구명에 쭉 뿌린다.
#
# 이를 바탕으로 각 시도 선거구별로 개표데이터를 후처리한다.
#
# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 시도 선거구별 정당 후보자 추출  -----
## 1.1. 데이터 불러오기 -----
# 출처: https://bit.ly/2Q1fEcq

local_sigungu_2018_dat <- read_excel("data-raw/20180619-7지선-02-(구시군의장)_읍면동별개표자료.xlsx", sheet="7회지선-구시군의장", skip=0,
                                     col_types = c(rep("text",19)))

names(local_sigungu_2018_dat) <- enc2native(names(local_sigungu_2018_dat))

## 1.2.후보정당과 후보 추출 -----
var_names <- c("선거종류", "시도", "선거구명", "시도명", "구시군명", "읍면동명", "구분",
               "선거인수", "투표수", paste0("party_", seq(1:7)), "계", "무효투표수", "기권수")

local_sigungu_party <- local_sigungu_2018_dat %>%
  filter(is.na(`시도명`)) %>%
  set_names(var_names) %>%
  mutate(party_1 = ifelse(party_1 == "\r\r\n", "party_1", party_1),
         party_2 = ifelse(party_2 == "\r\r\n", "party_2", party_2),
         party_3 = ifelse(party_3 == "\r\r\n", "party_3", party_3),
         party_4 = ifelse(party_4 == "\r\r\n", "party_4", party_4),
         party_5 = ifelse(party_5 == "\r\r\n", "party_5", party_5),
         party_6 = ifelse(party_6 == "\r\r\n", "party_6", party_6),
         party_7 = ifelse(party_7 == "\r\r\n", "party_7", party_7))  %>%
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
local_sigungu_2018_dat <- read_excel("data-raw/20180619-7지선-02-(구시군의장)_읍면동별개표자료.xlsx", sheet="7회지선-구시군의장", skip=1,
                             col_types = c(rep("text",7), rep("numeric", 12)))

names(local_sigungu_2018_dat) <- enc2native(names(local_sigungu_2018_dat))

## 2.2. 데이터 정제 -----
### 시도 선거구별 리스트칼럼 생성
local_sigungu_2018_df <- local_sigungu_2018_dat %>%
    set_names(var_names) %>%
    select(-`선거종류`, -`계`) %>%
    group_by(`시도`, `선거구명`) %>%
    nest()

## 2.3. 시도 선거구별 득표데이터 칼럼명 설정 -----
### 하나인 경우
local_sigungu_2018_df %>%
  filter(시도 == "서울특별시" & 선거구명 == "종로구") %>%
  pull(data) %>%
  .[[1]] %>%
  set_names(local_sigungu_party[1,-c(1,2)] %>% unlist %>% as.vector)

### 전체로 확장
sigungu_list <- list()

for(i in 1:nrow(local_sigungu_2018_df)) {
  sigungu_list[[i]] <- local_sigungu_2018_df %>%
    filter(row_number() == i) %>%
    pull(data) %>%
    .[[1]] %>%
    set_names(local_sigungu_party[i, -c(1,2)] %>% unlist %>% as.vector %>% str_replace_all(., "\r\r\n", " "))
}

sigungu_df <- sigungu_list %>% enframe %>%
  set_names(c("index", "data_clean"))

local_sigungu_2018 <- bind_cols(local_sigungu_2018_df, sigungu_df) %>%
  select(-index)

## 2.4. 시도 선거구별 득표데이터 정제 -----
### 데이터 후처리 -----

local_sigungu_2018 <- local_sigungu_2018 %>%
    mutate(data_clean = map(data_clean, ~ filter(., !is.na(`시도명`)))) %>%
    mutate(data_clean = map(data_clean, ~ filter(., !is.na(`구시군명`)))) %>%
    mutate(data_clean = map(data_clean, ~ filter(., `읍면동명` !="계"))) %>%
    mutate(data_clean = map(data_clean, ~ mutate(., `구분` = ifelse(is.na(`구분`), `읍면동명`, `구분`)))) %>% # because of NA
    mutate(data_clean = map(data_clean, ~ filter(., `구분` !="소계" ))) %>%
    mutate(data_clean = map(data_clean, ~ select(., -contains("party"))))

# 3. 단위테스트 검증 -------------

test_that("지방선거 구시군의 장선거 2018 후보득표검증", {

  local_sigungu_2018_check <- local_sigungu_2018 %>%
        filter(시도 == "서울특별시" & 선거구명 == "종로구") %>%
        pull(data_clean) %>% .[[1]] %>%
        mutate_at(vars(contains("당")), funs(as.numeric) ) %>%
        summarise(`더불어민주당 김영종` = sum(`더불어민주당 김영종`),
                  `자유한국당 이숙연`   = sum(`자유한국당 이숙연`),
                  `바른미래당 김복동`   = sum(`바른미래당 김복동`))

    expect_that( local_sigungu_2018_check$`더불어민주당 김영종`, equals(51305))
    expect_that( local_sigungu_2018_check$`자유한국당 이숙연`,   equals(19628))
    expect_that( local_sigungu_2018_check$`자유한국당 이숙연`,   equals(19628))
    expect_that( local_sigungu_2018_check$`바른미래당 김복동`,   equals(8765))
})

usethis::use_data(local_sigungu_2018, overwrite = TRUE)

