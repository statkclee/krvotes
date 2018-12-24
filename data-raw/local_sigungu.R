# packages -------------
library(tidyverse)
library(readxl)
library(testthat)

# read raw data -------------
## 후보정당과 후보 추출
var_names <- c("election_type","sido","precinct","sido_name", "sigungu","emd","type","electorate","vote",
               paste0("party_", seq(1:7)), "total", "invalid", "abstention")

local_sigungu_2018_dat <- read_excel("data-raw/20180619-7지선-02-(구시군의장)_읍면동별개표자료.xlsx", sheet="7회지선-구시군의장", skip=0,
                                     col_types = c(rep("text",19)))


local_sigungu_party <- local_sigungu_2018_dat %>%
  filter(is.na(`선거인수`)) %>%
  # select(`시도`, `선거구명`, `후보자별 득표수`, contains("X")) %>%
  set_names(var_names) %>%
  mutate(party_1 = ifelse(party_1 == "\r\r\n", "party_1", party_1),
         party_2 = ifelse(party_2 == "\r\r\n", "party_2", party_2),
         party_3 = ifelse(party_3 == "\r\r\n", "party_3", party_3),
         party_4 = ifelse(party_4 == "\r\r\n", "party_4", party_4),
         party_5 = ifelse(party_5 == "\r\r\n", "party_5", party_5),
         party_6 = ifelse(party_6 == "\r\r\n", "party_6", party_6),
         party_7 = ifelse(party_7 == "\r\r\n", "party_7", party_7))  %>%
  mutate(sido_name = "sido_name",
         sigungu   = "sigungu",
         emd       = "emd",
         type      = "type",
         electorate = "electorate",
         vote       = "vote",
         invalid    = "invalid",
         abstention = "abstention") %>%
  select(-election_type, -sido, -precinct, -total)

## 득표수 데이터
local_sigungu_2018_dat <- read_excel("data-raw/20180619-7지선-02-(구시군의장)_읍면동별개표자료.xlsx", sheet="7회지선-구시군의장", skip=1,
                             col_types = c(rep("text",7), rep("numeric", 12)))

local_sigungu_2018_df <- local_sigungu_2018_dat %>%
    set_names(var_names) %>%
    select(-election_type, -total) %>%
    group_by(sido, precinct) %>%
    nest()


## 데이터 정리 ------
### 하나인 경우
local_sigungu_2018_df %>%
  filter(sido == "서울특별시" & precinct == "종로구") %>%
  pull(data) %>%
  .[[1]] %>%
  set_names(local_sigungu_party[1,] %>% unlist %>% as.vector)

### 전체로 확장

sigungu_list <- list()

for(i in 1:nrow(local_sigungu_2018_df)) {
  sigungu_list[[i]] <- local_sigungu_2018_df %>%
    filter(row_number() == i) %>%
    pull(data) %>%
    .[[1]] %>%
    set_names(local_sigungu_party[i,] %>% unlist %>% as.vector %>% str_replace_all(., "\r\r\n", " "))
}

sigungu_df <- sigungu_list %>% enframe %>%
  set_names(c("index", "data_clean"))

local_sigungu_2018 <- bind_cols(local_sigungu_2018_df, sigungu_df) %>%
  select(-index)

### 데이터 후처리 -----

local_sigungu_2018 %>%
  mutate(vote_data = map(data_clean, ~select(., contains("당"))))

local_sigungu_2018 <- local_sigungu_2018 %>%
    mutate(data_clean = map(data_clean, ~ filter(., !is.na(sido_name)))) %>%
    mutate(data_clean = map(data_clean, ~ filter(., !is.na(sigungu)))) %>%
    mutate(data_clean = map(data_clean, ~ filter(., emd !="계"))) %>%
    mutate(data_clean = map(data_clean, ~ mutate(., type = ifelse(is.na(`type`), emd, type)))) %>% # because of NA
    mutate(data_clean = map(data_clean, ~ filter(.,  type !="소계" )))

# data wrangling -------------

# unit test -------------

test_that("지방선거 구시군의 장선거 2018 후보득표검증", {

  local_sigungu_2018_check <- local_sigungu_2018 %>%
        filter(sido == "서울특별시" & precinct == "종로구") %>%
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

