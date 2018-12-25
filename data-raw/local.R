# 2018년 지방선거 개표결과 - 시도지사

# 0. 팩키지 불러오기 -------------
library(tidyverse)
library(readxl)
library(testthat)

# 1. 데이터 불러오기 -----
# 출처: https://bit.ly/2Q1fEcq

var_names <- c("선거종류", "선거구명", "시도명", "구시군명", "읍면동명", "구분",
                 "선거인수", "투표수", "더불어민주당 박원순","자유한국당 김문수",
                 "바른미래당 안철수","정의당 김종민","민중당 김진숙","대한애국당 인지연",
                 "녹색당 신지예","우리미래 우인철","친박연대 최태현", "계", "무효투표수", "기권수")

local_2018_dat <- read_excel("data-raw/20180619-7지선-01-(시도지사)_읍면동별개표자료.xlsx", sheet="7지선-시도지사", skip=0,
                             col_types = c(rep("text",6), rep("numeric", 14)))

names(local_2018_dat) <- enc2native(names(local_2018_dat))

# 2. 데이터 정제 -----

local_2018_df <- local_2018_dat %>%
    set_names(var_names) %>%
    filter(row_number() !=1) %>%
    dplyr::select(-`선거종류`, -`계`) %>%
    group_by(`선거구명`) %>%
    nest()

local_2018 <- local_2018_df %>%
    mutate(data_clean = map(data, ~ filter(., !is.na(`시도명`)))) %>%
    mutate(data_clean = map(data_clean, ~ filter(., `읍면동명` !="계"))) %>%
    mutate(data_clean = map(data_clean, ~ mutate(., `구분` = ifelse(is.na(`구분`), `읍면동명`, `구분`)))) %>% # because of NA
    mutate(data_clean = map(data_clean, ~ filter(.,  `구분` !="소계" )))

# 3. 단위테스트 검증 -------------

test_that("지방선거 2018 후보득표검증", {

    # 서울시장 후보 단위테스트 검정
    local_2018_seoul_check <- local_2018 %>%
        filter(`선거구명` == "서울특별시") %>%
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

    expect_that( local_2018_seoul_check$`더불어민주당 박원순`, equals(2619497))
    expect_that( local_2018_seoul_check$`자유한국당 김문수`,   equals(1158487))
    expect_that( local_2018_seoul_check$`바른미래당 안철수`,   equals(970374))
    expect_that( local_2018_seoul_check$`정의당 김종민`,       equals(81664))
    expect_that( local_2018_seoul_check$`민중당 김진숙`,       equals(22134))
    expect_that( local_2018_seoul_check$`대한애국당 인지연`,   equals(11222))
    expect_that( local_2018_seoul_check$`녹색당 신지예`,       equals(82874))
    expect_that( local_2018_seoul_check$`우리미래 우인철`,     equals(11599))
    expect_that( local_2018_seoul_check$`친박연대 최태현`,     equals(4021))
})

usethis::use_data(local_2018, overwrite = TRUE)

