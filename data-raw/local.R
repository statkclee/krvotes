# 한글 변수명이 팩키지에 반영되지 않음 !!!

# packages -------------
library(tidyverse)
library(readxl)
library(testthat)

# read raw data -------------

var_names <- c("선거종류", "선거구명", "시도명", "시군구명", "읍면동명", "구분", "선거인수", "투표수",
               "더불어민주당\r\r\n박원순", "자유한국당\r\r\n김문수",
               "바른미래당\r\r\n안철수", "정의당\r\r\n김종민", "민중당\r\r\n김진숙",
               "대한애국당\r\r\n인지연", "녹색당\r\r\n신지예", "우리미래\r\r\n우인철",
               "친박연대\r\r\n최태현", "계", "무표투표수", "기권수")

var_names_party <- ifelse(str_detect(var_names, "\r\r\n"),
                          str_extract(var_names, ".+(?=\\r\\r\\n)"), # 당명만 추출
                          var_names)

local_2018_dat <- read_excel("data-raw/20180619-7지선-01-(시도지사)_읍면동별개표자료.xlsx", sheet="7지선-시도지사", skip=1,
                             col_types = c(rep("text",6), rep("numeric", 14)))

local_2018_df <- local_2018_dat %>%
    set_names(var_names_party) %>%
    select(-`선거종류`, -`계`) %>%
    group_by(선거구명) %>%
    nest()

local_2018 <- local_dat %>%
    mutate(data2 = map(data, ~ filter(., !is.na(`구시군명`)))) %>%
    mutate(data2 = map(data2, ~ filter(., `읍면동명` !="계"))) %>%
    mutate(data2 = map(data2, ~ mutate(., `구분` = ifelse(is.na(`구분`), `읍면동명`, `구분`)))) %>% # because of NA
    mutate(data2 = map(data2, ~ filter(.,  `구분` !="소계" ))) %>%
    mutate(data2 = map(data2, ~ filter(.,  str_detect(`구분`, "잘못") )))
    # filter(sido_adm == "세종특별자치시") %>%
    # pull(data2)


# data wrangling -------------

# unit test -------------


usethis::use_data(local_2018, overwrite = TRUE)
