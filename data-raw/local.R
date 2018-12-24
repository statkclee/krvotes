# packages -------------
library(tidyverse)
library(readxl)
library(testthat)

# read raw data -------------
# Party name convention
#
# 민주당: "democracy"
# 자유한국당: "freedom",
# 바른미래당: "barun",
# 정의당: "justice",
# 민중당: "minjung",
# 대한애국당: "korea",
# 녹색당: "green",
# 우리미래: "future",
# 친박연대: "park"

var_names <- c("election_type","precinct","sido","sigungu","emd","type","electorate","vote",
  "democracy","freedom",
  "barun", "justice", "minjung",
  "korea", "green", "future",
  "park", "total", "invalid", "abstention")

var_names_party <- ifelse(str_detect(var_names, "\r\r\n"),
                          str_extract(var_names, ".+(?=\\r\\r\\n)"), # 당명만 추출
                          var_names)

local_2018_dat <- read_excel("data-raw/20180619-7지선-01-(시도지사)_읍면동별개표자료.xlsx", sheet="7지선-시도지사", skip=1,
                             col_types = c(rep("text",6), rep("numeric", 14)))

# local_2018 <- local_2018_dat %>%
#     set_names(var_names_short) %>%
#     select(-`선거종류`, -`계`) %>%
#     filter(`시도명` != "합계") %>%
#     filter(`읍면동명` != "계") %>%
#     mutate(`구분` = case_when(is.na(`구분`) ~ `읍면동명`,
#                               TRUE ~ `구분`)) %>%
#     filter(`구분` != "소계")

local_2018_dat <- local_2018_dat %>%
    set_names(var_names) %>%
    select(-election_type, -total) %>%
    group_by(precinct) %>%
    nest()

local_2018 <- local_2018_dat %>%
    mutate(data_clean = map(data, ~ filter(., !is.na(sigungu)))) %>%
    mutate(data_clean = map(data_clean, ~ filter(., emd !="계"))) %>%
    mutate(data_clean = map(data_clean, ~ mutate(., type = ifelse(is.na(`type`), emd, type)))) %>% # because of NA
    mutate(data_clean = map(data_clean, ~ filter(.,  type !="소계" ))) # %>%
    # mutate(data_clean = map(data_clean, ~ filter(.,  !str_detect(type, "잘못") )))
    # filter(sido_adm == "세종특별자치시") %>%
    # pull(data2)


# data wrangling -------------

# unit test -------------

test_that("지방선거 2018 후보득표검증", {

    local_2018_seoul_check <- local_2018 %>%
        filter(precinct == "서울특별시") %>%
        pull(data_clean) %>% .[[1]] %>%
        summarise(`democracy` = sum(`democracy`),
                  `freedom` = sum(`freedom`),
                  `barun` = sum(`barun`),
                  `justice` = sum(`justice`),
                  `minjung` = sum(`minjung`),
                  `korea` = sum(`korea`),
                  `green` = sum(`green`),
                  `future` = sum(`future`),
                  `park` = sum(`park`))

    expect_that( local_2018_seoul_check$democracy, equals(2619497))
    expect_that( local_2018_seoul_check$freedom,   equals(1158487))
    expect_that( local_2018_seoul_check$barun,     equals(970374))
    expect_that( local_2018_seoul_check$justice,   equals(81664))
    expect_that( local_2018_seoul_check$minjung,   equals(22134))
    expect_that( local_2018_seoul_check$korea,     equals(11222))
    expect_that( local_2018_seoul_check$green,     equals(82874))
    expect_that( local_2018_seoul_check$future,    equals(11599))
    expect_that( local_2018_seoul_check$park,      equals(4021))
})

usethis::use_data(local_2018, overwrite = TRUE)

